%% -------------------------------------------------------------------
%%
%% Copyright (c) 2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(ebloom).
-author('Dave Smith <dizzyd@dizzyd.com>').
-export([new/3,
         insert/2,
         contains/2,
         clear/1,
         size/1,
         elements/1,
         effective_fpp/1,
         intersect/2,
         union/2,
         difference/2,
         serialize/1,
         deserialize/1]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec init() -> ok | {error, any()}.
-spec new(integer(), float(), integer()) -> {ok, reference()}.
-spec insert(reference(), binary()) -> ok.
-spec contains(reference(), binary()) -> true | false.
-spec clear(reference()) -> ok.
-spec size(reference()) -> integer().
-spec elements(reference()) -> integer().
-spec effective_fpp(reference()) -> float().
-spec intersect(reference(), reference()) -> ok.
-spec union(reference(), reference()) -> ok.
-spec difference(reference(), reference()) -> ok.
-spec serialize(reference()) -> binary().
-spec deserialize(binary()) -> {ok, reference()}.

init() ->
    SoName = case code:priv_dir(ebloom) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    filename:join([filename:dirname(Filename),"../priv", "ebloom_nifs"]);
                _ ->
                    filename:join("../priv", "ebloom_nifs")
            end;
        Dir ->
            filename:join(Dir, "ebloom_nifs")
    end,
    erlang:load_nif(SoName, 0).

new(_Count, _FalseProb, _Seed) ->
    erlang:nif_error({error, not_loaded}).

insert(_Ref, _Bin) ->
    erlang:nif_error({error, not_loaded}).

contains(_Ref, _Bin) ->
    erlang:nif_error({error, not_loaded}).

clear(_Ref) ->
    erlang:nif_error({error, not_loaded}).

size(_Ref) ->
    erlang:nif_error({error, not_loaded}).

elements(_Ref) ->
    erlang:nif_error({error, not_loaded}).

effective_fpp(_Ref) ->
    erlang:nif_error({error, not_loaded}).

intersect(_Ref, _OtherRef) ->
    erlang:nif_error({error, not_loaded}).

union(_Ref, _OtherRef) ->
    erlang:nif_error({error, not_loaded}).

difference(_Ref, _OtherRef) ->
    erlang:nif_error({error, not_loaded}).

serialize(_Ref) ->
    erlang:nif_error({error, not_loaded}).

deserialize(_Bin) ->
    erlang:nif_error({error, not_loaded}).

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new(5, 0.01, 123),
    0 = elements(Ref),
    insert(Ref, <<"abcdef">>),
    true = contains(Ref, <<"abcdef">>),
    false = contains(Ref, <<"zzzzzz">>).

union_test() ->
    {ok, Ref} = new(5, 0.01, 123),
    {ok, Ref2} = new(5, 0.01, 123),
    insert(Ref, <<"abcdef">>),
    false = contains(Ref2, <<"abcdef">>),
    union(Ref2, Ref),
    true = contains(Ref2, <<"abcdef">>).

serialize_test() ->
    {ok, Ref} = new(5, 0.01, 123),
    {ok, Ref2} = new(5, 0.01, 123),
    Bin = serialize(Ref),
    Bin2 = serialize(Ref2),
    true = (Bin =:= Bin2),
    insert(Ref, <<"abcdef">>),
    Bin3 = serialize(Ref),
    {ok, Ref3} = deserialize(Bin3),
    true = contains(Ref3, <<"abcdef">>),
    false = contains(Ref3, <<"rstuvw">>).

clear_test() ->
    {ok, Ref} = new(5, 0.01, 123),
    0 = elements(Ref),
    insert(Ref, <<"1">>),
    insert(Ref, <<"2">>),
    insert(Ref, <<"3">>),
    3 = elements(Ref),
    clear(Ref),
    0 = elements(Ref),
    false = contains(Ref, <<"1">>).

-endif.
