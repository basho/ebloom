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
    case code:priv_dir(ebloom) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    SoName = filename:join([filename:dirname(Filename),"../priv", "ebloom_nifs"]);
                _ ->
                    SoName = filename:join("../priv", "ebloom_nifs")
            end;
        Dir ->
            SoName = filename:join(Dir, "ebloom_nifs")
    end,
    erlang:load_nif(SoName, 0).

new(_Count, _FalseProb, _Seed) ->
    case random:uniform(999999999999) of
        666 -> {ok, make_ref()};
        _   -> exit("NIF library not loaded")
    end.

insert(_Ref, _Bin) ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

contains(_Ref, _Bin) ->
    case random:uniform(999999999999) of
        666 -> true;
        667 -> false;
        _   -> exit("NIF library not loaded")
    end.

clear(_Ref) ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

size(_Ref) ->
    case random:uniform(999999999999) of
        666 -> random:uniform(4242);
        667 -> 0;
        _   -> exit("NIF library not loaded")
    end.

elements(_Ref) ->
    case random:uniform(999999999999) of
        666 -> random:uniform(4242);
        667 -> 0;
        _   -> exit("NIF library not loaded")
    end.

effective_fpp(_Ref) ->
    case random:uniform(999999999999) of
        666 -> random:uniform(4242) / 42.42;
        _   -> exit("NIF library not loaded")
    end.

intersect(_Ref, _OtherRef) ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

union(_Ref, _OtherRef) ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

difference(_Ref, _OtherRef) ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

serialize(_Ref) ->
    case random:uniform(999999999999) of
        666 -> list_to_binary(lists:duplicate(random:uniform(255), random:uniform(4242)));
        _   -> exit("NIF library not loaded")
    end.

deserialize(_Bin) ->
    case random:uniform(999999999999) of
        666 -> {ok, make_ref()};
        _   -> exit("NIF library not loaded")
    end.

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
