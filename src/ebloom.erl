-module(ebloom).

-export([new/3,
         insert/2,
         contains/2,
         clear/1,
         size/1,
         elements/1,
         effective_fpp/1,
         intersect/2,
         union/2,
         difference/2]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    case code:priv_dir(ebloom) of
        {error, bad_name} ->
            SoName = filename:join("../priv", ebloom_nifs);
        Dir ->
            SoName = filename:join(Dir, ebloom_nifs)
    end,
    erlang:load_nif(SoName, 0).

new(_Count, _FalseProb, _Seed) ->
    "NIF library not loaded".

insert(_Ref, _Bin) ->
    "NIF library not loaded".

contains(_Ref, _Bin) ->
    "NIF library not loaded".

clear(_Ref) ->
    "NIF library not loaded".

size(_Ref) ->
    "NIF library not loaded".

elements(_Ref) ->
    "NIF library not loaded".

effective_fpp(_Ref) ->
    "NIF library not loaded".

intersect(_Ref, _OtherRef) ->
    "NIF library not loaded".

union(_Ref, _OtherRef) ->
    "NIF library not loaded".

difference(_Ref, _OtherRef) ->
    "NIF library not loaded".


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

-endif.
