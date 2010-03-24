-module(ebloom).

-export([new_filter/3,
         insert/2,
         contains/2]).

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

new_filter(_Count, _FalseProb, _Seed) ->
    "NIF library not loaded".

insert(_Ref, _Bin) ->
    "NIF library not loaded".

contains(_Ref, _Bin) ->
    "NIF library not loaded".


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new_filter(5, 0.99, 123),
    insert(Ref, <<"abcdef">>),
    true = contains(Ref, <<"abcdef">>),
    false = contains(Ref, <<"zzzzzz">>).

-endif.
