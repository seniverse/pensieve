-module(pensieve_app).

-behaviour(application).

-export([start/2, start_phase/3, stop/1, prep_stop/1,
         config_change/3]).

start(_StartType, _StartArgs) ->
    pensieve_sup:start_link().

start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

stop(_State) ->
    ok.

prep_stop(State) ->
    State.

config_change(_Changed, _New, _Removed) ->
    ok.
