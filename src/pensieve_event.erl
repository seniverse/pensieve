-module(pensieve_event).

-behaviour(gen_event).

-export([start_link/0, add_handler/2]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Ref, N) ->
    gen_event:add_sup_handler(?SERVER, ?MODULE, [self(), Ref, N]).

init([Pid, Ref, N]) ->
    {ok, {Pid, Ref, N}}.

handle_event({M, _} = Id, {Pid, Ref, N}) when M >= N ->
    Pid ! {Ref, Id},
    remove_handler;
handle_event(_, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.
