-module(pensieve_dummy_meta_store).

-behaviour(gen_server).

-export([childspecs/0, load/0, store/1]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

childspecs() ->
    [
     #{id => ?MODULE,
       start => {?MODULE, start_link, []},
       restart => permanent,
       shutdown => 5000,
       type => worker,
       modules => []}
    ].

load() ->
    gen_server:call(?SERVER, load).

store(Id) ->
    ok = gen_server:call(?SERVER, {store, Id}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, {0, []}}.

handle_call(load, _From, State) ->
    {reply, State, State};
handle_call({store, {Seq, _} = Id}, _From, {Seq0, _}) when is_integer(Seq), Seq > Seq0 ->
    {reply, ok, Id};
handle_call({store, _}, _From, State) ->
    {reply, {error, State}, State};
handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.
