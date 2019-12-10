-module(pensieve_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, pensieve).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),

    Delay = application:get_env(pensieve, server_store_delay, 200),

    Id = pensieve:latest(),
    Chunk = pensieve:get(Id),
    gen_event:notify(pensieve_event, Id),
    Index = pensieve_chunk:next_index(Id, Chunk),

    {ok,
     #{index => Index,
       queue => queue:new(),
       waitings => [],
       delay => Delay}
    }.

handle_call({append, Data}, From, State = #{queue := Queue}) ->
    {noreply, start_timer(State#{queue := queue:in({From, Data}, Queue)})};
handle_call({put, Id, Chunk}, _From, State = #{queue := Queue, waitings := Waitings}) ->
    State1 = (maps:remove(worker, State))#{index := pensieve_chunk:next_index(Id, Chunk), waitings := []},
    [gen_server:reply(From, ok) || {From, _} <- Waitings],
    gen_event:notify(pensieve_event, Id),
    {reply,
     ok,
     case State of
         #{timer := timeout} ->
             start_worker(State1);
         _ ->
             State1
     end
    };
handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({timeout, Timer, ok}, State = #{timer := Timer}) ->
    {noreply, start_worker(State)};
handle_info({'EXIT', Pid, Reason}, State = #{worker := Pid}) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.


start_timer(State = #{timer := _}) ->
    State;
start_timer(State = #{delay := Delay}) ->
    State#{timer => erlang:start_timer(Delay, self(), ok)}.

start_worker(State = #{worker := _}) ->
    State#{timer => timeout};
start_worker(State = #{waitings := Waitings, queue := Queue, index := Index}) ->
    Waitings1 = Waitings ++ queue:to_list(Queue),
    Data = [Item || {_, D} <- Waitings1, Item <- D],
    Self = self(),

    Pid =
        spawn_link(
          fun() ->
                  {Id, Chunk} = pensieve_chunk:encode([Index|Data]),
                  pensieve:put(Id, Chunk),
                  ok = gen_server:call(Self, {put, Id, [Index|Data]})
          end),

    (maps:remove(timer, State))
        #{worker => Pid,
          waitings := Waitings1,
          queue := queue:new()}.
