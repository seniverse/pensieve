-module(pensieve_ets_chunk_store).

-export([childspecs/0, load/1, store/2]).

childspecs() ->
    ets:new(?MODULE, [set, public, named_table, {read_concurrency, true}]),
    [].

load(Id) ->
    [{Id, Chunk}] = ets:lookup(?MODULE, Id),
    Chunk.

store(Id, Chunk) ->
    ets:insert(?MODULE, {Id, Chunk}).
