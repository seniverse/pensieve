-module(pensieve).

-export(
   [
    latest/0,
    get/1,
    put/2,
    append/1
   ]).


latest() ->
    MetaStore = persistent_term:get({?MODULE, meta_store}),
    MetaStore:load().

get({0, []}) ->
    [[]];
get(Id) ->
    ChunkStore = persistent_term:get({?MODULE, chunk_store}),
    Chunk = pensieve_chunk:decode(Id, ChunkStore:load(Id)),
    Chunk.

put(Id, Chunk) ->
    ChunkStore = persistent_term:get({?MODULE, chunk_store}),
    ChunkStore:store(Id, Chunk),
    MetaStore = persistent_term:get({?MODULE, meta_store}),
    MetaStore:store(Id),
    ok.

append(Data) ->
    gen_server:call(?MODULE, {append, Data}).
