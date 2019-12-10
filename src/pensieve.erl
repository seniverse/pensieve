-module(pensieve).

-compile({no_auto_import,[get/1]}).

-export(
   [
    latest/0,
    get/1,
    put/2,
    append/1,
    wait/1,
    wait/2
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


find(N) when is_integer(N) ->
    find(N, latest()).

find(N, {N, _}=Id) ->
    {ok, Id};
find(N, {M, _}) when M < N->
    {error, not_found};
find(N, {M, _} = Id) ->
    [Index|_] = get(Id),
    find(N, M-1, 1, Index).

find(N, M, Level, [[]|T]) ->
    find(N, M, Level * 2, T);
find(N, M, Level, [H|T]) when N >= M + 1 - Level ->
    find(N, {M, H});
find(N, M, Level, [_|T]) ->
    find(N, M-Level, Level * 2, T).

wait(N) ->
    wait(N, infinity).

wait(N, Timeout) ->
    case latest() of
        {N, _} = Id ->
            {ok, Id};
        {M, _} = Id when M >= N ->
            find(N, Id);
        _ ->
            Ref = make_ref(),
            pensieve_event:add_handler(Ref, N),
            receive
                {Ref, {N, _} = Id} ->
                    {ok, Id};
                {Ref, Id} ->
                    find(N, Id)
            after Timeout ->
                    {error, timeout}
            end
    end.
