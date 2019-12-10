-module(pensieve_chunk).

-export(
   [decode/2,
    encode/1,
    next_index/2]).

decode({Seq,Digest}, Bin) ->
    Digest = crypto:hash(sha512, Bin),
    Data = binary_to_term(Bin, [safe]),
    Seq = seq(Data),
    Data.

seq([Index|_]) ->
    seq(1, Index) + 1.

seq(_, []) ->
    0;
seq(Level, [[]|T]) ->
    seq(Level bsl 1, T);
seq(Level, [_|T]) ->
    Level + seq(Level bsl 1, T).

make_index(Digest, []) ->
    [Digest];
make_index(Digest, [[]|T]) ->
    [Digest|T];
make_index(Digest, [_|T]) ->
    [[]|make_index(Digest, T)].

next_index({_, Digest}, [Index|_]) ->
    make_index(Digest, Index).

encode(Chunk) ->
    Encoded = term_to_binary(Chunk),
    Digest = crypto:hash(sha512, Encoded),
    {{seq(Chunk), Digest}, Encoded}.
