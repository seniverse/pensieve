-module(pensieve_chunk).

-export(
   [decode/2,
    encode/1,
    next_indices/2]).

decode({Seq,Digest}, Bin) ->
    Digest = crypto:hash(sha512, Bin),
    Data = binary_to_term(Bin, [safe]),
    Seq = seq(Data),
    Data.

seq([Indices|_]) ->
    seq(1, Indices) + 1.

seq(_, []) ->
    0;
seq(Level, [[]|T]) ->
    seq(Level bsl 1, T);
seq(Level, [_|T]) ->
    Level + seq(Level bsl 1, T).

make_indices(Digest, []) ->
    [Digest];
make_indices(Digest, [[]|T]) ->
    [Digest|T];
make_indices(Digest, [_|T]) ->
    [[]|make_indices(Digest, T)].

next_indices({_, Digest}, [Indices|_]) ->
    make_indices(Digest, Indices).

encode(Chunk) ->
    Encoded = term_to_binary(Chunk),
    Digest = crypto:hash(sha512, Encoded),
    {{seq(Chunk), Digest}, Encoded}.
