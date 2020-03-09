-module(shared_uUID@foreign).

-export([ eqUUIDImpl/2
        , compareUUIDImpl/2
        , stringToUUIDImpl/1
        , uuidToStringImpl/1
        , emptyImpl/0
        ]).

eqUUIDImpl(Lhs, Rhs) when is_binary(Lhs),
                          byte_size(Lhs) == 16 ->
  Lhs =:= Rhs.

compareUUIDImpl(Lhs, Rhs) when is_binary(Lhs),
                               byte_size(Lhs) == 16,
                               is_binary(Rhs),
                               byte_size(Rhs) == 16
                               ->
  if
    Lhs < Rhs -> 'lT';
    Lhs =:= Rhs -> 'eQ';
    true -> 'gT'
  end.

stringToUUIDImpl(<<A:8/binary, "-",
                   B:4/binary, "-",
                   C:4/binary, "-",
                   D:4/binary, "-",
                   E:12/binary>>) ->
  {just, <<(erlang:binary_to_integer(A, 16)):32/big-integer,
           (erlang:binary_to_integer(B, 16)):16/big-integer,
           (erlang:binary_to_integer(C, 16)):16/big-integer,
           (erlang:binary_to_integer(D, 16)):16/big-integer,
           (erlang:binary_to_integer(E, 16)):48/big-integer>>};

stringToUUIDImpl(_) ->
  {nothing}.

uuidToStringImpl(<<A:32/big-integer,
                   B:16/big-integer,
                   C:16/big-integer,
                   D:16/big-integer,
                   E:48/big-integer>>) ->
  <<(pad(8, (erlang:integer_to_binary(A, 16)))):8/binary, "-",
    (pad(4, (erlang:integer_to_binary(B, 16)))):4/binary, "-",
    (pad(4, (erlang:integer_to_binary(C, 16)))):4/binary, "-",
    (pad(4, (erlang:integer_to_binary(D, 16)))):4/binary, "-",
    (pad(12, (erlang:integer_to_binary(E, 16)))):12/binary>>.

emptyImpl() ->
  <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>.

pad(N, Bin) when byte_size(Bin) < N ->
  Pad = N - byte_size(Bin),
  <<(binary:copy(<<"0">>, Pad))/binary, Bin/binary>>.
