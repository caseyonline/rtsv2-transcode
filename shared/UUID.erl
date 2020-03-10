-module(shared_uUID@foreign).

-export([ eqUUIDImpl/2
        , compareUUIDImpl/5
        , stringToUUIDImpl/3
        , uuidToStringImpl/1
        , emptyImpl/0
        ]).

eqUUIDImpl(Lhs, Rhs) when is_binary(Lhs),
                          byte_size(Lhs) == 16 ->
  Lhs =:= Rhs.

compareUUIDImpl(Lt, Eq, Gt, Lhs, Rhs) when is_binary(Lhs),
                                           byte_size(Lhs) == 16,
                                           is_binary(Rhs),
                                           byte_size(Rhs) == 16
                                           ->
  if
    Lhs < Rhs -> Lt;
    Lhs =:= Rhs -> Eq;
    true -> Gt
  end.

stringToUUIDImpl(Nothing, Just, String) ->
  try
    Just(rtsv2_types:string_to_uuid(String))
  catch
    error:badarg ->
      Nothing
  end.

uuidToStringImpl(Uuid) ->
  rtsv2_types:uuid_to_string(Uuid).

emptyImpl() ->
  <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>.
