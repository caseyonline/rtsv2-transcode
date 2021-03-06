-module(endpoint_helpers).

-export([ dataobject_to_ts/1
        , dataobject_operation_to_purs/1
        , dataobject_response_to_ts/1
        , dataobject_message_to_purs/3
        , client_address/1
        , make_ws_url/3
        ]).

dataobject_response_to_ts(Response) ->
  case Response of
    {ok} -> <<"ok">>;
    {error, {invalidKey, _}} -> <<"invalidKey">>;
    {error, {invalidValue, _}} -> <<"invalidValue">>;
    {error, {invalidOperation, _}} -> <<"invalidOperation">>;
    {error, {compareAndSwapFailed, _}} -> <<"compareAndSwapFailed">>;
    {error, {pendingInitialisation}} -> <<"pendingInitialisation">>;
    {error, {pendingSynchronisation}} -> <<"pendingSynchronisation">>;
    {error, {networkError}} -> <<"networkError">>;
    {error, {unexpected}} -> <<"unexpected">>
  end.

dataobject_to_ts(#{map := Map,
                   version := Version}) ->
  #{ <<"version">> => Version
   , <<"map">> => maps:map(fun(_Key, Value) -> dataobject_value_to_ts(Value) end, Map)
   }.

dataobject_message_to_purs(Id, Message, Destination) ->

  PursMessageDestination = case Destination of
                             #{ <<"tag">> := <<"publisher">> } ->
                               {publisher};
                             #{ <<"tag">> := <<"broadcast">> } ->
                               {broadcast};
                             #{ <<"tag">> := <<"private">>, <<"to">> :=  ToList } when is_list(ToList) ->
                               true = lists:all(fun is_binary/1, ToList),
                               {private, ToList}
                           end,

  #{ sender => Id
   , destination => PursMessageDestination
   , msg => Message
   , ref => make_ref()
   }.

dataobject_operation_to_purs(Operation) ->
  case Operation of
    #{ <<"tag">> := <<"inc">>
     , <<"keys">> := Keys
     , <<"increment">> := Inc
     , <<"createIfKeyMissing">> := CreateIfKeyMissing } ->

      {inc, #{ keys => Keys
             , increment => Inc
             , createIfKeyMissing => CreateIfKeyMissing
             }};

    #{ <<"tag">> := <<"dec">>
     , <<"keys">> := Keys
     , <<"decrement">> := Dec
     , <<"createIfKeyMissing">> := CreateIfKeyMissing } ->

      {dec, #{ keys => Keys
             , decrement => Dec
             , createIfKeyMissing => CreateIfKeyMissing
             }};

    #{ <<"tag">> := <<"cas">>
     , <<"keys">> := Keys
     , <<"compare">> := Compare
     , <<"swap">> := Swap
     , <<"createIfKeyMissing">> := CreateIfKeyMissing } ->

      {compareAndSwap, #{ keys => Keys
                        , compare => dataobject_value_to_purs(Compare)
                        , swap => dataobject_value_to_purs(Swap)
                        , createIfKeyMissing => CreateIfKeyMissing
                        }};

    #{ <<"tag">> := <<"add">>
     , <<"keys">> := Keys
     , <<"value">> := Value
     , <<"failIfKeyPresent">> := FailIfKeyPresent } ->

      {add, #{ keys => Keys
             , value => dataobject_value_to_purs(Value)
             , failIfKeyPresent => FailIfKeyPresent
             }};

    #{ <<"tag">> := <<"update">>
     , <<"keys">> := Keys
     , <<"value">> := Value
     , <<"createIfKeyMissing">> := CreateIfKeyMissing } ->

      {update, #{ keys => Keys
                , value => dataobject_value_to_purs(Value)
                , createIfKeyMissing => CreateIfKeyMissing
                }};

    #{ <<"tag">> := <<"delete">>
     , <<"keys">> := Keys
     , <<"failIfKeyMissing">> := FailIfKeyMissing } ->

      {delete, #{ keys => Keys
                , failIfKeyMissing => FailIfKeyMissing
                }};

    #{ <<"tag">> := <<"list.insert">>
     , <<"keys">> := Keys
     , <<"value">> := Value
     , <<"createIfKeyMissing">> := CreateIfKeyMissing
     , <<"failIfValuePresent">> := FailIfValuePresent } ->

      {listInsert, #{ keys => Keys
                    , value => dataobject_value_to_purs(Value)
                    , createIfKeyMissing => CreateIfKeyMissing
                    , failIfValuePresent => FailIfValuePresent
                    }};

    #{ <<"tag">> := <<"list.remove">>
     , <<"keys">> := Keys
     , <<"value">> := Value
     , <<"failIfKeyMissing">> := FailIfKeyMissing
     , <<"failIfValueMissing">> := FailIfValuePresent } ->

      {listRemove, #{ keys => Keys
                    , value => dataobject_value_to_purs(Value)
                    , failIfKeyMissing => FailIfKeyMissing
                    , failIfValueMissing => FailIfValuePresent
                    }}
  end.

dataobject_value_to_ts({bool, Boolean}) ->
  Boolean;
dataobject_value_to_ts({number, Number}) ->
  Number;
dataobject_value_to_ts({counter, Number}) ->
  Number;
dataobject_value_to_ts({string, String}) ->
  String;
dataobject_value_to_ts({list, List}) ->
  [dataobject_value_to_ts(Item) || Item <- List];
dataobject_value_to_ts({map, Map}) ->
  maps:map(fun(_Key, Value) ->
               dataobject_value_to_ts(Value)
           end,
           Map).

dataobject_value_to_purs(true) ->
  {bool, true};
dataobject_value_to_purs(false) ->
  {bool, false};
dataobject_value_to_purs(Number) when is_number(Number) ->
  {number, Number * 1.0};
dataobject_value_to_purs(String) when is_binary(String) ->
  {string, String};
dataobject_value_to_purs(List) when is_list(List) ->
  {list, [dataobject_value_to_purs(Item) || Item <- List]};
dataobject_value_to_purs(Map) when is_map(Map) ->
  {map, maps:map(fun(Key, Value) when is_binary(Key) ->
                     dataobject_value_to_purs(Value)
                 end,
                 Map)}.

%% Get the peer or (if present) proxied client IP and (original) destination port
client_address(Req) ->
  case cowboy_req:header(<<"x-forwarded-for">>, Req) of
    undefined ->
      { ClientAddr, _ClientPort } = cowboy_req:peer(Req),
      { list_to_binary(inet:ntoa(ClientAddr))
      , cowboy_req:port(Req)};

    RHost ->
      {RHost
      , cowboy_req:header(<<"x-forwarded-port">>, Req, 0)}
  end.

make_ws_url(Host, Port, Path) ->
  case os:getenv("IS_PROXIED") of
    "true" -> <<"wss://", Host/binary, Path/binary>>;
    "false" -> <<"ws://", Host/binary, ":", (integer_to_binary(Port))/binary, Path/binary>>
  end.
