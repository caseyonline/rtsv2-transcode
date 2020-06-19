-module(rtsv2_agents_intraPoP@foreign).

-export([ to_wire_message/1
        , from_wire_message/2
        , x/0
        , to_binary/1
        , from_binary/1
        ]).

%%------------------------------------------------------------------------------
%% The list of message names - a, b, c...
%% If a message changes add a NEW name and make sure that the
%% old message can still be parsed / provide defaults for the new data
%% before eventually retiring messages that are no longer in the wild
%%------------------------------------------------------------------------------
-define(iMAggregatorAvailableMsgName, <<"a">>).
-define(iMAggregatorStoppedMsgName,   <<"b">>).
-define(iMEgestStateMsgName,          <<"c">>).
-define(iMRelayStateMsgName,          <<"d">>).
-define(iMServerLoadMsgName,          <<"e">>).
-define(iMTransPoPLeaderMsgName,      <<"f">>).
-define(iMVMLivenessMsgName,          <<"g">>).
-define(iMSlotLookupMsgName,          <<"h">>).
%%------------------------------------------------------------------------------
%% End of list of message names
%%------------------------------------------------------------------------------
to_binary(X) ->
  term_to_binary(X).

from_binary(X) ->
  binary_to_term(X).

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------
to_wire_message(
  { iMAggregatorState
  , {available}
  , {agentKey, SlotId, SlotRole}
  , ServerAddress
  , _SlotCharacteristics = #{ numProfiles := NumProfiles
                            , totalBitrate := TotalKBitrate
                            }
  })
  when
    NumProfiles              > 0 andalso NumProfiles =< 16#20,              %% Max of 32 profiles
    TotalKBitrate            > 0 andalso TotalKBitrate =< 16#40000          %% Max of 250 Mbps total in a single ABR ladder
    ->
  { ?iMAggregatorAvailableMsgName
  , << (to_wire_element_slot_role(SlotRole)):1                          %%   1    1
     , (NumProfiles - 1):5                                              %%   5    6
     , (TotalKBitrate - 1):18/unsigned-big-integer                      %%  18   24
     , SlotId:16/binary                                                 %% 128  152
     , ServerAddress/binary                                             %% rest of msg
    >>
  };

to_wire_message(
  { iMAggregatorState
  , {stopped}
  , {agentKey, SlotId, SlotRole}
  , ServerAddress
  , _SlotCharacteristics = #{ numProfiles := _NumProfiles
                            , totalBitrate := _TotalKBitrate
                            }
  }) ->
  { ?iMAggregatorStoppedMsgName
  , << (to_wire_element_slot_role(SlotRole)):1                          %%   1    1
     , 0:7                                                              %%   7    8
     , SlotId:16/binary                                                 %% 128  152
     , ServerAddress/binary                                             %% rest of msg
    >>
  };

to_wire_message(
  { EgestOrRelay
  , Type
  , {agentKey, SlotId, SlotRole}
  , ServerAddress
  })
  when
    EgestOrRelay =:= iMEgestState orelse EgestOrRelay =:= iMRelayState
    ->
  MsgName = case EgestOrRelay of
              iMEgestState -> ?iMEgestStateMsgName;
              iMRelayState -> ?iMRelayStateMsgName
            end,
  { MsgName
  , << (to_wire_element_event_type(Type)):1             %%   1    1
     , (to_wire_element_slot_role(SlotRole)):1          %%   1    2
     , 0:6                                              %%   6    8
     , SlotId:16/binary                                 %% 128  136
     , ServerAddress/binary                             %% rest of msg
    >>
  };

to_wire_message(
  { iMServerLoad
  , ServerAddress
  , _CurrentLoad = #{ cpu := Cpu
                    , network := NetworkKbps
                    }
  , AcceptingRequests
  }) ->
  { ?iMServerLoadMsgName
  , << (to_wire_element_accepting_requests(AcceptingRequests)):1           %%   1    1
     , (to_wire_element_cpu(Cpu)):7/unsigned-big-integer                   %%   7    8
     , (to_wire_element_network_kbps(NetworkKbps)):16/unsigned-big-integer %%  16    24
     , ServerAddress/binary                                                %% rest of msg
    >>
  }.

from_wire_message(
  ?iMAggregatorAvailableMsgName
 , << WireSlotRole:1                                    %%   1     1
    , WireNumProfiles:5                                 %%   5     6
    , WireKBitrate:18/unsigned-big-integer              %%  18    24
    , SlotId:16/binary                                  %% 128   160
    , ServerAddress/binary                              %% rest of msg
   >>
 ) ->
  { just, { iMAggregatorState
          , {available}
          , {agentKey, SlotId, from_wire_element_slot_role(WireSlotRole)}
          , ServerAddress
          , #{ numProfiles => WireNumProfiles + 1
             , totalBitrate => WireKBitrate + 1
             }
          }
  };

from_wire_message(
  ?iMAggregatorStoppedMsgName
 , << WireSlotRole:1                                    %%   1     1
    , 0:7                                               %%   7     8
    , SlotId:16/binary                                  %% 128   136
    , ServerAddress/binary                              %% rest of msg
   >>
 ) ->
  { just, { iMAggregatorState
          , {stopped}
          , {agentKey, SlotId, from_wire_element_slot_role(WireSlotRole)}
          , ServerAddress
          , #{ numProfiles => 0
             , totalBitrate => 0
             }
          }
  };

from_wire_message(
  EgestOrRelayMsgName
 , << WireType:1                                        %%   1     1
    , WireSlotRole:1                                    %%   1     2
    , 0:6                                               %%   6     8
    , SlotId:16/binary                                  %% 128  136
    , ServerAddress/binary                              %% rest of msg
   >>
 )
  when
    EgestOrRelayMsgName =:= ?iMEgestStateMsgName orelse EgestOrRelayMsgName =:= ?iMRelayStateMsgName
    ->

  TypeAtom = case EgestOrRelayMsgName of
               ?iMEgestStateMsgName -> iMEgestState;
               ?iMRelayStateMsgName -> iMRelayState
            end,
  { just, { TypeAtom
          , from_wire_element_event_type(WireType)
          , {agentKey, SlotId, from_wire_element_slot_role(WireSlotRole)}
          , ServerAddress
          }
  };

from_wire_message(
  ?iMServerLoadMsgName
 , << WireAcceptingRequests:1               %%   1    1
    , WireCpu:7/unsigned-big-integer        %%   7    8
    , WireNetwork:16/unsigned-big-integer   %%  16    24
    , ServerAddress/binary                  %% rest of msg
   >>
 ) ->
  { just, { iMServerLoad
          , ServerAddress
          , _CurrentLoad = #{ cpu => from_wire_element_cpu(WireCpu)
                            , network => from_wire_element_network_kbps(WireNetwork)
                    }
          , from_wire_element_accepting_requests(WireAcceptingRequests)
          }
  }.




%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
to_wire_element_event_type({available}) -> 0;
to_wire_element_event_type({stopped})   -> 1.
from_wire_element_event_type(0) -> {available};
from_wire_element_event_type(1) -> {stopped}.

to_wire_element_slot_role({primary}) -> 0;
to_wire_element_slot_role({backup})  -> 1.
from_wire_element_slot_role(0) -> {primary};
from_wire_element_slot_role(1) -> {backup}.

to_wire_element_accepting_requests(true)  -> 1;
to_wire_element_accepting_requests(false) -> 0.
from_wire_element_accepting_requests(1)  -> true;
from_wire_element_accepting_requests(0) -> false.



-define(wireCpukLoadFactor, 1.27). %% 7 bits max = 127 - / 100
to_wire_element_cpu(Load) -> trunc(max(0, min(100, Load) * ?wireCpukLoadFactor)).
from_wire_element_cpu(Load) -> Load / ?wireCpukLoadFactor.


-define(wireNetworkLoadFactor, 2000).
to_wire_element_network_kbps(Kbps) ->
  min(16#ffff, Kbps div ?wireNetworkLoadFactor).
from_wire_element_network_kbps(Wire) -> Wire * ?wireNetworkLoadFactor.


%%------------------------------------------------------------------------------
%% Tests - should move to the Purerl so that changes to the
%% types causes breaks
%%------------------------------------------------------------------------------
x() ->
  lists:foreach(fun encode_decode/1,
                [ test_IMAggregatorAvailable_message()
                , test_IMAggregatorStopped_message()
                , test_IMEgestState_message()
                , test_IMRelayState_message()
                , test_IMServerLoad_message()
                ]),
  ok.


encode_decode(Msg) ->
  {Name, WireMsg} = to_wire_message(Msg),
  From = from_wire_message(Name, WireMsg),
  {just, Msg} = From.


test_IMAggregatorAvailable_message() ->
  {iMAggregatorState,{available},{agentKey, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>,{primary}},<<"172.16.169.1">>,#{numProfiles => 2,totalBitrate => 1500}}.

test_IMAggregatorStopped_message() ->
  {iMAggregatorState,{stopped},{agentKey, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>,{primary}},<<"172.16.169.1">>,#{numProfiles => 0,totalBitrate => 0}}.

test_IMEgestState_message() ->
  {iMEgestState,{available},{agentKey,<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>,{primary}},<<"172.16.170.1">>}.

test_IMRelayState_message() ->
  {iMRelayState,{available},{agentKey,<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>,{primary}},<<"172.16.170.1">>}.

test_IMServerLoad_message() ->
  {iMServerLoad,<<"172.16.169.3">>,#{cpu => 100.0,network => 0},true}.
