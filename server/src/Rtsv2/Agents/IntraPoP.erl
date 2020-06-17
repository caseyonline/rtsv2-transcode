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
-define(iMAggregatorStateMsgName, <<"a">>).
-define(iMEgestStateMsgName,      <<"b">>).
-define(iMRelayStateMsgName,      <<"c">>).
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
  , Type
  , {agentKey, SlotId, SlotRole}
  , ServerAddress
  , _SlotCharacteristics = #{ numProfiles := NumProfiles
                            , totalBitrate := TotalKBitrate
                            }
  })
  %% when
  %%   NumProfiles              > 0 andalso NumProfiles =< 16#10,              %% Max of 16 profiles
  %%   TotalKBitrate            > 0 andalso TotalKBitrate =< 16#40000          %% Max of 250 Mbps of a single ADR ladder total
    ->
  { ?iMAggregatorStateMsgName
  , << (to_wire_element_event_type(Type)):1                             %%   1    1
     , (to_wire_element_slot_role(SlotRole)):1                          %%   1    2
     , (NumProfiles - 1):4                                              %%   4    6
     , (TotalKBitrate - 1):18/unsigned-big-integer                      %%  18   24
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
  }.

from_wire_message(
  ?iMAggregatorStateMsgName
 , << WireType:1                                        %%   1     1
    , WireSlotRole:1                                    %%   1     2
    , WireNumProfiles:4                                 %%   4     6
    , WireKBitrate:18/unsigned-big-integer              %%  18    24
    , SlotId:16/binary                                  %% 128   160
    , ServerAddress/binary                              %% rest of msg
   >>
 ) ->
  { just, { iMAggregatorState
          , from_wire_element_event_type(WireType)
          , {agentKey, SlotId, from_wire_element_slot_role(WireSlotRole)}
          , ServerAddress
          , #{ numProfiles => WireNumProfiles + 1
             , totalBitrate => WireKBitrate + 1
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
  }.


%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
to_wire_element_event_type({available}) ->
  0;
to_wire_element_event_type({stopped}) ->
  1.

from_wire_element_event_type(0) ->
  {available};
from_wire_element_event_type(1) ->
  {stopped}.

to_wire_element_slot_role({primary}) ->
  0;
to_wire_element_slot_role({backup}) ->
  1.

from_wire_element_slot_role(0) ->
  {primary};
from_wire_element_slot_role(1) ->
  {backup}.




%%------------------------------------------------------------------------------
%% Tests - should move to the Purerl so that changes to the
%% types causes breaks
%%------------------------------------------------------------------------------
x() ->
  Msg = test_message(),
  {Name, ToBin} = to_wire_message(Msg),
  From = from_wire_message(Name, ToBin),
  {just, Msg} = From.


test_message() ->
  { iMAggregatorState
  , {available}
  , {agentKey
    , <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>
    , {primary}}
  , <<"172.16.169.1">>
  , #{ numProfiles => 2
     , totalBitrate => 1500
     }
  }.
