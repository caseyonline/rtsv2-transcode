-module(rtsv2_agents_transPoP@foreign).

-export([ to_wire_message/1
        , from_wire_message/2
        , x/0
        ]).

%%------------------------------------------------------------------------------
%% The list of message names - a, b, c...
%% If a message changes add a NEW name and make sure that the
%% old message can still be parsed / provide defaults for the new data
%% before eventually retiring messages that are no longer in the wild
%%------------------------------------------------------------------------------
-define(tMAggregatorStateMsgName, <<"a">>).
%%------------------------------------------------------------------------------
%% End of list of message names
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------------------
to_wire_message(
  { tMAggregatorState
  , Type
  , {agentKey, SlotId, SlotRole}
  , ServerAddress
  , _SlotCharacteristics = #{ numProfiles := NumProfiles
                            , totalBitrate := TotalKBitrate
                            }
  })
  when
    byte_size(ServerAddress) > 0 andalso byte_size(ServerAddress) =< 16#80, %% Server adress =< 256
    NumProfiles              > 0 andalso NumProfiles =< 16#10,              %% Max of 16 profiles
    TotalKBitrate            > 0 andalso TotalKBitrate =< 16#40000          %% Max of 250 Mbps of a single ADR ladder total
    ->
  { ?tMAggregatorStateMsgName
  , << (to_wire_element_event_type(Type)):1                             %%   1    1
     , (to_wire_element_slot_role(SlotRole)):1                          %%   1    2
     , (NumProfiles - 1):4                                              %%   4    6
     , (TotalKBitrate - 1):18/unsigned-big-integer                      %%  18   24
     , SlotId:16/binary                                                 %% 128  152
     , ServerAddress/binary                                             %% rest of msg
    >>
  }.

from_wire_message( ?tMAggregatorStateMsgName
                 , << WireType:1                                        %%   1     1
                    , WireSlotRole:1                                    %%   1     2
                    , WireNumProfiles:4                                 %%   4     6
                    , WireKBitrate:18/unsigned-big-integer              %%  18    24
                    , SlotId:16/binary                                  %% 128   160
                    , ServerAddress/binary                              %% rest of msg
                   >>
                 ) ->
  { just, { tMAggregatorState
          , from_wire_element_event_type(WireType)
          , {agentKey, SlotId, from_wire_element_slot_role(WireSlotRole)}
          , ServerAddress
          , #{ numProfiles => WireNumProfiles + 1
             , totalBitrate => WireKBitrate + 1
             }
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
  { tMAggregatorState
  , {available}
  , {agentKey
    , <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>
    , {primary}}
  , <<"172.16.169.1">>
  , #{ numProfiles => 2
     , totalBitrate => 1500
     }
  }.
