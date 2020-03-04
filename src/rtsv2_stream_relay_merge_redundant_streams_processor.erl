-module(rtsv2_stream_relay_merge_redundant_streams_processor).

-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").

-behaviour(workflow_processor).

-export([ spec/1
        , initialise/1
        , process_input/2
        , flush/1
        , ioctl/2
        ]).

-define(PACKETS_PER_SWAP, 1000).

-define(state, ?MODULE).

-record(?state,
        { state_per_ssrc = #{} :: maps:map(rtp:ssrc(), state_for_ssrc())
        }).

-record(state_for_ssrc,
        { seen_current = #{} :: maps:map(rtp:sequence_number(), true)
        , seen_previous = #{} :: maps:map(rtp:sequence_number(), true)
        , packets_until_swap = ?PACKETS_PER_SWAP :: non_neg_integer()
        }).
-type state_for_ssrc() :: #state_for_ssrc{}.

%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{
     consumes = ?all,
     generates = ?all,
     supports_synchronous_mode = true,
     is_pure = true
    }.

initialise(_Processor) ->
  {ok, #?state{}}.

process_input(#rtp{ ssrc = SSRC
                  , sequence_number = SequenceNumber
                  } = Packet,
              #?state{ state_per_ssrc = StatePerSSRC
                     } = State
             ) ->

  #state_for_ssrc{ seen_current = Current
                 , seen_previous = Previous
                 , packets_until_swap = PacketsUntilSwap
                 } =
    SSRCState =
      maps:get(SSRC, StatePerSSRC, #state_for_ssrc{}),

  IsKnownSequenceNumber = maps:is_key(SequenceNumber, Current) orelse maps:is_key(SequenceNumber, Previous),

  case IsKnownSequenceNumber of
    false ->

      NewSSRCState =
        case PacketsUntilSwap of
          0 ->
            SSRCState#state_for_ssrc{ seen_previous = Current
                                    , seen_current = maps:put(SequenceNumber, true, #{})
                                    , packets_until_swap = ?PACKETS_PER_SWAP
                                    };

          N ->
            SSRCState#state_for_ssrc{ seen_current = maps:put(SequenceNumber, true, Current)
                                    , packets_until_swap = N - 1
                                    }
        end,

      NewState = State#?state{ state_per_ssrc = maps:put(SSRC, NewSSRCState, StatePerSSRC) },

      {ok, Packet, NewState};

    true ->
      {ok, State}
  end.

flush(State) ->
  {flush_complete, State}.

ioctl(_, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
