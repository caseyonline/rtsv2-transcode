-module(rtsv2_rtp_trunk_receiver_generator).

-behaviour(workflow_generator).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").

-export([ init/1
        , handle_info/2
        , ioctl/2
        ]).

-define(state, ?MODULE).

-record(?state,
        { socket :: gen_udp:socket()
        , parse_info :: rtp:parse_info()
        }).

%%------------------------------------------------------------------------------
%% Generator API
%%------------------------------------------------------------------------------
init(#generator{}) ->
  {ok, Socket} = gen_udp:open(0, [binary, {recbuf, 100 * 1500}]),

  { ok
  , #?state{ socket = Socket
           , parse_info = rtsv2_rtp_util:build_parse_info()
           }
  }.

handle_info({udp, _Socket, _FromIP, _FromPort, Data}, #?state{ parse_info = ParseInfo } = State) ->
  RTP = rtp:parse(avp, Data, ParseInfo),
  {output, RTP, State}.

ioctl(get_port_number, State = #?state{ socket = Socket }) ->
  {ok, PortNumber} = inet:port(Socket),
  {ok, PortNumber, State}.
