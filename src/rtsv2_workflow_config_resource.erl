-module(rtsv2_workflow_config_resource).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").

-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2
        ]).

-record(state,
        {
         module :: atom(),
         function :: atom(),
         reference :: term()
        }).

init(Req, _) ->

  Req2 = cowboy_req:set_resp_header(<<"cache-control">>, <<"no-cache, no-store, must-revalidate">>, Req),
  Req3 = cowboy_req:set_resp_header(<<"pragma">>, <<"no-cache">>, Req2),
  Req4 = cowboy_req:set_resp_header(<<"expires">>, <<"0">>, Req3),

  Reference = cowboy_req:binding(reference, Req),

  {cowboy_rest, Req4, #state{reference = Reference}}.

allowed_methods(Req, State) ->
  { [<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, to_json}
   ], Req, State}.

to_json(Req, State = #state{reference = Reference}) ->

  Config =
    #workflow_visualisation_config{encoder_id = <<"placeholder">>,
                                  graph = #latest_only_graph_visualisation_config{
                                    url = <<"graph">>
                                  },
                                  graph_history =  undefined,
                                  metrics = #encoder_metrics_visualisation_config{
                                    url = <<"metrics">>
                                  }},


  Json = id3as_media_mapper:record_to_json(Config),

  {jsx:encode(Json), Req, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
