-module(rtsv2_agents_ingestAggregatorInstance@foreign).

-include_lib("id3as_media/include/id3as_workflow.hrl").

-export([
         startWorkflowImpl/1,
         addVariantImpl/3,
         removeVariantImpl/2
        ]).

startWorkflowImpl(SlotName) ->
  fun() ->
      Workflow = #workflow{
                    name = ingest_aggregator_instance,
                    display_name = SlotName,
                    generators = [
                                  #generator{name = ingests,
                                             module = rtsv2_rtmp_ingest_generator
                                            }
                                 ],

                    processors = [
                                  #processor{name = aggregate,
                                             subscribes_to = ingests,
                                             module = rtsv2_rtmp_ingest_aggregator_processor
                                            },

                                  #processor{name = null,
                                             subscribes_to = ?previous,
                                             module = dev_null_processor
                                            }
                                 ]
                   },

      {ok, Pid} = id3as_workflow:start_link(Workflow, self()),

      {ok, Handle} = id3as_workflow:workflow_handle(Pid),

      Handle
  end.

addVariantImpl(Handle, StreamAndVariant, Url) ->
  fun() ->
      ok = id3as_workflow:ioctl(ingests, {add_ingest, StreamAndVariant, Url}, Handle)
  end.

removeVariantImpl(Handle, StreamAndVariant) ->
  fun() ->
      ok = id3as_workflow:ioctl(ingests, {remove_ingest, StreamAndVariant}, Handle)
  end.
