# RTS-V2 release 88
**What's new**

 - Load monitoring and capacity management implemented; benchmarking still to be done to put sane numbers into the configuration

 - **RTSV2-26 / RTSV2-52 / RTSV2-54** - Ingest instances monitor media flow and exit if nothing detected in last X.  Parameters are configurable in ingestInstanceConfig

 - Workflow visualisation URLs are fixed.  HTML5 visualiser not yet integrated, but static visualisations can be generated:

    ```bash
    ➜  rtsv2 git:(master) ✗ curl -s http://172.16.171.3:3000/system/workflows | jq .
    [
      {
        "name": "{rtmp_ingest_handler,{ingestKey,<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>,\n                                {primary},\n                                <<\"high\">>}}",
        "display_name": "RTMP Ingest",
        "tags": {
          "profile": "high",
          "slot": "\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0001",
          "stream_role": "{primary}",
          "type": "rtmp_ingest_handler"
        },
        "ref": "#Ref<0.172368683.3401318403.109222>",
        "links": {
          "graph": "0.172368683.3401318403.109222/graph",
          "metrics": "0.172368683.3401318403.109222/metrics",
          "structure": "0.172368683.3401318403.109222/structure"
        }
      },
      ...
    ]
    curl -v -H "accept: text/vnd.graphviz"  http://172.16.171.3:3000/system/workflows/0.172368683.3401318403.109222/structure > /tmp/graphviz.gv
    ```

    Then take the resulting .gv file and use a tool such as GraphViz or https://dreampuf.github.io/GraphvizOnline to render

- Initial release of HLS playlist and segment generation and submission from Ingest Aggregator

- Basic Authentication supported on the Limelight provisioning APIs.  To configure, add a ```useBasicAuth``` field to ```llnwApiConfig```:

    ```
    {llnwApiConfig, #{ streamAuthTypeUrl => <<"http://1.2.3.4/rts/v1/streamauthtype">>
                     , streamAuthUrl => <<"http://1.2.3.4/rts/v1/streamauth">>
                     , streamPublishUrl => <<"http://1.2.3.4/rts/v1/streampublish">>
                     , slotLookupUrl => <<"http://1.2.3.4/rts/v1/slotid/{account}/{streamName}">>
                     , useBasicAuth => <<"user:password">>
                     }}

    ```

* **RTSV2-61** Issue when switching between RTMP and WebRTC ingest resolved

 - Release now running without dependency on NIX on the target host.
