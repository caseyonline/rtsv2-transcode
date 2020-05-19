# RTS-V2 release XX

**What's new**

* **RTSV2-78** AAC-HE and AAC-HE2 now supported for ingest

* TimeZero message being sent to player to indicate video timestamp of first frame

* Initial support for Canary mode

  * initial canary state currently defined in rtsv2_core.config

  * current state is visible from the healthCheck endpoint

  * canary state can be changed with a POST to /support/canary:

    ```curl -v -X POST -H "content-type: application/json" --data '"canary"' http://172.16.171.3:3000/support/canary```

    Post body is either "canary" or "live"

  * canary mode **cannot** be changed if the node has any active agents

    * current agent count is also returned from the healthCheck endpoint

* Prometheus endpoint for egest stats on ```/support/egests/metrics```

# RTS-V2 release 96

**What's new**

* hlsPublish flag in ingest aggregator json to indicate if HLS publishing is active
* CODE_LOADING_MODE set in start.sh, which will remove misleading error logs about missing NIFs on startup
* playbackBaseUrl removed from HlsPushSpec
* ingest no longer attempts to parse on-fi messages, and instead passes the payload as a json object through to the client
* Prometheus Ingest stats endpoint fixed
* Initial changes for Canary mode 
  * **BREAKING CHANGE** as discussed in a recent status call, canary mode is now being handled by having the ingest endpoints available on a different port rather than via a segment in the URL; this being simpler to secure to prevent public access.  As such, all the public URLs have now had the canary segment removed.

# RTS-V2 release 94

**What's new**

- **RTSV2-66** - handling of invalid data object messages from client fixed
- Player SDK senderRef renamed to requestResponseCorrelationId in dataObjectUpdateMessage
- Fix to IngestAggregator bug as reported by @hklingenberg on slack
- Egest EQ logging now includes shortName.  It does not include streamName since that doesn't really make sense on egest
- /support/healthCheck endpoint now returns load information
- segmentDuration and playlistDuration in HlsPushSpec are now optional with default values coming from the llnwApiConfig section of the rtsv2_core.config file

# RTS-V2 release 93
**What's new**

- Added openssl to release package

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
