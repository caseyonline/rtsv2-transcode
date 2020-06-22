# RTS-V2 release 107

**What's new**

* Fix to HLS publish if onFI messages present in inbound RTMP stream

* Fix to missing byte count in ingest eq log and missing ingest prometheus stats

* Fix to missing slotName / streamName / port number in egest eq log

* Initial support for RTMP egest, using an RTMP url such as:

  ```
  rtmp://node-name:1937/00000000-0000-0000-0000-000000000001/high
  ```

  A variant of stream discovery will be added to return the RTMP-style URLs for use on RTMP clients, but for now you need to know the slotId and profile name

* Fix to timing related issue that could prevent HLS output



# RTS-V2 release 106

**What's new**

* Slot Lookup API is now cached and responses to cache misses are sent over IntraPoP serf to pre-load the caches on peers.  Basic cache statistics returned under the healthCheck URL.

* RTVS2-50 - now fixed;  on a fatal error connecting to serf (such as introduced with tcpkill), the supervision trees will now retry repeatedly until finally the erlang node will exit.  At that point, systemd will restart it and the loop will continue until the connection to serf recovers.

* RTSV2-63 - now fixed; URLs are correctly on the wss scheme

* RTSV2-112 - EQ logs now flushed out on clean node exits

* RTSV2-111 - EQ Egest logs now include the byte counts

* client_ip field now included in all auth calls

* ingest bitrate validation now happening - currently just raises alerts but once parameters are tuned it can easily be extended to drop the ingest.  Parameters are in rtsv2_core.config:

  ```
  , qosAverageBitrateLowWatermark => 1.2
  , qosAverageBitrateHighWatermark => 1.5
  , qosPeakBitrateLowWatermark => 1.5
  , qosPeakBitrateHighWatermark => 2.0
  
  ```

  average bitrates are measured over a 20 second period, peak bitrates are over a 200ms period.  The numbers are simple factors of the configured bitrate from the SlotProfile

* ingest now validates the abscence of video on audio-only profiles and closes down the ingest if video is detected

* HLS segment numbers no longer reset to zero

# RTS-V2 release 105

**What's new**

* Further changes to interfaces to correctly support 'any'.  There are now 5 interfaces listed in rtsv2_environment

  * public listen - used for /public urls and live RTMP

  * support listen - used for /support urls and canary RTMP

  * system listen - used for internal /system urls

  * intra-serf - used for intra-serf traffic

  * trans-serf - used for trans-serf traffic

    Of these, the first 3 are used for the 3 HTTP servers and 'any' is a valid interface name, which equates to listening on 0.0.0.0.

    Note that currently we only hav eon nodee name in the popDefinition file - this means that all generated URLs have that node name embedded, which will obviously only resolve to one IP address.  As such, currently public, support and system interfaces all need to be the same.  If it is decided that we want to actually support multiple interfaces in production then we will need to extend popDefinition to include names that resolve to the appropriate interfaces (or have a systematic way in which thosee names can be derived, such as popDefinition having 'rtsv2-poc-1.devnet.llnw.net' and the platform deriving something like 'rtsv2-poc-1.system.devnet.llnw.net' etc).

    Also note that we are currently proxying through nginx for handling SSL termination.  system urls do not go through nginx, but both support and public do - so again if we want multiple interfaces it would, in production, probably be nginx configuration handling that, with the rtsv2 configuration binding public and support to lo0.

    Suspect this needs further discussion to finalise how live is going to be setup.



* Initial support for Alerts returned in the healthCheck URL.  For example:

  ```json
    "alerts": [
      {
        "codecId": "h263",
        "context": {
          "profileName": "high",
          "slotId": "00000000-0000-0000-0000-000000000001",
          "slotName": "slot1",
          "slotRole": "primary"
        },
        "initialReport": "1591347301380",
        "lastReport": "1591347301380",
        "pid": "<0.384.0>",
        "reason": "invalidVideoCodec",
        "repeatCount": 0,
        "source": {
          "function": "map_video_format_from_rtmp",
          "line": 133,
          "module": "rtmp_utils"
        },
        "type": "ingestFailed"
      },
      {
        "context": {
          "profileName": "high",
          "slotId": "00000000-0000-0000-0000-000000000001",
          "slotName": "slot1",
          "slotRole": "primary"
        },
        "initialReport": "1591347300760",
        "lastReport": "1591347300760",
        "pid": "<0.360.0>",
        "repeatCount": 0,
        "source": {
          "function": "-init/2-fun-5-",
          "line": 234,
          "module": "Rtsv2.Agents.IngestInstance@ps"
        },
        "type": "ingestStarted"
      }
    ],
  ```



  This show a list of alerts, with the latest first.  Each alert has the following fields:

  	* initialReport - a millisecond-timestamp of the first time this alert was reported
  	* lastReport - a millisecond-timestamp of the most recent time this alert was reported
  	* repeatCount - how many times this alert has been repeated
  	* type - the alert type
  	* source - not always present, but shows the module / function / line that raised th report
  	* context - not always present, but shows RTS-V2 context information (slotId, name etc)
   * and then various fields that are dependent on the alert type.  Current types with their fields:
      * "ingestStarted"
        	*  no addtional fields
      * "ingestFailed"
        	* reason
         * and then various fields that are dependent on the reason.  Current reasons with their fields:
            * "invalidVideoCodec"
              	* codecId - the invalid codec id
      * "lsrsFailed"
        	* reason - reason text returned from the underlying http client
      * "genericAlert"
        	* text - string formatted from the underlying alert

  More alerts will get added - let us know if you've seen things in logs that you'd like reported through this mechanism.  We will formally document this soon, along with the other endpoints in /public and /support.  For now, you can see the types and the json rendering in rtsv2/shared/Common.purs

  The initialReport / lastReport / repeatCount fields are to ensure that, for example, a client continually connecting and sending invalid video doesn't cause an explosion of alerts.  reportCounts are based on contextual data, so two different clients attempting to connect with invalid video would show up as two different alerts.



* The healthCheck URLnow supports a 'alertsFrom' parameter on the query string, which will filter out any alerts that have a 'lastReport' time earlier than 'from'

* Updated our side of provisioning API to support new audioBitrate / videoBitrate fields in Profile

* Initial support for audio-only streams - these are specified by having slots where *all* profiles have a videoBitrate of zero.  A couple of notes:

  * The ingest does not currently validate that it is only receiving audio;  that wil lchange soon, so please don't try submitting a video stream to an audio-only slot :)

  * The init message sent down the websocket to the player has an additional field that indicates if the stream is audio-only;  the client needs to create an appropriate SDP offer to only receive audio - the reference egest player shows this in Session.ts:412



* RTMP connections now timeout more reliably if the client doesn't send data - previous version required media flowing before the inactivity timer was started, it now starts as soon as the connection is established



* Minor HLS tweaks to support 'video-only' streams, although note that these are not (yet!) officially supported

* HLS sequence numbers no longer reset to zero on a restart

# RTS-V2 release 103

**What's new**

* Initial release of RunState functionality - passiveDrain and forceDrain

  * initial run state currently defined in rtsv2_core.config

  * current state is visible from the healthCheck endpoint

  * run state can be changed with a POST to /support/runState:

    ```bash
    curl -v -X POST -H "content-type: application/json" --data '"passiveDrain"' http://172.16.171.3:3000/support/runState
    ```

* Initial release of Media Vault integration

* HTTP endpoints now broken out to 3 different ports for public / support / system

* rtsv2_environment now supports different interfaces for public / support / system (previously was just public / private)

* rtsv2_environment now supports 'any' for an interface name, which results in bindings to 0.0.0.0

# RTS-V2 release 99

**What's new**

* Just a bug-fix release, no new or changed function:
  * Bug fix for incorrect handling on onFI frames in the ingest stats collection (which was in turn causing other ingests to fail)
  * Change to supervision structure for ingest so that any future issues in stats collection have minimal impact
  * Bug fix when handling a failure of the media gateway
  * Bug fix that could have allowed a non-canary stream relay to get created on a canary node

# RTS-V2 release 98

**What's new**

* **RTSV2-78** AAC-HE and AAC-HE2 now supported for ingest

* TimeZero message being sent to player to indicate video timestamp of first frame

* Initial support for Canary mode

  * initial canary state currently defined in rtsv2_core.config

  * current state is visible from the healthCheck endpoint

  * canary state can be changed with a POST to /support/canary:

    ```bash
curl -v -X POST -H "content-type: application/json" --data '"canary"' http://172.16.171.3:3000/support/canary
    ```

Post body is either "canary" or "live"

* canary mode **cannot** be changed if the node has any active agents

  * current agent count is also returned from the healthCheck endpoint

* Prometheus endpoint for egest stats on ```/support/egests/metrics``

* Initial release of media-gateway to give improved edge scalability

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
