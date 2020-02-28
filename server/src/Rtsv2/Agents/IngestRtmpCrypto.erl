-module(rtsv2_agents_ingestRtmpCrypto@foreign).

-export([ compareAdobeChallengeImpl/6
        , compareLlnwChallengeImpl/7
        ]).

compareAdobeChallengeImpl(Username, Salt, Password, Challenge, ClientChallenge, ClientResponse) ->

  rtmp:compare_adobe_challenge_response(Username,
                                        Salt,
                                        Password,
                                        Challenge,
                                        ClientChallenge,
                                        ClientResponse).

compareLlnwChallengeImpl(Username, Password, ShortName, Nonce, ClientNc, ClientNonce, ClientResponse) ->

  Realm = <<"live">>,
  Method = <<"publish">>,
  Qop = <<"auth">>,

  rtmp:compare_llnw_challenge_response(Username,
                                       Realm,
                                       Password,
                                       Method,
                                       ShortName,
                                       Nonce,
                                       ClientNc,
                                       ClientNonce,
                                       Qop,
                                       ClientResponse).
