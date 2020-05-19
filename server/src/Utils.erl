-module(rtsv2_utils@foreign).


-export([ noprocToMaybeImpl/3
        , badargToMaybeImpl/3
        , cryptoStrongBytes/1
        , binaryToBase64/1
        , binaryToHexStr/1
        ]).

noprocToMaybeImpl(Nothing, Just, Eff) ->
  fun() ->
      try Eff() of
          Result -> Just(Result)
      catch
        exit:{noproc, _} ->
          Nothing
      end
  end.

badargToMaybeImpl(Nothing, Just, Eff) ->
  fun() ->
      try Eff() of
          Result -> Just(Result)
      catch
        error:badarg ->
          Nothing
      end
  end.

cryptoStrongBytes(Len) ->
  fun() ->
      crypto:strong_rand_bytes(Len)
  end.

binaryToBase64(Bin) ->
  base64:encode(Bin).

binaryToHexStr(Bin) ->
  i_convert:convert(Bin, hexbin).
