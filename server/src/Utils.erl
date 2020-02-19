-module(rtsv2_utils@foreign).


-export([ noprocToMaybeImpl/3
        , cryptoStrongBytes/1
        , binaryToBase64/1
        , binaryToHexStr/1
        ]).

%% catchNotThereImpl :: forall a. Maybe a -> (a -> Maybe a) -> Effect a -> Effect (Maybe a)
noprocToMaybeImpl(Nothing, Just, Eff) ->
  fun() ->
      try Eff() of
          Result -> Just(Result)
      catch
        exit:{noproc, _} ->
          Nothing
      end
  end.

cryptoStrongBytes(Len) ->
  fun() ->
      crypto:strong_rand_btyes(Len)
  end.

binaryToBase64(Bin) ->
  base64:encode(Bin).

binaryToHexStr(Bin) ->
  i_convert:convert(Bin, hexbin).
