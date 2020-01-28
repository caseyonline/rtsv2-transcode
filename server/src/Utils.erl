-module(rtsv2_utils@foreign).


-export([ noprocToMaybeImpl/3
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
