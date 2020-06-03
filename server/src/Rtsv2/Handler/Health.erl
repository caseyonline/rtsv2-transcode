-module(rtsv2_handler_health@foreign).

-export([ vmMetricsImpl/0
        , matchQs/2
        ]).

vmMetricsImpl() ->
  fun() ->
      trim_right(prometheus_text_format:format())
  end.

matchQs(Req, Keys) ->
  Matches = cowboy_req:match_qs([{Key, [], undefined}
                                 || Key <- Keys], Req),
  maps:map(fun(_, undefined) -> {nothing};
              (_, Value) -> {just, Value}
           end,
           Matches).

trim_right(Bin) ->
  case binary:last(Bin) of
    $\n ->
      Len = byte_size(Bin) - 1,
      <<Lhs:Len/binary, _:1/binary>> = Bin,
      trim_right(Lhs);
    _ ->
      <<Bin/binary, "\n">>
  end.
