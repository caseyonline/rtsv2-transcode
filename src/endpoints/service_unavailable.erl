-module(service_unavailable).

-export([ init/2
        ]).

init(Req, State) ->
    Req2 = cowboy_req:reply(503, #{}, <<>>, Req),
    {ok, Req2, State}.
