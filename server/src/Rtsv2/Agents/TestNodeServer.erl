-module(rtsv2_agents_testNodeServer@foreign).
-export([execute/2,test/1,build/2]).

execute(Title, Bitrate) ->
  fun() ->
    Cmd = "ffmpeg -i " ++ erlang:binary_to_list(Title) ++ ".mp4 -b:v " ++ erlang:binary_to_list(Bitrate) ++ " " ++ erlang:binary_to_list(Title) ++ "-" ++ erlang:binary_to_list(Bitrate) ++ ".mp4",
    Pid = spawn(fun () -> transcode(Cmd) end),
    unit
  end.

transcode(Cmd) ->
  Opt = [stream, exit_status, use_stdio, stderr_to_stdout, in, eof],
  P = open_port({spawn, Cmd},Opt),
  {R, D} = do_read(P,[]),
  io:format("--- Transcode Complete ---\n").

do_read(Port,D) ->
  receive
    {Port, {data, Data}} ->
      %io:format("Data: ~p~n",[Data]),
      do_read(Port,D ++ Data);
    {Port, eof} ->
      %%io:format("EOF"),
      %%io:format("Data: ~p~n",[D]),
      {ok,D};
    {Port, {exit_status,N}} ->
      io:format("Exit Status: ~p~n", [N]),
      do_read(Port,D);
    Any ->
      io:format("Any, ~p~n", [Any]),
      do_read(Port,D)
  end.

build(Title, Bitrate) ->
  Cmd = "ffmpeg -i " ++ Title ++ ".mp4 -b:v " ++ Bitrate ++ " " ++ Title ++ "-" ++ Bitrate ++ ".mp4",
  Cmd.

test(Title) ->
  fun() ->
    io:format("--- test ---~p\n",[Title]),
    {R,V} = testfunc(24),
    unit
  end.

testfunc(N) ->
  {ok,N+5}.
