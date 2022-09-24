-module(server).
-import(client,[createRandomString/2,createEncryptedString/1,childProcess/5]).
-export([start/1,serverStart/0]).

start(NumOfZeros) ->
    io:format("Start time: ~p\n",[calendar:now_to_datetime(erlang:timestamp())]),

    MasterPID = spawn(server, serverStart, []),
    % spawn(client, childProcess, [MasterPID, 6, "abcdefghijklmnopqrstuvwxyz0123456789", NumOfZeros, 2000000]),
    % spawn(client, childProcess, [MasterPID, 6, "abcdefghijklmnopqrstuvwxyz0123456789", NumOfZeros, 2000000]),
    % spawn(client, childProcess, [MasterPID, 6, "abcdefghijklmnopqrstuvwxyz0123456789", NumOfZeros, 2000000]),
    % spawn(client, childProcess, [MasterPID, 6, "abcdefghijklmnopqrstuvwxyz0123456789", NumOfZeros, 2000000]),
    % spawn(client, childProcess, [MasterPID, 6, "abcdefghijklmnopqrstuvwxyz0123456789", NumOfZeros, 2000000]).
    spawn(client, childProcess, [MasterPID, 6, "abcdefghijklmnopqrstuvwxyz0123456789", NumOfZeros, 1000]).

serverStart() ->
    receive
        finished ->
            io:format("Master process ends~n", []),
            io:format("End time: ~p\n",[calendar:now_to_datetime(erlang:timestamp())]);

        {get, RandomStr, EncryptedStr, NumOfZeros, ChildPid} ->
            io:format("~p ~p ~p from child: ~p~n", [NumOfZeros, RandomStr, EncryptedStr, ChildPid]),
            serverStart()
    end.