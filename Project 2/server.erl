-module(server).
% -import(, []).
-export([start/0, print/0]).

start() ->
    % PID = spawn(server, print, []),
    % PID2 = spawn(server, print, []),
    PIDList = [],
    register('PID1', spawn(server, print, [])),
    PIDList = lists:append(PIDList, ['PID1']),
    register('PID2', spawn(server, print, [])),
    register('PID3', spawn(server, print, [])),
    io:format("~p~n", [registered()]),
    io:format("~p~n", [length(registered())]),
    io:format("~p~n", [self()]),
    io:format("~p~n", [processes()]).
% PID ! printThisShit,
% PID ! printThisShit,
% PID2 ! sendMSG.

print() ->
    receive
        printThisShit ->
            io:format("Started an actor~n", []),
            'PID3' ! printThisShit,
            io:format("After PID3~p~n", [self()]),
            print();
        sendMSG ->
            io:format("Second print~n", [])
    end.
