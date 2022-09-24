-module(tut15).

-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_PID) ->         %child pid started executing this function #5, arguments - number, masterPID
    Pong_PID ! {ping, self()},    % searches for this RPC in the masterPID's proc #6  
    receive
        pong ->
            io:format("Ping received pong~n", [])    % child process runs if matched #9
    end,
    ping(N - 1, Pong_PID).

pong() ->                                 % master in receiving mode  - step 2
    receive           %receiving mode activated #3
        finished ->   % these are the patterns masterPID  
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->                            % #7 matches with this pattern, pingPID is child
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,    % searches for this pattern in child process #8
            pong()
    end.

start() ->
    Pong_PID = spawn(tut15, pong, []),      %master PID created - step 1
    spawn(tut15, ping, [3, Pong_PID]).     %created a child PID #4