-module(server).
% -import(, []).
-export([start/0, initiate/2,initiate/1]).

start() ->
    register('actor1', spawn(server, initiate, ["This is a rumor",1])),
    io:format("Initiating a rumor~n"),
    register('actor2', spawn(server, initiate, [1])),
    register('actor3', spawn(server, initiate, [1])),
    'actor1' ! receiveRumour.
% PID ! printThisShit,
% PID2 ! sendMSG.


initiate(Count) ->
    receive
        receiveRumour ->
            io:format("Received ~n",[]),
            io:format("~p Count ~n",[Count]),
            % 'actor1' ! receiveRumour,
            % count=count+1,
            if
                Count==10->
                    % self ! stop;
                    "";
                true->
                    % self ! receiveRumour
                    'actor1' ! receiveRumour
                    
            end,
            initiate(Count+1);
        stop ->
            io:format("~p Stopping~n",[self()]),
            'actor1' ! stop
    end.

initiate(Rumor,Count) ->
    receive
        receiveRumour ->
            io:format("Received a rumor ~n",[]),
            io:format("~p Count parent~n",[Count]),
            if
                Count==10->
                    % self ! stop;
                    "";
                true->
                    % self ! receiveRumour
                    'actor2' ! receiveRumour,
                    'actor3' ! receiveRumour
            end,
            initiate(Rumor,Count+1);
        stop ->
            io:format("~p Stopping~n",[self()]),
            'actor2' ! stop,
            'actor3' ! stop
    end.



% count(Cnt)->
%     if
%         Cnt==10->
%             io:format("terminating ~n",[]);
%         true->
%             io:format("~p ~n",[Cnt]),
%             count(Cnt+1)
%     end.
