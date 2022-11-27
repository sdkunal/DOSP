-module(client).
-import(server,[start/2]).
-export([initiateUser/1]).

initiateUser(Uid)->
    io:format("Actor initiated with UID ~p and PID ~p ~n",[Uid,self()]),
    receive
        {initiate}->
            io:format("Actor ~p is in received state with PID ~p ~n",[Uid,self()])
    end.
