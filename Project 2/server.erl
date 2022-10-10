-module(server).
-import(lists, [nth/2]).
-export([startFull/1, createFullList/2, spawnFull/3, initiateFull/2, spreadFullRumor/5, randomize/5,
         startLine/1, createLineList/2, spawnLine/3, initiateLine/2, spreadLineRumor/4,
         processKiller/1, checkIfEqual/2]).

startFull(NumNodes) ->
    List=createFullList(1,NumNodes),
    RandList = [rand:uniform(NumNodes) || _ <- lists:seq(1, NumNodes div 10)],
    Curr=nth(1,RandList),
    randomize(RandList,1,List,NumNodes,Curr).

randomize(RandList,Index,List,NumNodes,Curr)->
    case Index=<length(RandList) of
        true->
            nth(Curr,List) ! {receiveRumour,List,NumNodes},
            Curr2=nth(Index+1,RandList),
            randomize(RandList,Index+1,List,NumNodes,Curr2);
        false->
            ""
    end.

startLine(NumNodes) ->
    List=createLineList(1,NumNodes),
    nth(1,List) ! {receiveRumour,List,NumNodes}.

createFullList(S,E)->
    spawnFull(S,E,[]).

createLineList(S,E)->
    spawnLine(S,E,[]).

spawnFull(S,E,L)->
    case S=<E of
        true ->
            spawnFull(S+1,E,lists:append([L,[spawn(server,initiateFull,["This is a rumor",1])]]));
        false ->
            L
    end.

spawnLine(S,E,L)->
    case S=<E of
        true ->
            spawnLine(S+1,E,lists:append([L,[spawn(server,initiateLine,["This is a rumor",1])]]));
        false ->
            L
    end.

initiateFull(Rumor,Count) ->
    receive
        {receiveRumour,List,NumNodes} ->
            io:format("~p ~p ~n",[self(),Rumor]),
            io:format("~p Count: ~n",[Count]),
            if
                Count==5->
                    processKiller(self());
                true->
                    spreadFullRumor(length(List),NumNodes,List,Rumor,self()),
                    initiateFull(Rumor,Count+1)
            end;
        stop ->
            io:format("~p Stopping~n",[self()])
    end.

initiateLine(Rumor,Count)->
    receive
        {receiveRumour,List,NumNodes} ->
            io:format("~p ~p ~n",[self(),Rumor]),
            io:format("~p Count: ~n",[Count]),
            if
                Count==5->
                    processKiller(self());
                true->
                    spreadLineRumor(length(List),List,Rumor,self()),
                    initiateLine(Rumor,Count+1)
            end;
        stop->
            io:format("~p Stopping~n",[self()])
    end.

spreadFullRumor(Nodes,NumNodes,List,Rumor,CurrPID)->
    case NumNodes>0 of
        true->
            case checkIfEqual(CurrPID,nth(NumNodes,List)) of
                true->
                    spreadFullRumor(Nodes,NumNodes-1,List,Rumor,CurrPID);
                false->
                    nth(NumNodes,List) ! {receiveRumour,List,Nodes},
                    spreadFullRumor(Nodes,NumNodes-1,List,Rumor,CurrPID)
            end;
        false->
            ""
    end.

spreadLineRumor(Nodes,List,Rumor,CurrPID)->
    case checkIfEqual(CurrPID,nth(1,List)) of
        true->
            nth(2,List) ! {receiveRumour,List,Nodes};
        false->
            case checkIfEqual(CurrPID,lists:last(List)) of
                true->
                    nth(length(List)-1,List) ! {receiveRumour,List,Nodes};
                false->
                    nth(string:str(List,[CurrPID])+1,List) ! {receiveRumour,List,Nodes},
                    nth(string:str(List,[CurrPID])-1,List) ! {receiveRumour,List,Nodes}
            end
    end.

checkIfEqual(PID1,PID2)->
    if
        PID1==PID2 ->
            true;
        true->
            false
    end.

processKiller(PIDToKill) ->
    exit(PIDToKill, kill).
