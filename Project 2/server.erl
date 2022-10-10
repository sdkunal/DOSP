-module(server).
-import(lists, [nth/2]).
-export([start/1, initiate/2, processKiller/1,add/1,createList/2,append/3,spreadRumor/5,randomize/5]).

start(NumNodes) ->
    List=createList(1,NumNodes),
    RandList = [rand:uniform(NumNodes) || _ <- lists:seq(1, NumNodes div NumNodes)],
    io:format("~p randomNum: ~n",[nth(1,RandList)]),
    Curr=nth(1,RandList),
    randomize(RandList,1,List,NumNodes,Curr).
    
randomize(RandList,Index,List,NumNodes,Curr)->
    case Index=<length(RandList) of
        true->
            nth(Curr,List) ! {receiveRumour,List,NumNodes},
            % io:format("hello~n",[]),
            Curr2=nth(Index+1,RandList),
            randomize(RandList,Index+1,List,NumNodes,Curr2);
        false->
            ""
    end.

    

createList(S,E)->
    append(S,E,[]).

append(S,E,L)->
    case S=<E of
        true ->
            append(S+1,E,lists:append([L,[spawn(server,initiate,["This is a rumor",1])]]));
        false ->
            L
    end.

initiate(Rumor,Count) ->
    receive
        {receiveRumour,List,NumNodes} ->
            io:format("~p ~p ~n",[self(),Rumor]),
            io:format("~p Count: ~n",[Count]),
            if
                Count==5->
                    processKiller(self());
                true->
                    spreadRumor(length(List),NumNodes,List,Rumor,self()),
                    initiate(Rumor,Count+1)
                    
            end;
        stop ->
            io:format("~p Stopping~n",[self()])
    end.

spreadRumor(Nodes,NumNodes,List,Rumor,CurrPID)->
    case NumNodes>0 of
        true->
            case checkIfEqual(CurrPID,nth(NumNodes,List)) of
                true->
                    spreadRumor(Nodes,NumNodes-1,List,Rumor,CurrPID);
                false->
                    nth(NumNodes,List) ! {receiveRumour,List,Nodes},
                    spreadRumor(Nodes,NumNodes-1,List,Rumor,CurrPID)
            end;
        false->
            ""
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


% initiate(Count) ->
%     receive
%         receiveRumour ->
%             io:format("~p Received ~n",[self()]),
%             io:format("~p Count ~n",[Count]),
%             if
%                 Count==5->
%                     processKiller(self()),
%                     initiate(Count);
%                 true->
%                     'actor1' ! receiveRumour,
%                     initiate(Count+1)
%             end;
%         stop ->
%             io:format("~p Stopping~n",[self()]),
%             exit('actor1',kill),
%             exit(self(),kill)
%     end.






% count(Cnt)->
%     if
%         Cnt==10->
%             io:format("terminating ~n",[]);
%         true->
%             io:format("~p ~n",[Cnt]),
%             count(Cnt+1)
%     end.




add(List)->
    io:format("~p hello~n",[nth(1,List)]).
