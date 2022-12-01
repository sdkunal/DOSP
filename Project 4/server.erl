-module(server).
-import(client,[initiateUser/3,startSimulation/2]).
-import(lists,[nth/2,append/1]).
-export([start/2,generateActors/4,createList/2,createNewList/3,simulator/1,takeOnline/2,replacenth/3]).
-record(user, {id,followers=[],following=[],tweet=[],mentions=[],status=false}). 

start(NumNodes,NumTweets)->
    io:format("Num Nodes: ~p NumTweets: ~p ~n",[NumNodes,NumTweets]),
    % Actor_List=generateActors(1,NumNodes,self()),
    Actor_List=generate(1,NumNodes,self()),
    io:format("Actor_List: ~p ~n",[Actor_List]),
    % io:format("Self: ~p ~n",[self()]),
    Record_List = createList(1, NumNodes),
    spawn(client,startSimulation,[Actor_List,self()]),
    % io:format("List: ~p ~n",[List]),
    simulator(Record_List).
    % P=#user{id=1,followers=[1,2,3],following=[4,5],tweet=["abc","xyz"],mentions=["mention1","mention2"],status=true},

generate(S,E,PPid)->
    generateActors(S,E,[],PPid).

simulator(List)->
    receive
        {addFollowers}->
            io:format("in simulator: ~p ~n",[self()]),
            simulator(List);
        {makeOnline,Uid,Pid}->
            io:format("Actor ~p is in online state with PID ~p ~n",[Uid,Pid]),
            List1=takeOnline(List,Uid),
            simulator(List1);
        {setFollowers,Uid,FollowerList}->
            io:format("Followers ~p assigned to ~p ~n",[FollowerList,Uid]),
            List1=addFollowers(Uid,FollowerList,List),
            % io:format("List with followers: ~p ~n",[List1]),
            % List2=addToFollowing(Uid,FollowerList,List1),
            Length=length(FollowerList),
            List2=addToFollowing(Uid,FollowerList,List1,1,Length),
            %io:format("Final List: ~p ~n",[List2]),
            simulator(List2);
        {go_offline,Uid}->
            List1=takeOffline(List,Uid),
            io:format("After offline List: ~p ~n",[List1]),
            simulator(List1)
    end.

takeOffline(List,Uid)->
    P=nth(Uid,List),
    P1 = P#user{status = false},
    replacenth(List,Uid,P1).

addToFollowing(Uid,FollowerList,List,S,Length)->
    case S=<Length of
        true->
            Curr=nth(S,FollowerList),
            P=nth(Curr,List),
            TempList=P#user.following,
            P1=P#user{following = lists:append(TempList,[Uid])},
            List2=replacenth(List,Curr,P1),
            addToFollowing(Uid,FollowerList,List2,S+1,Length);
        false->
            List
    end.

addFollowers(Uid,FollowerList,List)->
    P=nth(Uid,List),
    P1 = P#user{followers = FollowerList},
    replacenth(List,Uid,P1).

takeOnline(List,Uid)->
    P=nth(Uid,List),
    P1 = P#user{status = true},
    replacenth(List,Uid,P1).

replacenth(L,Index,NewValue) -> 
    {L1,[_|L2]} = lists:split(Index-1,L),
    L1++[NewValue|L2].

generateActors(S,E,L,PPid)-> %E is numNodes
    case S=<E of
        true->
            % spawn(client,initiateUser,[S,PPid,E]),
            generateActors(S+1,E,lists:append([L,[spawn(client,initiateUser,[S,PPid,E])]]),PPid);
        false->
            io:format("All actors generated ~n"),
            L
    end.

createList(S, E) ->
    createNewList(S, E, []).

createNewList(S, E, L) ->
    case S =< E of
        true ->
            createNewList(
                S + 1,
                E,
                append([L, [#user{id=S,followers=[],following=[],tweet=[],mentions=[],status=false}]])
            );
        false ->
            L
    end.
