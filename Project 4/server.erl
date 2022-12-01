-module(server).
-import(client,[initiateUser/3]).
-import(lists,[nth/2,append/1]).
-export([start/2,generateActors/3,createList/2,createNewList/3,simulator/1,takeOnline/2,replacenth/3]).
-record(user, {id,followers=[],following=[],tweet=[],mentions=[],status=false}). 

start(NumNodes,NumTweets)->
    io:format("Num Nodes: ~p NumTweets: ~p ~n",[NumNodes,NumTweets]),
    generateActors(1,NumNodes,self()),
    io:format("Self: ~p ~n",[self()]),
    List = createList(1, NumNodes),
    io:format("List: ~p ~n",[List]),
    simulator(List).
    % P=#user{id=1,followers=[1,2,3],following=[4,5],tweet=["abc","xyz"],mentions=["mention1","mention2"],status=true},
    
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
            io:format("Followers assigned to ~p ~n",[Uid]),
            List1=addFollowers(Uid,FollowerList,List),
            io:format("List with followers: ~p ~n",[List1]),
            % List2=addToFollowing(Uid,FollowerList,List1),
            simulator(List1)
    end.


helper(Elem,List,Uid)->
    P=nth(Elem,List),
    TempList=P#user.following,
    P1 = P#user{following = lists:append(TempList,[Uid])},
    replacenth(List,Elem,P1).

helper2()->
    io:format("Dummy print: ~n",[]).

addToFollowing(Uid,FollowerList,List)->
    Function =  fun(Elem) -> 
                    % helper(Elem,List,Uid)
                    helper2()
                end,
    lists:foreach(Function,FollowerList).

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

generateActors(S,E,PPid)-> %E is numNodes
    case S=<E of
        true->
            spawn(client,initiateUser,[S,PPid,E]),
            generateActors(S+1,E,PPid);
        false->
            io:format("All actors generated ~n")
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
