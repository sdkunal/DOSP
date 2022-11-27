-module(server).
-import(client,[initiateUser/1]).
-export([start/2,generateActors/2]).
-record(user, {id,followers = [],following=[],tweet=[],mentions=[],status}). 

start(NumNodes,NumTweets)->
    io:format("Num Nodes: ~p NumTweets: ~p ~n",[NumNodes,NumTweets]),
    % generateActors(1,NumNodes).
    P=#user{id=1,followers=[1,2,3],following=[4,5],tweet=["abc","xyz"],mentions=["mention1","mention2"],status=true},
    io:fwrite("~p~n",[P#user.id]),
    io:fwrite("~p~n",[P#user.followers]),
    io:fwrite("~p~n",[P#user.following]),
    io:fwrite("~p~n",[P#user.tweet]),
    io:fwrite("~p~n",[P#user.mentions]),
    io:fwrite("~p~n",[P#user.status]).

generateActors(S,E)-> %E is numNodes
    case S=<E of
        true->
            spawn(client,initiateUser,[S]),
            generateActors(S+1,E);
        false->
            io:format("All actors generated ~n")
    end.


% actions list [tweet, retweet, follow, query hashtags, query mentions, query followed tweets]
% offline, online actions
% maintain status for each actor -> save data effectively for every actor -> 
% assign IDs to user,manage PID and UID

% what to store for an user
% followers -> list will suffice
% following -> list
% tweet list -> list
% mentions list -> list
% status -> boolean


%schema

%UID - primary key



%struct -> boolean, list, list, list, list 
%[]

% universal storage for hashtags

