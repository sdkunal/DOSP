-module(client).
-import(server,[start/2]).
-export([initiateUser/3]).

initiateUser(Uid,PPid,NumNodes)->
    PPid ! {makeOnline,Uid,self()},
    assignFollowers(Uid,PPid,NumNodes).
    % generateRandomTweets(Uid,PPid,NumNodes).

assignFollowers(Uid,PPid,NumNodes)->
    NumFollowers=rand:uniform(NumNodes-2)+1,
    List=[rand:uniform(NumNodes) || _ <- lists:seq(1, NumFollowers)],
    List2=checkList(List,Uid),
    Set=ordsets:from_list(List2),
    FollowerList=ordsets:to_list(Set),
    PPid ! {setFollowers,Uid,FollowerList}.

generateRandomTweets(Uid,PPid,NumNodes)->
    WordList=["one","life","one","love","one","chance","covfefe"],
    NumWords=rand:uniform(7),
    List=[rand:uniform(NumWords) || _ <- lists:seq(1, NumWords)],
    Length=length(List),
    String=generateTweet(WordList,"",1,Length,List),
    io:format("Generated String: ~p ~n",[String]).

generateTweet(WordList,String,S,Length,List)->
    case S=<Length of
        true->
            Curr=lists:nth(S,List),
            Str=lists:nth(Curr,WordList),
            String2=string:concat(String,Str),
            String3=string:concat(String2," "),
            generateTweet(WordList,String3,S+1,Length,List);
        false->
            String
    end.


checkList(List,Uid)->
    Bool=lists:member(Uid,List),
    case Bool of
        true->
            lists:delete(Uid,List); 
        false->
            List
    end.


