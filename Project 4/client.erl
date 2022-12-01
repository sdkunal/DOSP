-module(client).
-import(server,[start/2]).
-export([initiateUser/3]).

initiateUser(Uid,PPid,NumNodes)->
    PPid ! {makeOnline,Uid,self()},
    assignFollowers(Uid,PPid,NumNodes).

assignFollowers(Uid,PPid,NumNodes)->
    NumFollowers=rand:uniform(NumNodes-2)+1,
    List=[rand:uniform(NumNodes) || _ <- lists:seq(1, NumFollowers)],
    List2=checkList(List,Uid),
    Set=ordsets:from_list(List2),
    FollowerList=ordsets:to_list(Set),
    PPid ! {setFollowers,Uid,FollowerList}.

checkList(List,Uid)->
    Bool=lists:member(Uid,List),
    case Bool of
        true->
            lists:delete(Uid,List); 
        false->
            List
    end.


