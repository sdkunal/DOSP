-module(client).
-import(server, [start/2]).
-export([initiateUser/3, startSimulation/3, generateRandomTweets/3, generateTweet/5]).

initiateUser(Uid, PPid, NumNodes) ->
    PPid ! {makeOnline, Uid, self()},
    assignFollowers(Uid, PPid, NumNodes).

startSimulation(Actor_List, PPid, S) ->
    Ops_List = ["tweet", "go_offline", "go_online", "follow"],
    % io:format("starting simulation: ~n", []),
    case S =< 10 of
        true ->
            RandomUid = rand:uniform(length(Actor_List)),
            Op_idx = rand:uniform(4),
            Operation = lists:nth(Op_idx, Ops_List),
            io:format("Operation selected: ~p ~n", [Operation]),
            case Operation == "tweet" of
                true ->
                    io:format("Make tweets~n", []);
                false ->
                    case Operation == "go_offline" of
                        true ->
                            goOffline(PPid, RandomUid);
                        false ->
                            case Operation == "go_online" of
                                true ->
                                    goOnline(PPid, RandomUid);
                                false ->
                                    case Operation == "follow" of
                                        true ->
                                            io:format("Follow people~n", []);
                                        false ->
                                            ""
                                    end
                            end
                    end
            end,
            startSimulation(Actor_List, PPid, S + 1);
        false ->
            ""
    end.

goOffline(PPid, Uid) ->
    PPid ! {go_offline, Uid}.

goOnline(PPid, Uid) ->
    PPid ! {go_online, Uid}.

assignFollowers(Uid, PPid, NumNodes) ->
    NumFollowers = rand:uniform(NumNodes - 2) + 1,
    List = [rand:uniform(NumNodes) || _ <- lists:seq(1, NumFollowers)],
    List2 = checkList(List, Uid),
    Set = ordsets:from_list(List2),
    FollowerList = ordsets:to_list(Set),
    PPid ! {setFollowers, Uid, FollowerList}.

generateRandomTweets(Uid, PPid, NumNodes) ->
    WordList = ["one", "life", "one", "love", "one", "chance", "covfefe"],
    NumWords = rand:uniform(7),
    List = [rand:uniform(NumWords) || _ <- lists:seq(1, NumWords)],
    Length = length(List),
    String = generateTweet(WordList, "", 1, Length, List),
    io:format("Generated String: ~p ~n", [String]).

generateTweet(WordList, String, S, Length, List) ->
    case S =< Length of
        true ->
            Curr = lists:nth(S, List),
            Str = lists:nth(Curr, WordList),
            String2 = string:concat(String, Str),
            String3 = string:concat(String2, " "),
            generateTweet(WordList, String3, S + 1, Length, List);
        false ->
            String
    end.

checkList(List, Uid) ->
    Bool = lists:member(Uid, List),
    case Bool of
        true ->
            lists:delete(Uid, List);
        false ->
            List
    end.
