-module(client).
-import(server, [start/2]).
-export([initiateUser/3, startSimulation/3, generateRandomTweets/3, generateTweet/5]).

initiateUser(Uid, PPid, NumNodes) ->
    PPid ! {makeOnline, Uid, self()},
    assignFollowers(Uid, PPid, NumNodes).

startSimulation(Actor_List, PPid, S) ->
    Ops_List = [
        "tweet", "go_offline", "go_online", "follow", "query_hashtags", "get_feed", "retweet"
    ],
    HashTagList = ["tag1", "tag2", "tag3", "tag4", "tag5"],
    % io:format("starting simulation: ~n", []),
    case S =< 20 of
        true ->
            NumNodes = length(Actor_List),
            RandomUid = rand:uniform(NumNodes),
            Op_idx = rand:uniform(length(Ops_List)),
            Operation = lists:nth(Op_idx, Ops_List),
            io:format("Operation selected: ~p ~n", [Operation]),
            case Operation == "tweet" of
                true ->
                    io:format("Uid ~p is tweeting~n", [RandomUid]),
                    goOnline(PPid, RandomUid),
                    makeTweet(PPid, RandomUid, NumNodes);
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
                                            UidToFollow = rand:uniform(NumNodes),
                                            followUser(NumNodes, UidToFollow, RandomUid, PPid);
                                        false ->
                                            case Operation == "query_hashtags" of
                                                true ->
                                                    SelectHashTag = rand:uniform(5),
                                                    Hash = lists:nth(SelectHashTag, HashTagList),
                                                    query_hashtags(Hash, PPid);
                                                false ->
                                                    case Operation == "get_feed" of
                                                        true ->
                                                            get_feed(RandomUid, PPid);
                                                        false ->
                                                            case Operation == "retweet" of
                                                                true ->
                                                                    reTweet(RandomUid, PPid);
                                                                false ->
                                                                    ""
                                                            end
                                                    end
                                            end
                                    end
                            end
                    end
            end,
            startSimulation(Actor_List, PPid, S + 1);
        false ->
            ""
    end.

reTweet(Uid, PPid) ->
    PPid ! {retweet, Uid}.

followUser(NumNodes, FollowUid, Uid, PPid) ->
    case FollowUid == Uid of
        true ->
            NewUidToFollow = rand:uniform(NumNodes),
            followUser(NumNodes, NewUidToFollow, Uid, PPid);
        false ->
            PPid ! {addFollowers, FollowUid, Uid}
    end.

get_feed(Uid, PPid) ->
    PPid ! {get_feed, Uid}.

query_hashtags(Hash, PPid) ->
    PPid ! {display_hashtags, Hash}.

makeTweet(PPid, Uid, NumNodes) ->
    Tweet = generateRandomTweets(Uid, PPid, NumNodes),
    PPid ! {postTweet, Uid, Tweet}.

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
    HashTagList = ["tag1", "tag2", "tag3", "tag4", "tag5"],
    ChooseHashTag = rand:uniform(2),
    NumWords = rand:uniform(7),
    List = [rand:uniform(NumWords) || _ <- lists:seq(1, NumWords)],
    Length = length(List),
    String = generateTweet(WordList, "", 1, Length, List),
    case ChooseHashTag == 1 of
        true ->
            SelectHashTag = rand:uniform(5),
            Hash = lists:nth(SelectHashTag, HashTagList),
            HashTag = string:concat("#", Hash),
            String2 = string:concat(String, HashTag),
            String2;
        false ->
            String
    end.

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
