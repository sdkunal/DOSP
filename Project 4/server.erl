-module(server).
-import(client, [initiateUser/3, startSimulation/3, generateRandomTweets/3, generateTweet/5]).
-import(lists, [nth/2, append/1]).
-export([
    start/2,
    generateActors/4,
    createList/2,
    createNewList/3,
    simulator/2,
    takeOnline/2,
    replacenth/3,
    takeOffline/2
]).
-record(user, {
    id, followers = [], following = [], tweet = [], feed = [], mentions = [], status = false
}).

start(NumNodes, NumTweets) ->
    io:format("Num Nodes: ~p NumTweets: ~p ~n", [NumNodes, NumTweets]),
    % Actor_List=generateActors(1,NumNodes,self()),
    Actor_List = generate(1, NumNodes, self()),
    io:format("Actor_List: ~p ~n", [Actor_List]),
    % io:format("Self: ~p ~n",[self()]),
    Record_List = createList(1, NumNodes),
    HashTags = #{"tag1" => [], "tag2" => [], "tag3" => [], "tag4" => [], "tag5" => []},
    spawn(client, startSimulation, [Actor_List, self(), 1]),
    % io:format("List: ~p ~n",[List]),
    simulator(Record_List, HashTags).
% P=#user{id=1,followers=[1,2,3],following=[4,5],tweet=["abc","xyz"],mentions=["mention1","mention2"],status=true},

generate(S, E, PPid) ->
    generateActors(S, E, [], PPid).

simulator(List, HashTags) ->
    receive
        {addFollowers, FollowUid, Uid} ->
            % io:format("in simulator: ~p ~n", [self()]),
            P = nth(Uid, List),
            FollowerList = P#user.followers,
            CheckMember = lists:member(FollowUid, FollowerList),
            case CheckMember == false of
                true ->
                    % io:format("List before adding new followers: ~p ~n", [List]),
                    List1 = addNewFollower(Uid, FollowUid, List),
                    List2 = addNewFollowing(Uid, FollowUid, List1),
                    % io:format("List after adding new followers and following: ~p ~n", [List2]),
                    simulator(List2, HashTags);
                false ->
                    simulator(List, HashTags)
            end;
        {makeOnline, Uid, Pid} ->
            io:format("Actor ~p is in online state with PID ~p ~n", [Uid, Pid]),
            List1 = takeOnline(List, Uid),
            simulator(List1, HashTags);
        {setFollowers, Uid, FollowerList} ->
            io:format("Followers ~p assigned to ~p ~n", [FollowerList, Uid]),
            List1 = addFollowers(Uid, FollowerList, List),
            Length = length(FollowerList),
            List2 = addToFollowing(Uid, FollowerList, List1, 1, Length),
            %io:format("Final List: ~p ~n",[List2]),
            simulator(List2, HashTags);
        {go_offline, Uid} ->
            List1 = takeOffline(List, Uid),
            % io:format("After offline List: ~p ~n", [List1]),
            simulator(List1, HashTags);
        {go_online, Uid} ->
            List1 = setOnline(List, Uid),
            % io:format("After offline List: ~p ~n", [List1]),
            simulator(List1, HashTags);
        {postTweet, Uid, Tweet} ->
            List1 = postTweet(List, Uid, Tweet),
            P = nth(Uid, List1),
            FollowerList = P#user.followers,
            Length = length(FollowerList),
            List2 = distributeTweet(List1, Uid, Tweet, FollowerList, 1, Length),
            ContainsHashTag = string:chr(Tweet, $#),
            case ContainsHashTag =/= 0 of
                true ->
                    Start = ContainsHashTag + 1,
                    End = ContainsHashTag + 4,
                    Tag = string:substr(Tweet, Start, End),
                    TagList = maps:get(Tag, HashTags),
                    TagList2 = lists:append(TagList, [Tweet]),
                    Map2 = maps:put(Tag, TagList2, HashTags),
                    io:format("Hashtags after update: ~p ~n", [Map2]),
                    simulator(List2, Map2);
                false ->
                    io:format("Hashtags without update: ~p ~n", [HashTags]),
                    simulator(List2, HashTags)
            end;
        {display_hashtags, Hash} ->
            TweetList = maps:get(Hash, HashTags),
            io:format("Tweets with ~p hashtags: ~p ~n", [Hash, TweetList]),
            simulator(List, HashTags);
        {get_feed, Uid} ->
            Feed = displayFeed(Uid, List),
            io:format("Displaying feed for ~p user: ~p~n", [Uid, Feed]),
            simulator(List, HashTags);
        {retweet, Uid} ->
            P = nth(Uid, List),
            FeedList = P#user.feed,
            FeedListLength = length(FeedList),
            case FeedListLength =/= 0 of
                true ->
                    RandTweetIdx = rand:uniform(FeedListLength),
                    RandTweet = nth(RandTweetIdx, FeedList),
                    List1 = reTweeting(Uid, List, RandTweet),
                    FollowerList = P#user.followers,
                    Length = length(FollowerList),
                    List2 = distributeTweet(List1, Uid, RandTweet, FollowerList, 1, Length),
                    ContainsHashTag = string:chr(RandTweet, $#),
                    case ContainsHashTag =/= 0 of
                        true ->
                            Start = ContainsHashTag + 1,
                            End = ContainsHashTag + 4,
                            Tag = string:substr(RandTweet, Start, End),
                            TagList = maps:get(Tag, HashTags),
                            TagList2 = lists:append(TagList, [RandTweet]),
                            Map2 = maps:put(Tag, TagList2, HashTags),
                            io:format("Hashtags after update: ~p ~n", [Map2]),
                            simulator(List2, Map2);
                        false ->
                            io:format("Hashtags without update: ~p ~n", [HashTags]),
                            simulator(List2, HashTags)
                    end;
                false ->
                    io:format("No tweets in feed to retweet~n", []),
                    simulator(List, HashTags)
            end
    end.

reTweeting(Uid, List, RandTweet) ->
    P = nth(Uid, List),
    TweetList = P#user.tweet,
    P1 = P#user{tweet = lists:append(TweetList, [RandTweet])},
    replacenth(List, Uid, P1).

addNewFollowing(Uid, FollowUid, List) ->
    P = nth(FollowUid, List),
    FollowingList = P#user.following,
    P1 = P#user{following = lists:append(FollowingList, [Uid])},
    replacenth(List, FollowUid, P1).

addNewFollower(Uid, FollowUid, List) ->
    P = nth(Uid, List),
    FollowerList = P#user.followers,
    P1 = P#user{followers = lists:append(FollowerList, [FollowUid])},
    replacenth(List, Uid, P1).

displayFeed(Uid, List) ->
    P = nth(Uid, List),
    P#user.feed.

distributeTweet(List, Uid, Tweet, FollowerList, S, Length) ->
    case S =< Length of
        true ->
            Curr = nth(S, FollowerList),
            P = nth(Curr, List),
            FeedList = P#user.feed,
            P1 = P#user{feed = lists:append(FeedList, [Tweet])},
            List2 = replacenth(List, Curr, P1),
            distributeTweet(List2, Uid, Tweet, FollowerList, S + 1, Length);
        false ->
            List
    end.

postTweet(List, Uid, Tweet) ->
    P = nth(Uid, List),
    TweetList = P#user.tweet,
    P1 = P#user{tweet = lists:append(TweetList, [Tweet])},
    replacenth(List, Uid, P1).

takeOffline(List, Uid) ->
    P = lists:nth(Uid, List),
    Bool = P#user.status,
    case Bool == true of
        true ->
            P1 = P#user{status = false},
            replacenth(List, Uid, P1);
        false ->
            io:format("User was already offline~n", []),
            List
    end.

setOnline(List, Uid) ->
    P = lists:nth(Uid, List),
    Bool = P#user.status,
    case Bool == false of
        true ->
            P1 = P#user{status = true},
            replacenth(List, Uid, P1);
        false ->
            io:format("User was already online~n", []),
            List
    end.

addToFollowing(Uid, FollowerList, List, S, Length) ->
    case S =< Length of
        true ->
            Curr = nth(S, FollowerList),
            P = nth(Curr, List),
            TempList = P#user.following,
            P1 = P#user{following = lists:append(TempList, [Uid])},
            List2 = replacenth(List, Curr, P1),
            addToFollowing(Uid, FollowerList, List2, S + 1, Length);
        false ->
            List
    end.

addFollowers(Uid, FollowerList, List) ->
    P = nth(Uid, List),
    P1 = P#user{followers = FollowerList},
    replacenth(List, Uid, P1).

takeOnline(List, Uid) ->
    P = nth(Uid, List),
    P1 = P#user{status = true},
    replacenth(List, Uid, P1).

replacenth(L, Index, NewValue) ->
    {L1, [_ | L2]} = lists:split(Index - 1, L),
    L1 ++ [NewValue | L2].

%E is numNodes
generateActors(S, E, L, PPid) ->
    case S =< E of
        true ->
            % spawn(client,initiateUser,[S,PPid,E]),
            generateActors(
                S + 1, E, lists:append([L, [spawn(client, initiateUser, [S, PPid, E])]]), PPid
            );
        false ->
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
                lists:append([
                    L,
                    [
                        #user{
                            id = S,
                            followers = [],
                            following = [],
                            tweet = [],
                            feed = [],
                            mentions = [],
                            status = false
                        }
                    ]
                ])
            );
        false ->
            L
    end.
