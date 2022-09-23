-module(bitcoin).
-import(string,[len/1,substr/3]).
-export([sha/2,get_random_string/2,repeat/1,start/0]).

repeat(Num) ->
    %io:fwrite(integer_to_list(Num)),
    case 
        Num>0 of 
        true ->
            sha(get_random_string(6,"abcdefghijklmnopqrstuvwxyz0123456789"),3),
            %sha(curStr,3),
            io:fwrite("\n"), 
            repeat(Num-1); 
        false ->
            ""
    end.

start() ->
    repeat(100000).



sha(Str,Zeros)->
    EncrytedStr = [element(C+1, {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$A,$B,$C,$D,$E,$F}) || <<C:4>> <= crypto:hash(sha256,Str)],
    io:fwrite(integer_to_list(Zeros)++"\t"++Str++"\t"++EncrytedStr++"\n"),
    Str2 = substr(EncrytedStr,1,Zeros),
    Str2 =:= lists:flatten(lists:duplicate(Zeros,"0")).

get_random_string(Length, AllowedChars) ->
    Str2="kdudhe:"++lists:foldl(fun(_, Acc) ->
        [lists:nth(rand:uniform(length(AllowedChars)),AllowedChars)]++ Acc end, [], lists:seq(1, Length)),
    Str2.
%this is a comment