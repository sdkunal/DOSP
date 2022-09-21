-module(bitcoin).
-import(string,[len/1,substr/3]).
-export([sha/2]).

sha(Str,Zeros)->
    EncrytedStr = [element(C+1, {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$A,$B,$C,$D,$E,$F}) || <<C:4>> <= crypto:hash(sha256,Str)],
    io:fwrite(integer_to_list(Zeros)++"\t"++Str++"\t"++EncrytedStr++"\n"),
    Str2 = substr(EncrytedStr,1,Zeros),
    Str2 =:= lists:flatten(lists:duplicate(Zeros,"0")).
%this is a comment