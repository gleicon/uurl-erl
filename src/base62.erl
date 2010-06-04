%% @author gleicon <gleicon@gmail.com>
%% @copyright 2010 gleicon.

%% @doc Web server for uurl.


-module(base62).
-author('gleicon <gleicon@gmail.com>').
-export([encode/1, decode/1]).

nthchar(N) when N =< 9 -> $0 + N;
nthchar(N) when N =< 35 -> $A +N - 10;
nthchar(N) -> $a + N - 36.


encode(Id) -> encode(Id, []).
encode(Id, Acc) when Id < 0 -> encode(-Id, Acc); 
encode(Id, []) when Id =:= 0 -> "0";
encode(Id, Acc) when Id =:= 0 -> Acc;
encode(Id, Acc) ->
	R = Id rem 62,
	Id1 = Id div 62,
	Ac1 = [nthchar(R)|Acc],
	encode(Id1, Ac1).

decode(S) -> decode(S, 0).
decode([C|Cs], Acc)  when C >= $0, C =< $9 -> decode(Cs, 62 * Acc + (C - $0));
decode([C|Cs], Acc)  when C >= $A, C =< $Z -> decode(Cs, 62 * Acc + (C - $A + 10));
decode([C|Cs], Acc)  when C >= $a, C =< $z -> decode(Cs, 62 * Acc + (C - $a + 36));
decode([], Acc) -> Acc.

	