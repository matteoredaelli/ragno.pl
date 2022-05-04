/*
    ragno: a light spider writtend in (swi)prolog for crawling root web sites

    Copyright (C) 2022  Matteo Redaelli

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

:- module(list_ext, [
		     append_to_lists/3,
		     atomic_lists_concat/3,
		     merge_lists/3,
		     groupN/3,
		     range/3
		    ]).

selectN(0, L, [], L).
selectN(N, [X|L],[X|L1], L2) :-
	N > 0,
	N1 is N-1,
	selectN(N1,L,L1, L2).

%%  works if list legth can be dividev by N without rest
%%groupN([],_,[]).
%%groupN(G,N,[G1|Gs]) :-
%%	selectN(N,G,G1, Rest),
%%	groupN(Rest,N,Gs).

groupN(N, List, [List]):-
	length(List, M),
	M =< N, !.
groupN(N, List, [H|T]):-
	append(H, T1, List),
	length(H, N),
	groupN(N, T1, T).

atomic_lists_concat([], _Sep, []).
atomic_lists_concat([L1|List], Sep, [S|Strings] ):-
	atomic_list_concat(L1, Sep, S),
	atomic_lists_concat(List, Sep, Strings).

list_join(List, Separator, String):-
	atomic_list_concat(List, Separator, Atom),
	atom_string(Atom, String).


append_to_lists([], _L, []).
append_to_lists([FirstList|Others], L, [NewFirstList|NewOthers]):-
	append(L, FirstList, NewFirstList),
	append_to_lists(Others, L, NewOthers).

merge_lists([],[],[]).
merge_lists([H1|T1], [H2|T2] ,[H1, H2|T]):-
	merge_lists(T1, T2, T).

range(X,X,[X]) :- !.
range(X,Y,[X|Xs]) :-
    X =< Y,
    Z is X+1,
    range(Z,Y,Xs).
