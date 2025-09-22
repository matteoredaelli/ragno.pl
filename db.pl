% -*- Mode: Prolog -*-
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

:- module(db,
[
    merge_assocs/3,
    delete/1,
    open/1,
    put/2,
    get/2,
    merge/2
]).

:- use_module(library(http/json)).
:- use_module(library(rocksdb)).
:- use_module(library(pcre)).

open(DBname):-
  rocks_open(DBname,
             _DB, 
             [alias(mdb), 
              merge(merge),
              value(string)]).

delete(K):-
%  fast_term_serialized(V, Vserialized),
  rocks_delete(mdb, K).

get(K,V):-
    rocks_get(mdb, K, Json),
    atom_json_dict(Json, V, []).

put(K,V):-
    atom_json_dict(Json, V, []),
%  fast_term_serialized(V, Vserialized),
    rocks_put(mdb, K, Json).

merge(K,V):-
%  fast_term_serialized(V, Vserialized),
  rocks_merge(mdb, K, V).

merge(partial, _Key, Left, Right, Result) :-
	debug(merge, 'Merge partial ~p ~p', [Left, Right]),
	ord_union(Left, Right, Result).
merge(full, _Key, Initial, Additions, Result) :-
	debug(merge, 'Merge full ~p ~p', [Initial, Additions]),
	append([Initial|Additions], List),
	sort(List, Result).

merge_assoc(partial, _Key, Left, Right, Result) :-
	debug(merge, 'Merge partial ~p ~p', [Left, Right]),
	merge_assocs(Left, Right, Result).
merge_assoc(full, _Key, Initial, Additions, Result) :-
	debug(merge, 'Merge full ~p ~p', [Initial, Additions]),
	merge_assocs(Initial, Additions, Result).

merge_assocs(Assoc1, Assoc2, Merged) :-
    assoc_to_list(Assoc2, List2),
    foldl(add_pair, List2, Assoc1, Merged).

add_pair(Key-Value, AssocIn, AssocOut) :-
    put_assoc(Key, AssocIn, Value, AssocOut).


merge_dict(partial, _Key, Left, Right, Result) :-
	debug(merge, 'Merge partial ~p ~p', [Left, Right]),
	put_dict(Left, Right, Result).
merge_dict(full, _Key, Initial, Additions, Result) :-
	debug(merge, 'Merge full ~p ~p', [Initial, Additions]),
	put_dict(Additions, Initial, Result).

enum(K,V):-
  rocks_enum(mdb, K, Json),
  atom_json_dict(Json, V, []).

all_records(Pairs):-
  findall(K-V, enum(K, V), Pairs).
