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
    open/1,
    put/2,
    get/2,
    merge/2
]).

:- use_module(library(rocksdb)).

open(DBname):-
  rocks_open(DBname,
             _DB, 
             [alias(mdb), merge(merge),value(term)]).

get(K,V):-
  rocks_get(mdb, K, Vserialized),
  fast_term_serialized(V, Vserialized).

put(K,V):-
  fast_term_serialized(V, Vserialized),
  rocks_put(mdb, K, Vserialized).

merge(K,V):-
  fast_term_serialized(V, Vserialized),
  rocks_merge(mdb, K, Vserialized).

enum(K,V):-
  rocks_enum(mdb, K, Vserialized),
  fast_term_serialized(V, Vserialized).

all_records(Pairs):-
  findall(K-V, enum(K, V), Pairs).
