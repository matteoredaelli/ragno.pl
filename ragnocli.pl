% -*- Mode: Prolog -*-

/*
    ragno: a light spider written in (swi)prolog for crawling root web sites

    Copyright (C) 2022-2023  Matteo Redaelli

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

:- use_module(ragno).
:- use_module(ragnodb).
:- use_module(config).
:- use_module(db).

:- initialization(main, main).

main(Argv):-
    config:dbname(DBname),
    db:open(DBname),
    once(ragnocli(Argv)).

ragnocli([crawl|Argv]):-
    ragno:crawl_domains(Argv,_).

ragnocli([new_domains|_Argv]):-
    ragnodb:find_and_add_new_domains().

ragnocli([run|_Argv]):-
    ragnodb:find_todo_domains_and_crawl().

ragnocli(_):-
    writeln(["ragnocli crawl|run"]).
