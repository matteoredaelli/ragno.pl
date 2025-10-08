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

:- use_module(crawler).
:- use_module(ragnodb).
:- use_module(config).
:- use_module(db).
:- use_module(threadpool).

:- initialization(main, main).

main(Argv):-
    config:dbname(DBname),
    config:threadpool_size(S),
    once(S =< 0 ; threadpool:start_pool(ragnopool, S)),
    once(ragnocli(DBname, Argv)).
%    threadpool:shutdown_thread_pool.

ragnocli(DBname, [crawl|Argv]):-
    db:open(DBname, [mode(read_write)]),
    crawler:crawl_domains(Argv, _).

ragnocli(DBname, [find_domains|_Argv]):-
    db:open(DBname, [mode(read_write)]),
    ragnodb:full_scan_add_new_domains.

ragnocli(DBname, [domains|_Argv]):-
    db:open(DBname, [mode(read_only)]),
    ragnodb:full_scan_domain_name.

ragnocli(DBname, [run|_Argv]):-
    db:open(DBname, [mode(read_write)]),
    ragnodb:full_scan_todo_domains_and_crawl.

ragnocli(_DBname, []):-
    writeln(["ragnocli crawl|domains|find_domains|run"]).
