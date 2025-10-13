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
    once(ragnocli(Argv)).
%    threadpool:shutdown_thread_pool.

ragnocli([crawl, domains|Argv]):-
    db:open(domains, [mode(read_write)]),
    db:open(urls, [mode(read_write)]),
    crawler:crawl_domains(Argv).

ragnocli([crawl, urls|Argv]):-
    db:open(urls, [mode(read_write)]),
    crawler:crawl_urls(Argv).

ragnocli([dump_json, DBname|_Argv]):-
    db:open(DBname, [mode(read_only)]),
    ragnodb:dump_json(DBname).

ragnocli([dump, DBname|_Argv]):-
    db:open(DBname, [mode(read_only)]),
    ragnodb:dump(DBname).

ragnocli([find_domains|_Argv]):-
    db:open(domains, [mode(read_write)]),
    db:open(urls, [mode(read_write)]),
    ragnodb:full_scan_domain_for_new_domains,
    ragnodb:full_scan_url_for_new_domains.

ragnocli([run, ObjectType|_Argv]):-
    config:threadpool_size(S),
    once(S =< 0 ; threadpool:start_pool(ragnopool, S)),
    db:open(domains, [mode(read_write)]),
    db:open(urls, [mode(read_write)]),
    ragnodb:full_scan_todo_and_crawl(ObjectType).

ragnocli(_):-
    writeln(["ragnocli crawl|dump|find_domains|run"]).
