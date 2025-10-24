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

ragnocli([crawl, domains_by_name, Name]):-
    config:threadpool_size(S),
    once(S =< 0 ; threadpool:start_pool(ragnopool, S)),
    db:open(domains, [mode(read_write)]),
    db:open(urls, [mode(read_write)]),
    crawler:crawl_domains_by_name(Name),
    sleep(20).

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

ragnocli([pool|Argv]):-
    db:open(domains, [mode(read_write)]),
    db:open(urls, [mode(read_write)]),
    config:threadpool_size(S),
    threadpool:start_pool(ragnopool, S),
    once(ragnocli_pool(Argv)),
    sleep(7200).

ragnocli(_):-
    writeln(["ragno crawl|dump|run"]),
    writeln(["ragno pool run domains"]),
    writeln(["ragno pool run domains|urls todo"]).

ragnocli_pool([run]):-
    writeln(["ragno run"]),
    ragnodb:full_scan_domains_and_crawl_new_domains,
    ragnodb:full_scan_todo_and_crawl(domains),
    ragnodb:full_scan_todo_and_crawl(urls),
    ragnodb:full_scan_urls_and_crawl_new_domains,
    sleep(5),
    ragnocli_pool([run]).

ragnocli_pool([run, ObjectType, todo]):-
    ragnodb:full_scan_todo_and_crawl(ObjectType).
