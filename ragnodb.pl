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

:- module(ragnodb,
          [
              full_scan_add_new_domains/0,
              dump/1,
              full_scan_todo_domains_and_crawl/0
          ]).

:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(crawler).
:- use_module(db).
%%:- use_module(threadpool).
:- use_module(uri_ext).

add_new_domain(Domain):-
    writeln(["Adding domain", Domain]),
    get_time(Timestamp),
    Data = domain{ragno_status:todo,
                  ragno_ts:Timestamp,
                  domain:Domain,
                  domains:[]},
    db:put(domain, Domain, Data).

add_new_domain_if_missing(Domain):-
    once(
        db:get(domain, Domain, _Data) ;
        add_new_domain(Domain)).

add_new_domains_if_missing(Domains):-
    config:skip_domains(RegexList),
    uri_ext:exclude_domains(Domains, RegexList, FilteredDomains),
    maplist(add_new_domain_if_missing, FilteredDomains).

scan_add_new_domains:-
    db:enum(domain, _K, Data),
    Domains = Data.domains,
    "done" == Data.ragno_status,
    %    config:threadpool_size(S),
%    (S > 0 -> 
%        threadpool:submit_task(ragnopool, ragnodb:add_new_domains_if_missing(Domains)) ;
    add_new_domains_if_missing(Domains).

full_scan_add_new_domains:-
    findall(_, scan_add_new_domains, _).

scan_todo_domain_and_crawl:-
    db:enum(domain, Domain, Data),
    "todo" == Data.ragno_status,
    %%format("Submitting domain ~q\n", [Domain]),
%    crawler:crawl_domain(Domain).
    config:threadpool_size(S),
    (S > 0 ->
         threadpool:submit_task(ragnopool, crawler:crawler:crawl_domain(Domain)) ;
     crawler:crawl_domain(Domain)).

full_scan_todo_domains_and_crawl:-
    findall(_, scan_todo_domain_and_crawl, _),
    sleep(10000).

enum_json(DBname):-
    db:enum(DBname, _K, V),
    %    "error" \= Data.ragno_status,
    atom_json_dict(Json, V, []),
    format("~w\n", [Json]).

dump(DBname):-
    findall(_, enum_json(DBname), _).
