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
              full_scan_domain_for_new_domains/0,
              full_scan_url_for_new_domains/0,
              dump/1,
              dump_json/1,
              full_scan_todo_and_crawl/1
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
                  domain:Domain
    },
    db:put(domain, Domain, Data).

add_new_domain_if_missing(Domain):-
    once(
        db:get(domain, Domain, _Data) ;
        add_new_domain(Domain)).

add_new_domains_if_missing(Domains):-
    config:skip_domains(RegexList),
    uri_ext:exclude_domains(Domains, RegexList, FilteredDomains),
    maplist(add_new_domain_if_missing, FilteredDomains).

scan_domain_for_new_domains:-
    db:enum(domain, Domain, Data),
    done == Data.ragno_status,
    FinalDomain = Data.final_domain,
    once(Domain == FinalDomain ;
         %    config:threadpool_size(S),
%    (S > 0 -> 
%        threadpool:submit_task(ragnopool, ragnodb:add_new_domains_if_missing(Domains)) ;
         add_new_domains_if_missing([FinalDomain])).

full_scan_domain_for_new_domains:-
    findall(_, scan_domain_for_new_domains, _).

scan_url_for_new_domains:-
    db:enum(url, _, Data),
    done == Data.ragno_status,
    Domains = Data.domains,
    add_new_domains_if_missing(Domains).

full_scan_url_for_new_domains:-
    findall(_, scan_url_for_new_domains, _).

scan_todo_and_crawl(DBname):-
    db:enum(DBname, Object, Data),
    todo == Data.ragno_status,
    %%format("Submitting domain ~q\n", [Domain]),
%    crawler:crawl_domain(Domain).
    once(
        (DBname == domain,
         threadpool:submit_task(ragnopool, crawler:crawler:crawl_domain(Object))) ;
        (DBname == url,
         threadpool:submit_task(ragnopool, crawler:crawler:crawl_url(Object, Data, _)))).

full_scan_todo_and_crawl(DBname):-
    findall(_, scan_todo_and_crawl(DBname), _),
    threadpool:pool_status(ragnopool),
    sleep(10000).

dump(DBname):-
    findall(_,
            (db:enum(DBname, _K, V), format("~q\n", [V])),
            _).

enum_json(DBname):-
    db:enum(DBname, _K, V),
    %    "error" \= Data.ragno_status,
%    VwithK = V.put(_{domain: K}),
    atom_json_dict(Json, V, []),
    format("~w\n", [Json]).

dump_json(DBname):-
    findall(_, enum_json(DBname), _).
