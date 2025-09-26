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
    find_and_add_new_domains/0
]).

:- use_module(library(apply)).
:- use_module(config).
:- use_module(db).
:- use_module(uri_ext).

add_new_domain(Domain):-
    writeln(["Adding domain ", Domain]),
    get_time(Timestamp),
    Data = domain{ragno_status:todo,
                  ragno_ts:Timestamp,
                  domain:Domain,
                  domains:[]},
    db:put(Domain, Data).

add_new_domain_if_missing(Domain):-
    once(
        db:get(Domain, _Data) ;
        add_new_domain(Domain)).
        
add_new_domains_if_missing(Domains):-
    maplist(add_new_domain_if_missing, Domains).

get_domain_and_add_new_domains():-
    db:enum(_K,Data),
    WWWDomains = Data.domains,
    "done" == Data.ragno_status,
    maplist(uri_ext:remove_www, WWWDomains, Domains),
    add_new_domains_if_missing(Domains).

find_and_add_new_domains():-
    findall(_, get_domain_and_add_new_domains, _).
