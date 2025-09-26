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

:- module(uri_ext, [
	      uri_domain/2,
	      uris_domains/2,
	      level2_domain/2,
	      is_http_uri/1,
	      remove_www/2,
	      www_without_numbers/2,
	      split_links/4,
	      uri_without_fragment/2
	  ]).

:- use_module(library(uri)).
:- use_module(library(apply)).


/*
* add / at the end or remove paths from url
*/
uri_domain(Uri, Domain):-
    uri_components(Uri, uri_components(_, Domain, _, _, _)).

uris_domains(Uris, Domains):-
    convlist([Uri, Domain]>>uri_domain(Uri, Domain), Uris, DomainsList),
    setof(X, member(X,DomainsList), Domains).    

same_domain(Uri1, Uri2):-
    uri_domain(Uri1, Domain),
    uri_domain(Uri2, Domain).

same_level2_domain(Uri1, Uri2):-
    uri_domain(Uri1, Domain1),
    uri_domain(Uri2, Domain2),
    level2_domain(Domain1, Domain),
    level2_domain(Domain2, Domain).

same_domain_link(Uri, Domain):-
    uri_domain(Uri, DomainAtom),
    atom_string(DomainAtom, Domain).

is_internal_link(Uri, Domain):-
    uri_domain(Uri, UriDomain),
    level2_domain(UriDomain, Domain).

is_external_link(Uri, Domain):-
    \+ is_internal_link(Uri, Domain).

uri_without_fragment(Url1, Url2):-
    uri_components(Url1, uri_components(Schema, DomainPort, Path, Params, _Fragment)),
    uri_components(Url2, uri_components(Schema, DomainPort, Path, Params, _)).

/*
level2_domain(+Domain1, -Domain2)

convert a domain like 'www.redaelli.org' to 'redaelli.org'

*/
level2_domain(Domain, Domain2):-
    atom_string(Domain, DomainString),
    split_string(DomainString, ".", "", DomainList),
    reverse(DomainList, [D1,D2|_L]), 
    atomic_list_concat([D2, '.', D1], Domain2String),
    atom_string(Domain2, Domain2String).

level2_domains(Domain1List, Domain2Set):-
    convlist([Domain1, Domain2]>>level2_domain(Domain1, Domain2), Domain1List, Domain2List),
    setof(X, member(X,Domain2List), Domain2Set).    

is_http_uri(Url):-
    uri_components(Url, uri_components(https, _DomainPort, _Path, _Params, _)) ;
    uri_components(Url, uri_components(http, _DomainPort, _Path, _Params, _)).

split_links(Links, Domain, SameDomainLinks, ExternalLinks):-
    include([X]>> (member(X,Links), same_domain_link(X,Domain)), Links, SameDomainLinks),
    subtract(Links, SameDomainLinks, ExternalLinks).
    

www_without_numbers(FromUrl, ToUrl):-
    re_replace("^www\\d*\\.", "www.", FromUrl, ToUrl).

remove_www(FromUrl, ToUrl):-
    re_replace("^www\\d*\\.", "", FromUrl, ToUrl).
