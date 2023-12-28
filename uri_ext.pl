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
	      domain_uri/2,
	      domain_uris/2,
	      domain_details/3,
	      http_uri/1,
	      uri_without_fragment/2
	  ]).

:- use_module(library(uri)).
:- use_module(library(apply)).


domain_uri(Uri, RootUri):-
    uri_normalized("/", Uri, DirtyRootUri),
    %% Sometimes I see %20 inside the domain name
    re_replace("%20"/g, "", DirtyRootUri, RootUri).

domain_uris(Links, RootLinks):-
    convlist([Uri, RootUri]>>domain_uri(Uri, RootUri), Links, RootUris),
    setof(X, member(X,RootUris), RootLinks).    

is_internal_link(Domain, Link):-
    uri_components(Link,  uri_components(_Schema, Domain, _, _, _)).

is_external_link(Domain, Link):-
    \+ is_internal_link(Domain, Link).

filter_external_links(Links, Domain, ExternalLinks):-
    include(is_external_link(Domain), Links, ExternalLinks).

uri_without_fragment(Url1, Url2):-
    uri_components(Url1, uri_components(Schema, DomainPort, Path, Params, _Fragment)),
    uri_components(Url2, uri_components(Schema, DomainPort, Path, Params, _)).

domain_details(Domain, DomainList, Level):-
    atom_string(Domain, DomainString),
    split_string(DomainString, ".", "", DomainList),
    length(DomainList, Level).

/*
level2_domain(+Domain1, -Domain2)

convert a domain like 'www.redaelli.org' to 'redaelli.org'

*/
level2_domain(Domain1, Domain2):-
    domain_details(Domain1, DomainList, _Level),
    reverse(DomainList, [D1,D2|_L]), 
    atomic_list_concat([D2, '.', D1], Domain2).

level2_domains(Domain1List, Domain2Set):-
    convlist([Domain1, Domain2]>>level2_domain(Domain1, Domain2), Domain1List, Domain2List),
    setof(X, member(X,Domain2List), Domain2Set).    

http_uri(Url):-
    uri_components(Url, uri_components(https, _DomainPort, _Path, _Params, _)) ;
    uri_components(Url, uri_components(http, _DomainPort, _Path, _Params, _)).
