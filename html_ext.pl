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

:- module(html_ext, [
	      safe_extract_all_links/3,
	      extract_table_to_list_of_list/2,
	      find_table/3
	  ]).

:-use_module(library(xpath)).
:-use_module(library(uri)).
:-use_module(list_ext).

extract_value_from_dom(element(_, _, [Value]), Value).

findall_table_th(DOM, Texts):-
	findall(Texts, xpath(DOM, //tr/th(text), Texts), Texts).

findall_table_td(DOM, Texts):-
	findall(Texts, xpath(DOM, //td(text), Texts), Texts).

extract_table_to_list_of_list(DOM, [Header, ListOfRowLists]):-
	findall_table_th(DOM, Header),
	length(Header, N),
	findall_table_td(DOM, FlatList),
	list_ext:groupN(N, FlatList, ListOfRowLists).

find_table(DOM, N, Table):-
	xpath(DOM, //table(N), Table).

extract_link(DOM, Url, Link):-
    xpath(DOM, //a(@href=HREF0, text), _Title),
    uri_normalized(HREF0, Url, LinkWithFragment),
    uri_without_fragment(LinkWithFragment, Link).

extract_all_links(DOM, Url, Links):-
    setof(Link, extract_link(DOM, Url, Link), Links).

safe_extract_all_links(DOM, Url, Links):-
    catch(extract_all_links(DOM, Url, Links),
			ExTerm, (format("Exception: ~q\n",[ExTerm]), Links is [])).

safe_extract_title(DOM, Title):-
    xpath(DOM, //head/title(text), Title) ; Title is "".

