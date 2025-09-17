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

:- module(tagger, [
  tags_from_headers/2,
  social_tag_from_links/2,
  social_tags_from_links/2,
  tags_from_values/2
]).

:- use_module(library(uri)).
:- use_module(headers_ext).
:- use_module(tags).

tags_from_headers(Headers, Tags):-
	headers_ext:headers_keys_values(Headers, Keys, Values),
	atomic_list_concat(Keys, ' ', KeysStr),
	downcase_atom(KeysStr, KeysStrLower),
	atomic_list_concat(Values, ' ', ValuesStr),
	downcase_atom(ValuesStr, ValuesStrLower),
  all_tags_from_keys(KTags, KeysStrLower),
  all_tags_from_values(VTags, ValuesStrLower),
  all_tags_from_h(HTags, Headers),
  foldl(append, [HTags, VTags, KTags], [], Tags).

tags_from_keys(Tags, String):-
	tags:key_tags(Key, Tags),
	sub_string(String, _, _, _, Key).

tags_from_values(Tags, String):-
	tags:value_tags(Value, Tags),
	sub_string(String, _, _, _, Value).

tags_from_h(Tags, Headers):-
  member(H, Headers),
	tags:header_tags(H, Tags).

all_tags_from_h(Tags, Headers):-
	findall(T, tags_from_h(T, Headers), ListTags),
	flatten(ListTags, Tags).

all_tags_from_keys(Tags, String):-
	findall(T, tags_from_keys(T, String), ListTags),
	flatten(ListTags, Tags).

all_tags_from_values(Tags, String):-
	findall(T, tags_from_values(T, String), ListTags),
	flatten(ListTags, Tags).

social_tag_from_links(Tag, Links):-
  member(L, Links),
	tags:social_tag(SocialName, Regex),
	re_matchsub(Regex, L, Dict, []),
  User = Dict.user,
	Tag =.. [SocialName, User].

social_tags_from_links(Tags, Links):-
	findall(T, social_tag_from_links(T, Links), Tags).
