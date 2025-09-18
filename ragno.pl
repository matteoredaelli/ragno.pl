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

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(xpath)).

:- use_module(library(uri)).
:- use_module(library(apply)).

:- use_module(html_ext).
:- use_module(config).
:- use_module(db).
:- use_module(tagger).
:- use_module(uri_ext).

:- initialization(main, main).

main(Argv):-
    config:dbname(DBname),
    db:open(DBname),
    crawl_domains(Argv).

safe_make_directory(Directory):-
    catch(make_directory(Directory),
	  _ExTerm,
	  %%format("WARNING: ~q\n",[ExTerm])
	  true
	 ).

save_uri_to_jsonfile(Uri, DomainDict):-
    %uri_components(Uri, uri_components(_Schema, Domain, _, _, _)),
    re_replace("/"/g, "", Uri, Filename),
    atomic_list_concat(['data/', Filename, '.json'], FullPathFilename),
    open(FullPathFilename, write, Fd),
    json_write_dict(Fd, DomainDict, [serialize_unknown(true)]),
    close(Fd).

ragno_http_options(MyOptions, HttpOptions):-
    %% [final_url(FinalUrl), headers(Headers)]
    %% collapse_options(MergedHttpOptions, FinalHttpOptions),
    config:http_options(HttpDefaultOptions),
    merge_options(MyOptions, HttpDefaultOptions, HttpOptions).

get_http_page_info(Url, FinalUrl, Headers):-
    ragno_http_options([method(get), final_url(FinalUrl), headers(Headers)], HttpOptions),
    http_open(Url, In, HttpOptions),
    close(In).

get_html_page(Url, FinalUrl, Headers, DOM):-
    ragno_http_options([method(get), final_url(FinalUrl), headers(Headers)], HttpOptions),
    setup_call_cleanup(
	    http_open(Url, In, HttpOptions),
	    load_html(In, DOM, []),
        close(In)).

%% get_final_url(Url, FinalUrl):-
%%     ragno_http_options([final_url(FinalUrl)], HttpOptions),
%%     http_open(Url, In, HttpOptions),
%%     close(In).
%%
%% safe_get_final_url(Url, FinalUrl, Err):-
%%     catch((get_final_url(Url, FinalUrl), Err = none),
%% 	  Err,
%% 	  FinalUrl = none).

crawl_html_page(Url, FinalUrl, Headers, HttpLinks):-
    get_html_page(Url, FinalUrl, AllHeaders, DOM),
    removed_http_headers(HeadersToBeRemoved),
    list_ext:remove_list_keys(AllHeaders, HeadersToBeRemoved, Headers),
    html_ext:safe_extract_all_links(DOM, FinalUrl, Links),
    %% filter http or https uris
    include(uri_ext:is_http_uri, Links, HttpLinks).

%% safe_crawl_html_page(Url, FinalUrl, Headers, HttpLinks, Err),
%%     catch((crawl_html_page(Url, FinalUrl, Headers, HttpLinks), Err = none),
%% 	  Err,
%% 	  (FinalUrl=Url, Headers=[], HttpLinks=[])).
    
crawl_url(Url, domain{url:Url, 
                domain:Domain,
                final_domain:FinalDomain,
                final_url:FinalUrl,
                domains:Domains,
                headers:HeadersDict,
                tags:Tags,
                social_tags:SocialTags,
                internalLinks:SameDomainLinks,
                externalLinks:ExternalLinks}
):-
    uri_ext:uri_domain(Url, Domain),
    crawl_html_page(Url, FinalUrl, Headers, AllLinks),
    headers_ext:headers_to_dict(Headers, HeadersDict),
    %% remove finalUrl from links
    select(FinalUrl, AllLinks, Links),
    uri_components(FinalUrl, uri_components(_, FinalDomain, _, _, _)),
    %% split external and internal links
    split_links(Links, FinalDomain, SameDomainLinks, ExternalLinks),
    tagger:tags_from_headers(Headers, Tags),
    tagger:social_tags_from_links(SocialTags, ExternalLinks),
    uri_ext:uris_domains(Links, Domains).

crawl_domains([]).
crawl_domains([Domain|Domains]):-
    uri_components(Url, uri_components('https', Domain, "/", _Params, _Frag)),
    crawl_url(Url, Data),
    atom_json_dict(Json, Data, []),
    db:put(Domain, Json),
    crawl_domains(Domains).
