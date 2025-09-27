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


:- module(crawler,
[
    crawl_domain/2,
    crawl_domains/2
]).

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

ragno_http_options(MyOptions, HttpOptions):-
    %% [final_url(FinalUrl), headers(Headers)]
    %% collapse_options(MergedHttpOptions, FinalHttpOptions),
    config:http_options(HttpDefaultOptions),
    merge_options(MyOptions, HttpDefaultOptions, HttpOptions).

get_http_page_info(Url, FinalUrl, Headers):-
    ragno_http_options([method(get), final_url(FinalUrl), headers(Headers)], HttpOptions),
    http_open(Url, In, HttpOptions),
    close(In).

crawl_url_with_redirect_to_other_domain(Url, Domain, DataIn, DataOut):-
    get_http_page_info(Url, FinalUrl, _Headers),
    uri_ext:uri_domain(FinalUrl, FinalDomain),
    Domain \= FinalDomain,
    put_dict(domain{ragno_status: done,
                    final_url:FinalUrl,
                    final_domain:FinalDomain,
                    domains:[FinalDomain],
                    headers:_{}
                    }, DataIn, DataOut).

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

crawl_url(Url, Domain, DataIn, DataOut):-
    get_html_page(Url, FinalUrl, AllHeaders, DOM),
    uri_components(FinalUrl, uri_components(_, FinalDomain, _, _, _)),
    removed_http_headers(HeadersToBeRemoved),
    list_ext:remove_list_keys(AllHeaders, HeadersToBeRemoved, Headers),
    headers_ext:headers_to_dict(Headers, HeadersDict),
    put_dict(domain{ragno_status: headers,
                    final_domain:FinalDomain,
                    final_url:FinalUrl,
                    headers:HeadersDict
                    }, DataIn, Data02),
    db:put(Domain, Data02),
    html_ext:safe_extract_all_links(DOM, FinalUrl, AllLinks),
    %% filter http or https uris
    include(uri_ext:is_http_uri, AllLinks, HttpLinks),
    %% remove finalUrl from links
    select(FinalUrl, HttpLinks, Links),
    uri_ext:uris_domains(Links, DirtyDomains),
    maplist(uri_ext:www_without_numbers, DirtyDomains, Domains),
    %% split external and internal links
    split_links(Links, FinalDomain, SameDomainLinks, ExternalLinks),
    put_dict(domain{ragno_status: domains,
                    domains:Domains,
                    internalLinks:SameDomainLinks,
                    externalLinks:ExternalLinks},
                    Data02, Data03),
    db:put(Domain, Data03),
    tagger:tags_from_headers(Headers, Tags),
    tagger:social_tags_from_links(SocialTags, ExternalLinks),
    once(
        html_ext:safe_extract_text(DOM, //head/title(text), Title) ;
        xpath(DOM, //meta(@name=title, @content=Title), _) ;
        Title = ""),
    once(
        xpath(DOM, //meta(@name=description, @content=Description), _) ;
        xpath(DOM, //meta(@property='og:description', @content=Description), _) ; 
        Description = ""),
    once(xpath(DOM, //meta(@property='og:type', @content=OgType), _) ; OgType = ""),
    put_dict(domain{ragno_status: done,
                    html_title:Title,
                    html_description:Description,
                    og_type:OgType,
                    tags:Tags,
                    social_tags:SocialTags},
            Data03, DataOut),
    db:put(Domain, DataOut).

once_crawl_url(Url, Domain, DataIn, DataOut):-
    once(
        crawl_url_with_redirect_to_other_domain(Url, Domain, DataIn, DataOut) ;
        crawl_url(Url, Domain, DataIn, DataOut)
    ).

crawl_domain(Domain, DataOut):-
    uri_components(Url, uri_components('https', Domain, "/", _Params, _Frag)),
    get_time(Timestamp),
    DataIn = domain{url:Url,
                    ragno_status:starting,
                    ragno_ts:Timestamp,
                    domain:Domain,
                    domains:[]},
    db:put(Domain, DataIn),
    catch(
        once_crawl_url(Url, Domain, DataIn, DataOut),
		ExTerm, 
        (
            format("Exception: ~q\n",[ExTerm]),
            term_to_atom(ExTerm, ExString),
            put_dict(domain{ragno_status: error,
                            error_message:ExString},
                            DataIn, DataOut)
        )
    ),
    db:put(Domain, DataOut).

crawl_domains(Domains, Results):-
    maplist(crawl_domain, Domains, Results).
