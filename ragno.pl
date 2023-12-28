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

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(xpath)).

:- use_module(library(uri)).
:- use_module(library(apply)).

:- use_module(html_ext).
:- use_module(uri_ext).

:- initialization(main, main).

main(Argv) :-
    crawl_root_sites(Argv).

/*
TODO
domini con redirect è su un altro dominio: es http to https, ...
se trovo a.b.com dovrei esplorare anche i domini b.com e www.b.com 
parsing html
- head/title
- body/h1

code di priorità: domain.com o www.domain.com è priroitario rispetto a subdomain.domain.com

i link andrebbero aperti x ottenere la targeturl prima di estrarre i domini.: potrebbero delle short url
*/

%% needed to convert headers to dict

add_key_value(Term, Dict, DictNew):-
    Term =.. [K,V],
    DictNew = Dict.put(K,V).
    
headers_to_dict(Headers, Dict):-
     foldl(add_key_value, Headers, headers{}, Dict).

safe_make_directory(Directory):-
    catch(make_directory(Directory),
	  _ExTerm,
	  %%format("WARNING: ~q\n",[ExTerm])
	  true
	 ).

save_uri_to_jsonfile(Uri, DomainDict):-
    uri_components(Uri, uri_components(_Schema, Domain, _, _, _)),
    domain_details(Domain, DomainList, _Level),
    reverse(DomainList, [_D1,D2|_X]),
    sub_atom(D2, 0, 2, _After, D3),
    atomic_list_concat(['data/', D3], '', Directory),
    safe_make_directory(Directory),
    atomic_list_concat([Directory, '/', D2], '', Directory2),
    safe_make_directory(Directory2),
    re_replace("/"/g, "", Uri, Filename),
    atomic_list_concat([Directory2, '/', Filename, '.json'], FullPathFilename),
    open(FullPathFilename, write, Fd),
    json_write_dict(Fd, DomainDict, [serialize_unknown(true)]),
    close(Fd).


get_html_page(Url, FinalUrl, Headers, DOM):-
    setup_call_cleanup(
	http_open(Url, In, [final_url(FinalUrl),
			     headers(Headers),
			     redirect(true),
			     timeout(4),
			     %% proxy(proxy.local:80),
			     cert_verify_hook(cert_accept_any),
			     user_agent("prolog/ragno")]),
	load_html(In, DOM, []),
        close(In)).

get_final_url(Url, FinalUrl):-
    http_open(Url, In, [final_url(FinalUrl),
			redirect(true),
			timeout(4),
			%% proxy(proxy.local:80),
			cert_verify_hook(cert_accept_any),
			user_agent("prolog/ragno")]),
    close(In).

safe_get_final_url(Url, FinalUrl, Err):-
    catch((get_final_url(Url, FinalUrl), Err = none),
	  Err,
	  FinalUrl = none).

crawl_html_page(Url, FinalUrl, Headers, HttpLinks):-
    get_html_page(Url, FinalUrl, Headers, DOM),
    html_ext:safe_extract_all_links(DOM, FinalUrl, Links),
    %% filter http or https uris
    include(uri_ext:http_uri, Links, HttpLinks).

safe_crawl_html_page(Url, FinalUrl, Headers, Links):-
    catch(crawl_html_page(Url, FinalUrl, Headers, Links),
	  ExTerm,
	  (format("ERROR: ~q\n",[ExTerm]),
	   false)).

crawl_root_site(GenericUrl):-
    uri_ext:domain_uri(GenericUrl, Url),
    safe_crawl_html_page(Url, FinalUrl, Headers, Links),
    %% TODO
    %% [X] remove fragments like #xxx
    %% [ ] https://website.com shoudl be renamed to https://website.com/
    %% [ ] removing port 80 for http nd 443 for https urls
    
    %% extract the domain
    uri_ext:domain_uri(FinalUrl, FinalDomain),
    %%uri_ext:domain_uri(Url, Domain),
    %% split external and internal links
    partition(uri_ext:is_internal_link(FinalDomain), Links, InternalLinks, ExternalLinks),
    headers_to_dict(Headers, HeadersDict),
    uri_ext:domain_uris(Links, RootLinks),
    save_uri_to_jsonfile(Url, domain{url:Url,
				     final_url:FinalUrl,
				     linked_domains:RootLinks,
				     internal_links:InternalLinks,
				     external_links:ExternalLinks,
				     headers:HeadersDict
     				    }).

crawl_root_sites([]).
crawl_root_sites([Url|Urls]):-
    writeln(Url),
    crawl_root_site(Url),
    crawl_root_sites(Urls).
