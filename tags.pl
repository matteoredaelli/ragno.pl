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

:- module(tags, [
              social_tag/2,
              header_tags/2,
              key_tags/2,
              value_tags/2
                ]).

:- use_module(library(uri)).
:- use_module(headers_ext).

key_tags("x_amz_", ['cloud/aws']).
key_tags("x_amz_cf_", ['cdn/cloudfront']).
key_tags("x_azure", ['cloud/azure']).
key_tags("bigip", ['bigip']).
key_tags("akamai", ['cdn/akamai']).
key_tags("cloudfront", ['cdn/cloudfront']).
key_tags("netlify", ['cloud/netlify']).
key_tags("x-edg", ['cloud/edgio']).
key_tags("aruba", ['cloud/aruba']).
key_tags("x-goog", ['cloud/google']).
key_tags("varnish", ['varnish']).


header_tags(server('Netlify'), ['cloud/netlify']).
header_tags(server(X), [Tag]):-
    atomic_list_concat([server, "/", X], Tag).
header_tags('x-powered-by'(X), [Tag]):-
    atomic_list_concat(['x-powered-by', "/", X], Tag).

value_tags("cloudflare", ['cdn/cloudflare']).
value_tags("httpd", ['sw/apache']).
value_tags("nginx", ['sw/nginx']).
value_tags("varnish", ['sw/varnish']).
value_tags("lightspeed", ['sw/lightspeed']).

social_tag(facebook, "^https?://(www.)?facebook.com/profile\\.php\\?id=(?<user>\\d+)$").
social_tag(facebook, "^https?://(www.)?facebook.com/(?<user>[A-Za-z0-9_\\-.]+)/?$").
social_tag(github, "^https?://(www.)?github.com/(?<user>[A-Za-z0-9_.-]+)/?$").
social_tag(instagram, "^https?://(www.)?instagram.com/(?<user>[A-Za-z0-9_.-]+)/?$").
social_tag(instagram, "^https?://(www.)?instagr.am/(?<user>[A-Za-z0-9_.-]+)/?$").
social_tag(linkedin, "^https?://(www.)?linkedin.com/(company/)?(?<user>[A-Za-z0-9_\\.-@]+)/?$").
social_tag(youtube, "^https?://(www.)?youtube.com/((c|user)/)?(?<user>[A-z0-9_\\.-@]+)/?$").
social_tag(tiktok, "^https?://(www.)?tiktok.com/@(?<user>[A-Za-z0-9_.-]+)/?$").
social_tag(twitter, "^https?://(www.)?twitter.com/(?<user>[A-Za-z0-9_.-]+)/?$").
