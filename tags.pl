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
	      key_tags/2,
	      value_tags/2
	  ]).

:-use_module(library(uri)).
:-use_module(headers_ext).

key_tags("x_amz_",     ['cloud/aws']).
key_tags("x_azure",    ['cloud/azure']).
key_tags("bigip",      ['bigip']).
key_tags("akamai",     ['cdn/akamai']).
key_tags("cloudfront", ['cdn/cloudfront']).
key_tags("netlify",    ['cloud/netlify']).
key_tags("x-goog",     ['cloud/google']).
key_tags("varnish",    ['varnish']).
	

tag(server('Netlify'), ['cloud/netlify']).
tag(server(X), [server(X)]).
tag('x-powered-by'(X), ['x-powered-by'(X)]).

value_tags("cloudflare", ['cdn/cloudflare']).
