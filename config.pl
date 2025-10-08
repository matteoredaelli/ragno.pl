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

:- module(config,
          [
              threadpool_size/1,
              http_options/1,
              removed_http_headers/1,
              skip_domains/1
          ]).

tld(com).
tld(eu).
tld(io).
tld(it).
tld(net).
tld(rs).
tld(org).

threadpool_size(15).

http_options( [redirect(true),
               timeout(8),
               %% proxy(proxy.local:80),
               cert_verify_hook(cert_accept_any),
               user_agent("Ragno.pl/0.1")]).

removed_http_headers([accept_ranges,
                      alt_svc,
                      cache_control,
                      content_security_policy,
                      etag,
                      expires,
                      reporting_endpoints,
                      strict_transport_security,
                      set_cookie,
                      x_cache,
                      x_cache_hits,
                      x_connection_hash,
                      x_timer,
                      x_xss_protection
                     ]).

skip_domains([
                 "porn|sex|xxx|sesso",
                 "^\\d+\\."
             ]).
