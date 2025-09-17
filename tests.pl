% -*- Mode: Prolog -*-
% Load the plunit testing framework
:- use_module(library(plunit)).
:- use_module(library(pcre)).

%:- use_module(tagger, [tags_from_keys/2, social_tag_from_links/2]).
:- use_module(tagger).
:- use_module(tags).

% ============================================
% UNIT TEST SUITES
% ==================================:w==========

headers([
    status_code(200),content_type('text/html; charset=UTF-8'),
    content_length(171131),connection(close),date('Tue, 09 Sep 2025 15:46:15 GMT'),
    x_cache_hits('3'),expires('Tue, 09 Sep 2025 15:47:04 GMT'),accept_ranges(bytes),
    x_cache('Hit from cloudfront'),
    via('1.1 b99e5246c1a12f20769781294e616682.cloudfront.net (CloudFront)'),
    x_amz_cf_pop('FCO50-P4'),x_amz_cf_id('-D_FoKV2JZb96h9KRXJoypkSotf9kITi09ifB8T8ZZHFV_nz2zsuTg=='),
    age('38')]).

:- begin_tests(tagger).

test(dummy):- true.

test(tag_aws_cloudfront):-
  tagger:tags_from_keys(Tags, "status_code age x_amz_dd"),
  member("cloud/aws", Tags).

test(social_tag_fb10):-
    re_matchsub("^http[s]?://(www.)?facebook.com/profile\\.php\\?id=(?<user>\\d+)$", 
                "https://www.facebook.com/profile.php?id=1111", 
                Dict, []), 
    Dict.user = "1111".

test(social_tag_fb11):-
    re_matchsub("^https?://(www.)?facebook.com/(?<user>[A-Za-z0-9_.-]+)/?$", 
                "https://www.facebook.com/1111", 
                Dict, []), 
    Dict.user = "1111".

%social_tag(facebook, "^http[s]?://(www.)?facebook.com/(?<user>[A-Za-z0-9_\-\.]+)/?$").
test(social_tag_fb20):- 
	tags:social_tag(facebook, Regex),
	re_matchsub(Regex, "https://www.facebook.com/profile.php?id=1111", Dict, []),
    User = Dict.user,
	Tag =.. [facebook, User],
    Tag = facebook("1111").

test(social_tag_fb30):-
  tagger:social_tag_from_links(facebook("1111"), ["https://www.facebook.com/profile.php?id=1111"]).

:- end_tests(tagger).
