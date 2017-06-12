:- module(
  html_social,
  [
    fb_follow_img//0,
    fb_follow_img//1,     % +User:atom
    twitter_follow_img//0
  ]
).

/** <module> HTML social

@author Wouter Beek
@version 2017/06
*/

:- use_module(library(html/html_ext)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(settings)).
:- use_module(library(uri/uri_ext)).

:- html_meta
   twitter_follow0(+, html, ?, ?).

:- multifile
    nlp:nlp_string0/3.

nlp:nlp_string0(en, follow_us_on_x, "Follow us on ~s").
nlp:nlp_string0(nl, follow_us_on_x, "Volg ons op ~s").

:- setting(
     html:twitter_profile,
     any,
     _,
     "Optional Twitter profile name."
   ).





%! fb_follow_img// is det.
%! fb_follow_img(+User:atom)// is det.

fb_follow_img -->
  {setting(html:fb_profile, User), ground(User)}, !,
  fb_follow_img(User).
fb_follow_img --> [].


fb_follow_img(User) -->
  {nlp_string(like_us_on_x, ["Facebook"], Str)},
  tooltip(Str, \fb_follow0(User, \fb_img0)).



%! twitter_follow_img// is det.

twitter_follow_img -->
  {
    setting(html:twitter_profile, User),
    ground(User)
  }, !,
  {nlp_string(follow_us_on_x, ["Twitter"], String)},
  tooltip(String, \twitter_follow0(User, \twitter_img0)).  
twitter_follow_img --> [].





% HELPERS %

%! fb_follow0(+User:atom, :Html_0)// is det.

fb_follow0(User, Html_0) -->
  {fb_user_uri(User, Uri)},
  html(a(href=Uri, Html_0)).



%! fb_user_uri0(+User:atom, -Uri:atom) is det.

fb_user_uri0(User, Uri) :-
  uri_comps(Uri, uri(https,'facebook.com',[User],_,_)).



%! twitter_follow0(+User:atom, :Content_0)// is det.

twitter_follow0(User, Content_0) -->
  {twitter_user_uri0(User, Uri)},
  html(a(href=Uri, Content_0)).



%! twitter_img0// is det.

twitter_img0 -->
  image(img('twitter.png'), [alt="Twitter"]).



%! twitter_user_uri0(+User:atom, -Uri:atom) is det.

twitter_user_uri0(User, Uri) :-
  uri_comps(Uri, uri(https,'twitter.com',[User],_,_)).
