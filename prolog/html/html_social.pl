:- module(
  html_social,
  [
    facebook_follow_img//0,
    facebook_share//2,      % +Resource, +Title
    twitter_follow_img//0,
    twitter_share//2        % +Resource, +Title
  ]
).

/** <module> HTML social

@author Wouter Beek
@version 2017/06-2017/08
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/http_server)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(settings)).
:- use_module(library(uri/uri_ext)).

:- html_meta
   twitter_follow_(+, html, ?, ?).

:- multifile
    nlp:nlp_string_/3.

nlp:nlp_string_(en, follow_us_on_x, "Follow us on ~s").
nlp:nlp_string_(nl, follow_us_on_x, "Volg ons op ~s").
nlp:nlp_string_(en, like_us_on_x, "Like us on ~s").
nlp:nlp_string_(nl, like_us_on_x, "Like ons op ~s").
nlp:nlp_string_(en, share_x_on_y, "Share “~s” on ~s.").
nlp:nlp_string_(nl, share_x_on_y, "Deel “~s” op ~s.").

:- setting(
     html:facebook_app_id,
     any,
     _,
     "Facebook application identifier."
   ).
:- setting(
     html:facebook_profile_name,
     any,
     _,
     "Facebook profile name."
   ).
:- setting(
     html:twitter_profile_name,
     any,
     _,
     "Optional Twitter profile name."
   ).





%! facebook_follow_img// is det.
%! facebook_follow_img(+ProfileName:atom)// is det.

facebook_follow_img -->
  {setting(html:facebook_profile_name, ProfileName), ground(ProfileName)}, !,
  {nlp_string(like_us_on_x, ["Facebook"], String)},
  tooltip(String, \facebook_follow_(ProfileName, \facebook_img_)).
facebook_follow_img --> [].



%! facebook_share(+Resource:atom, +Title:string)// is det.

facebook_share(Resource, Title) -->
  {
    nlp_string(share_x_on_y, [Title,"Facebook"], String),
    uri_comps(
      Uri,
      uri(http,'www.facebook.com',['share.php'],[title(Title),u(Resource)],_)
    )
  },
  tooltip(String, a([href=Uri,target='_blank'], \facebook_img_)).



%! twitter_follow_img// is det.

twitter_follow_img -->
  {
    setting(html:twitter_profile_name, ProfileName),
    ground(ProfileName)
  }, !,
  {nlp_string(follow_us_on_x, ["Twitter"], String)},
  tooltip(String, \twitter_follow_(ProfileName, \twitter_img_)).
twitter_follow_img --> [].



%! twitter_share(+Resource:atom, +Title:string)// is det.

twitter_share(Resource, Title) -->
  {
    nlp_string(share_x_on_y, [Title,"Twitter"], String),
    uri_comps(
      Uri,
      uri(https,'twitter.com',[share],[text(Title),url(Resource)],_)
    )
  },
  tooltip(String, a(href=Uri, \twitter_img_)).





% HELPERS %

%! facebook_img_// is det.

facebook_img_ -->
  {http_absolute_location(img('facebook.png'), Uri)},
  html(img([alt="Facebook",src=Uri], [])).



%! facebook_follow_(+ProfileName:atom, :Content_0)// is det.

facebook_follow_(ProfileName, Content_0) -->
  {facebook_user_uri_(ProfileName, Uri)},
  html(a(href=Uri, Content_0)).



%! facebook_user_uri_(+ProfileName:atom, -Uri:atom) is det.

facebook_user_uri_(ProfileName, Uri) :-
  uri_comps(Uri, uri(https,'facebook.com',[ProfileName],_,_)).



%! twitter_follow_(+ProfileName:atom, :Content_0)// is det.

twitter_follow_(ProfileName, Content_0) -->
  {twitter_user_uri_(ProfileName, Uri)},
  html(a(href=Uri, Content_0)).



%! twitter_img_// is det.

twitter_img_ -->
  {http_absolute_location(img('twitter.png'), Image)},
  html(img([alt="Twitter",src=Image], [])).



%! twitter_user_uri_(+ProfileName:atom, -Uri:atom) is det.

twitter_user_uri_(ProfileName, Uri) :-
  uri_comps(Uri, uri(https,'twitter.com',[ProfileName],_,_)).
