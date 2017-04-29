:- module(
  html_ext,
  [
    deck//2,              % :Card_1, +Items
    deck//3,              % +Attrs, :Card_1, +Items
    html_call//1,         % :Html_0
    html_call//2,         % :Html_1, +Arg1
    html_maplist//2,      % :Html_1, +Args1
    html_thousands//1,    % +Integer
    image//1,             % +Spec
    image//2,             % +Spec, +Attrs
    meta_authors//0,
    meta_description//1,  % +Desc
    navbar//3,            % :Brand_0, :Menu_0, :Right_0
    open_graph//2,        % +Key, +Value
    tooltip//2,           % +String, :Html_0
    twitter_follow_img//0
  ]
).
:- reexport(library(http/html_head)).
:- reexport(library(http/html_write)).

/** <module> HTML extensions

Besides the reusable HTML snippets provided by this module, raw HTML
can always be included by using the following quasi-quoting notation:

```
html({|html||...|}).
```

@author Wouter Beek
@version 2017/04
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_resource)).
:- use_module(library(http/jquery)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(uri_ext)).

% jQuery
:- set_setting(jquery:version, '3.2.1.min').

:- dynamic
    html:author/1,
    nlp:nlp_string0/3.

:- html_meta
    deck(3, +, ?, ?),
    deck(+, 3, +, ?, ?),
    html_call(html, ?, ?),
    html_call(3, +, ?, ?),
    html_maplist(3, +, ?, ?),
    navbar(html, html, html, ?, ?),
    tooltip(+, html, ?, ?).

% Bootstrap
:- if(debugging(css(bootstrap))).
  :- html_resource(
       css(bootstrap),
       [requires([css('bootstrap.css')]),virtual(true)]
     ).
:- else.
  :- html_resource(
       css(bootstrap),
       [requires([css('bootstrap.min.css')]),virtual(true)]
     ).
:- endif.
:- if(debugging(js(bootstrap))).
  :- html_resource(
       js(bootstrap),
       [
         ordered(true),
         requires([jquery,tether,js('bootstrap.js')]),
         virtual(true)
       ]
     ).
:- else.
  :- html_resource(
       js(bootstrap),
       [
         ordered(true),
         requires([jquery,tether,js('bootstrap.min.js')]),
         virtual(true)
       ]
     ).
:- endif.
:- html_resource(
     bootstrap,
     [requires([css(bootstrap),js(bootstrap)]),virtual(true)]
   ).

% FontAwesome
:- if(debugging(css('font-awesome'))).
  :- html_resource(
       css('font-awesome'),
       [requires([css('font-awesome-4.7.0.css')]),virtual(true)]
     ).
:- else.
  :- html_resource(
       css('font-awesome'),
       [requires([css('font-awesome-4.7.0.min.css')]),virtual(true)]
     ).
:- endif.
:- html_resource(
     'font-awesome',
     [requires([css('font-awesome')]),virtual(true)]
   ).

% HTML extensions
:- html_resource(
     html_ext,
     [
       ordered(true),
       requires([bootstrap,'font-awesome',css('html_ext.css')]),
       virtual(true)
     ]
   ).

% Tether
:- if(debugging(js(tether))).
  :- html_resource(
       js(tether),
       [requires([js('tether-1.3.3.js')]),virtual(true)]
     ).
:- else.
  :- html_resource(
       js(tether),
       [requires([js('tether-1.3.3.min.js')]),virtual(true)]
     ).
:- endif.
:- html_resource(
     tether,
     [requires([js(tether)]),virtual(true)]
   ).

:- multifile
    html:author/1,
    nlp:nlp_string0/3.

nlp:nlp_string0(en, follow_us_on_x, "Follow us on ~s").
nlp:nlp_string0(nl, follow_us_on_x, "Volg ons op ~s").

:- setting(
     html:twitter_profile,
     any,
     _,
     "Optional Twitter profile name."
   ).
:- setting(
     nlp:language,
     atom,
     en,
     "The natural language in which content should be displayed to the user."
  ).





%! deck(:Card_1, +Items)// is det.
%! deck(+Attrs, :Card_1, +Items)// is det.

deck(Card_1, L) -->
  deck([], Card_1, L).


deck(Attrs1, Card_1, L) -->
  {merge_attrs([class=['card-columns']], Attrs1, Attrs2)},
  html(div(Attrs2, \html_maplist(Card_1, L))).



%! html_call(:Html_0)// is det.
%! html_call(:Html_1, +Arg1)// is det.

html_call(Html_0, X, Y) :-
  call(Html_0, X, Y).


html_call(Html_1, Arg1, X, Y) :-
  call(Html_1, Arg1, X, Y).



%! html_maplist(:Html_1, +Args1) .

html_maplist(_, []) --> !, [].
html_maplist(Html_1, [H|T]) -->
  html_call(Html_1, H),
  html_maplist(Html_1, T).


%! html_thousands(+Integer)// is det.

html_thousands(inf) --> !,
  html("∞").
html_thousands(Integer) -->
  html("~:D"-[Integer]).



%! image(+Spec)// is det.
%! image(+Spec, +Attrs)// is det.

image(Spec) -->
  image(Spec, []).


image(Spec, Attrs1) -->
  {
    uri_specification(Spec, Uri),
    merge_attrs(Attrs1, [src=Uri], Attrs2)
  },
  html(img(Attrs2, [])).



%! meta_authors// is det.

meta_authors -->
  {
    findall(String, html:author(String), Strings),
    atomics_to_string(Strings, ",", String)
  },
  meta(author, String).



%! meta_description(+String)// is det.

meta_description(String) -->
  meta(description, String).



%! navbar(:Brand_0, :Menu_0, :Right_0)// is det.

navbar(Brand_0, Menu_0, Right_0) -->
  html([
    nav([
      class=[
        'bg-faded',
        'fixed-top',
        navbar,
        'navbar-light',
        'navbar-toggleable-md'
      ]
    ], [
        \hamburger,
        a([class='navbar-brand',href='/'], Brand_0),
        div([class=[collapse,'navbar-collapse'],id=target], [
          ul(class=['navbar-nav','mr-auto'], Menu_0),
          ul(class='navbar-nav', Right_0)
        ])
      ]
    )
  ]).

hamburger -->
  html(
    button([
      'aria-controls'='target#',
      'aria-expanded'=false,
      'aria-label'="Toggle navigation",
      class=[collapsed,'navbar-toggler','navbar-toggler-right'],
      'data-target'='target#',
      'data-toggle'=collapse,
      type=button
    ], span(class='navbar-toggler-icon', []))
  ).



%! open_graph(+Key, +Value)// is det.

open_graph(Key0, Val) -->
  {atomic_list_concat([og,Key0], :, Key)},
  html(meta([property=Key,content=Val], [])).



%! tooltip(+String, :Html_0)// is det.

tooltip(String, Html_0) -->
  html(span(['data-toggle'=tooltip,title=String], Html_0)).



%! twitter_follow_img// is det.

twitter_follow_img -->
  {
    setting(html:twitter_profile, User),
    ground(User)
  }, !,
  {nlp_string(follow_us_on_x, ["Twitter"], String)},
  tooltip(String, \twitter_follow0(User, \twitter_img0)).  
twitter_follow_img --> [].

nlp_string(Name, Args, String) :-
  setting(nlp:language, LTag),
  nlp:nlp_string0(LTag, Name, Format),
  format(string(String), Format, Args).

twitter_follow0(User, Html_0) -->
  {twitter_user_uri0(User, Uri)},
  html(a(href=Uri, html_call(Html_0))).

twitter_img0 -->
  image(img('twitter.png'), [alt="Twitter"]).

twitter_user_uri0(User, Uri) :-
  uri_comps(Uri, uri(https,'twitter.com',[User],_,_)).





% HELPERS %

%! merge_attrs(+Attrs1, +Attrs2, -Attrs3) is det.
%
% Merge two lists of HTML attributes into one.

merge_attrs([], L, L) :- !.
% HTTP attribute with (possibly) multiple values.
merge_attrs([Key=Val1|T1], L2a, [Key=Val3|T3]):-
  attr_multi_value(Key),
  selectchk(Key=Val2, L2a, L2b), !,
  maplist(ensure_list, [Val1,Val2], [Val1L,Val2L]),
  append(Val1L, Val2L, ValL),
  sort(ValL, Val3),
  merge_attrs(T1, L2b, T3).
% HTTP attribute with a single value.
merge_attrs([Key=_|T1], L2a, [Key=Val2|T3]) :-
  selectchk(Key=Val2, L2a, L2b), !,
  merge_attrs(T1, L2b, T3).
merge_attrs([H|T1], L2, [H|T3]):-
  merge_attrs(T1, L2, T3).

attr_multi_value(class).

ensure_list(L, L) :-
  is_list(L), !.
ensure_list(Elem, [Elem]).



%! meta(+Name, +Content)// is det.

meta(Name, Content) -->
  html(meta([name=Name,content=Content], [])).



%! uri_specification(+Spec, -Uri) is det.
%
% Allows a URI to be specified in the following ways:
%
%   - link_to_id(<HANDLE-ID>)
%
%   - link_to_id(<HANDLE-ID>,<QUERY>)
%
%   - Compound terms processed by http_absolute_location/3
%
%   - atoms
%
% Whenever possible, the URI is abbreviated in case its schema, host
% and port are the local schema, host and port.

uri_specification(link_to_id(HandleId), Uri2) :- !,
  http_link_to_id(HandleId, [], Uri1),
  uri_remove_host(Uri1, Uri2).
% @tbd
%uri_specification(link_to_id(HandleId,Query0), Uri2) :- !,
%  maplist(rdf_query_term, Query0, Query), %HACK
%  http_link_to_id(HandleId, Query, Uri1),
%  uri_remove_host(Uri1, Uri2).
uri_specification(Spec, Uri2) :-
  http_absolute_location(Spec, Uri1, []),
  uri_remove_host(Uri1, Uri2).
