:- module(
  html_pagination,
  [
    html_pagination_links//1,          % +Page
    html_pagination_result//2,         % +Page, :Html_1
    html_pagination_result_nonempty//2 % +Page, :Html_1
  ]
).

/** <module> HTML support for pagination

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(dict)).

:- use_module(library(html/html_ext)).
:- use_module(library(http/http_pagination)).
:- use_module(library(pagination)).

:- meta_predicate
    html_pagination_result(+, 3, ?, ?),
    html_pagination_result_nonempty(+, 3, ?, ?).





%! html_pagination_estimate(+Page:dict)// is det.

html_pagination_estimate(Page) -->
  {dict_get(total_number_of_results, Page, TotalNumberOfResults)}, !,
  html([" (of ",\html_thousands(TotalNumberOfResults),")"]).
html_pagination_estimate(_) --> [].



%! html_pagination_links(+Page:dict)// is det.

html_pagination_links(Page) -->
  {http_pagination_links(Page, Pairs)},
  html_maplist(html_link, Pairs).

html_link(Relation-Uri) -->
  html(link([href=Uri,rel=Relation])).



%! html_pagination_next(+Page:dict)// is det.

html_pagination_next(Page) -->
  {http_pagination_link(Page, next, Next)}, !,
  html(li(class='page-item', a([class='page-link',href=Next], "Next"))).
html_pagination_next(_) -->
  html(li(class=[disabled,'page-item'], a(class='page-link', "Next"))).



%! html_pagination_prev(+Page:dict)// is det.

html_pagination_prev(Page) -->
  {http_pagination_link(Page, prev, Prev)}, !,
  html(li(class='page-item', a([class='page-link',href=Prev], "Previous"))).
html_pagination_prev(_) -->
  html(li(class=[disabled,'page-item'], a(class='page-link', "Previous"))).



%! html_pagination_range(+Page:dict)// is det.

html_pagination_range(Page) -->
  {pagination_range(Page, Low-High)},
  html(
    li(class='page-text', [
      "results ",
      \html_thousands(Low),
      " to ",
      \html_thousands(High),
      \html_pagination_estimate(Page)
    ])
  ).



%! html_pagination_result(+Page:dict, :Html_1)// is det.
%
% Opts are required because it contains the `uri` based on which the
% backward/forward request URIs are build.
%
% @arg Page is a dictionary that contains the following keys:
%
%      * number_of_results(nonneg)
%
%      * page(positive_integer)
%
%      * page_size(nonneg)
%
%      * query(list(compound))
%
%      * uri(atom)

html_pagination_result(Page, Html_1) -->
  html_call(Html_1, Page.results),
  html(
    nav('aria-label'="Search results pages",
      ul(class=pagination, [
        \html_pagination_prev(Page),
        " ",
        \html_pagination_range(Page),
        " ",
        \html_pagination_next(Page)
      ])
    )
  ).


%! html_pagination_result_nonempty(+Page:dict, :Html_1)// is det.
%
% Like html_pagination_result/2, but does not generate anything if
% Page is empty.

html_pagination_result_nonempty(Page, _) -->
  {pagination_is_empty(Page)}, !, [].
html_pagination_result_nonempty(Page, Html_1) -->
  html_pagination_result(Page, Html_1).
