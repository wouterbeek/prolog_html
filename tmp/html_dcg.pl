:- module(
  html_dcg,
  [
    html_attr//1,    % +Attribute
    html_dcg//1,     % +Content
    html_element//1, % +Name
    html_element//2, % +Name, +Attributes
    html_element//3, % +Name, +Attributes, :Content_0
    html_entity//1,  % +Name
    html_graphic//1, % +Code
    html_string//1,  % +String
    html_style//1    % +Pair
  ]
).

/** <module> HTML DCG

DCG grammar for generating HTML snippets.

@author Wouter Beek
@compat HTML 4.01
@version 2015/07-2015/08, 2015/11-2015/12, 2017/08
*/

:- use_module(library(dcg/dcg_ext)).

:- meta_predicate
    html_element(+, +, //, ?, ?).





%! html_attr(+Attribute:pair)// is det.

html_attr(Name-Value) -->
  " ",
  atom(Name),
  "=\"",
  html_string(Value),
  "\"".



%! html_dcg(+Content:list(compound))// is det.

% Tag with no content.
html_dcg([tag(Name,Attributes)|T]) --> !,
  html_element(Name, Attributes),
  html_dcg(T).
% Tag with content.
html_dcg([tag(Name,Attributes,Contents)|T]) --> !,
  html_element(Name, Attributes, html_dcg(Contents)),
  html_dcg(T).
% Atom.
html_dcg([H|T]) -->
  {atom(H)}, !,
  atom(H),
  html_dcg(T).
% Codes list.
html_dcg([H|T]) -->
  html_string(H), !,
  html_dcg(T).
% Done.
html_dcg([]) --> !, "".



%! html_entity(+Name:atom)// is det.

html_entity(Name) -->
  "&",
  atom(Name),
  ";".



%! html_element(+Name:atom)// is det.
%! html_element(+Name:atom, ?Attributes:list(pair))// is det.

html_element(Name) -->
  html_element(Name, []).


html_element(Name, Attributes) -->
  "<",
  atom(Name),
  *(html_attr, Attributes),
  "/>".


%! html_element(+Name:atom, ?Attributes:list(pair), :Content_0)// is det.

html_element(Name, Attributes, Content_0) -->
  "<",
  atom(Name),
  *(html_attr, Attributes),
  ">",
  Content_0,
  "</",
  atom(Name),
  ">".



%! html_graphic(+Code:code)// is det.
%
% HTML reserves the following ASCII characters:
%
%   - Ampersand
%   - Apostrophe
%   - Greater-than
%   - Less-than
%   - Quotation mark

html_graphic(0'&) --> !, "&amp;".
html_graphic(0'') --> !, "&#39".
html_graphic(0'") --> !, "&quot;". %"
html_graphic(0'<) --> !, "&lt;".
html_graphic(0'>) --> !, "&gt;".
html_graphic(C)   --> [C].



%! html_string(+String:atom)// is det.
%
% An **HTML string** is a sequence of printable or graphic HTML
% characters.  This includes spaces.

html_string(S) -->
  {atom_codes(S, Cs)},
  *(html_graphic, Cs).



%! html_style(+Pair:pair)// is det.

html_style(Name-Value) -->
  atom(Name),
  ":",
  (" ", ! ; ""),
  atom(Value),
  ";".
