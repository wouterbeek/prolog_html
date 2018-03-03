:- module(
  html_doc,
  [
    http_doc_handler//2 % +Module, +Handler
  ]
).

/** <module> HTML documentation

@author Wouter Beek
@version 2016-2018
*/

:- use_module(library(aggregate)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- use_module(library(html/html_ext)).
:- use_module(library(media_type)).

:- multifile
    html_doc:custom_param_type//1,
    http:media_types/2,
    http:param/2,
    http:params/2.





http_doc_handler(Module, Handler) -->
  {
    http_current_handler(Location, Module:Handler),
    http:media_types(Handler, MediaTypes),
    http:params(Handler, Keys),
    aggregate_all(
      set(Key-Spec),
      (
        member(Key, Keys),
        http:param(Key, Spec)
      ),
      Params
    )
  },
  html([
    h1(a(href=Location, code(Location))),
    h2("Media Types"),
    \table(
      \media_type_header_row,
      \html_maplist(media_type_data_row, MediaTypes)
    ),
    h2("HTTP parameters"),
    \table(
      \http_param_header_row,
      \html_maplist(http_param_data_row, Params)
    )
  ]).

media_type_header_row -->
  table_header_row(["Media Type","Name"]).

media_type_data_row(MediaType) -->
  {
    media_type_label(MediaType, Label),
    media_type_comps(MediaType, Super, Sub, _)
  },
  html(tr([td(code([Super,"/",Sub])),td(Label)])).

http_param_header_row -->
  table_header_row(["Parameter","Type","Required","Default","Description"]).

http_param_data_row(Key-Spec) -->
  html(
    tr([
      td(\param_key(Key)),
      td(\param_type(Spec)),
      td(\param_required(Spec)),
      td(\param_default(Spec)),
      td(\param_desc(Spec))
    ])
  ).

param_key(Key) -->
  html(code(Key)).

% atom
param_type(Spec) -->
  {memberchk(atom, Spec)}, !,
  html("atom").
% between(Low,High)
param_type(Spec) -->
  {memberchk(between(Low,High), Spec)}, !,
  {format(string(Label), "~D ≤ n ≤ ~D", [Low,High])},
  html(Label).
% boolean
param_type(Spec) -->
  {memberchk(boolean, Spec)}, !,
  html("boolean").
% float
param_type(Spec) -->
  {memberchk(float, Spec)}, !,
  html("float").
% nonneg
param_type(Spec) -->
  {memberchk(nonneg, Spec)}, !,
  html("n ≥ 0").
% positive integer
param_type(Spec) -->
  {memberchk(positive_integer, Spec)}, !,
  html("n ≥ 1").
% rdf_iri
param_type(Spec) -->
  {memberchk(rdf_iri, Spec)}, !,
  html("RDF IRI").
% rdf_literal
param_type(Spec) -->
  {memberchk(rdf_literal, Spec)}, !,
  html("RDF literal").
% string
param_type(Spec) -->
  {memberchk(string, Spec)}, !,
  html("String").
% custom parameter types
param_type(Spec) -->
  html_doc:custom_param_type(Spec), !.

param_required(Spec) -->
  {
    (   option(default(_), Spec)
    ->  Bool = false
    ;   option(optional(Bool), Spec, false)
    ),
    param_bool_label(Bool, Label)
  },
  html(Label).

param_bool_label(false, "No").
param_bool_label(true, "Yes").

param_default(Spec) -->
  {option(default(Val), Spec, "")},
  html(Val).

param_desc(Spec) -->
  {option(description(Desc), Spec, "")},
  html(Desc).
