:- module(
  html_doc,
  [
    http_doc_handler//2 % +Module, +Handler
  ]
).

/** <module> HTML documentation

@author Wouter Beek
@version 2016/08-2016/09, 2017/08
*/

:- use_module(library(aggregate)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- multifile
    http_media_types/2,
    http_param/1,
    http_params/1.





http_doc_handler(Module, Handler) -->
  {
    http_current_handler(Location, Module:Handler),
    Module:http_media_types(Handler, MediaTypes),
    Module:http_params(Handler, Keys),
    aggregate_all(
      set(Key-Spec),
      (
        member(Key, Keys),
        Module:http_param(Key, Spec)
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
  table_header_row(["Media Type"]).

media_type_data_row(media(Supertype/Subtype,_)) -->
  html(tr(td(code([Supertype,"/",Subtype])))).

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
% rdf_term
param_type(Spec) -->
  {memberchk(rdf_term, Spec)}, !,
  html("RDF term").
% string
param_type(Spec) -->
  {memberchk(string, Spec)}, !,
  html("String").

param_required(Spec) -->
  {
    option(optional(Bool), Spec, false),
    param_bool_label(Bool, Label)
  },
  html(Label).

param_bool_label(true, "No").
param_bool_label(false, "Yes").

param_default(Spec) -->
  {option(default(Val), Spec, "")},
  html(Val).

param_desc(Spec) -->
  {option(description(Desc), Spec, "")},
  html(Desc).
