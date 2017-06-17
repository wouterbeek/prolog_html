:- module(
  html_ext,
  [
    data_link//2,          % +Resource, :Html_0
    data_link//3,          % +Resource, +Attrs, :Html_0
    deck//2,               % :Card_1, +Items
    deck//3,               % +Attrs, :Card_1, +Items
    dropdown_menu//3,      % :Top_0, :Item_1, +Items
    dropdown_menu//4,      % +Attrs, :Top_0, :Item_1, +Items
    external_link//1,      % +Uri
    external_link//2,      % +Uri, :Content_0
    external_link//3,      % +Uri, +Attrs, :Content_0
    external_link_icon//1, % +Uri
    favicon//1,            % +Spec
    flag_icon//1,          % +LTag
    form//2,               % +Spec, :Content_0
    form//3,               % +Spec, +Attrs, :Content_0
    google_analytics//0,
    html_call//1,          % :Html_0
    html_call//2,          % :Html_1, +Arg1
    html_date_time//1,     % +Something
    html_date_time//2,     % +Something, +Opts
    html_ellipsis//2,      % +String, +MaxLen
    html_maplist//2,       % :Html_1, +Args1
    html_nlp_string//1,    % +Name
    html_page/2,           % :Head_0, :Body_0
    html_page/3,           % +Context, :Head_0, :Body_0
    html_seplist//2,       % :Html_0, :Sep_0
    html_seplist//3,       % :Html_1, :Sep_0, +Args
    html_set//1,           % +Args
    html_set//2,           % :Html_1, +Args
    html_space//0,
    html_thousands//1,     % +Integer
    icon//1,                 % +Name
    icon_button//1,          % +Name
    icon_button//2,          % +Name, +Func
    ignore//1,             % :Html_0
    image//1,              % +Spec
    image//2,              % +Spec, +Attrs
    internal_link//1,      % +Spec
    internal_link//2,      % +Spec, :Content_0
    internal_link//3,      % +Spec, +Attrs, :Content_0
    language_menu//1,      % +LTags
    mail_icon//1,          % +Uri
    meta_authors//0,
    meta_description//1,   % +Desc
    navbar//3,             % :Brand_0, :Menu_0, :Right_0
    open_graph//2,         % +Key, +Value
    pipe//0,
    row_1//1,              % :ContentA_0
    row_1//2,              % +WidthsA, :ContentA_0
    row_1//3,              % +Attrs, +WidthsA, :ContentA_0
    row_3//3,              % :ContentA_0, :ContentB_0, :ContentC_0
    row_3//6,              % +WidthsA, :ContentA_0,
                           % +WidthsB, :ContentB_0
                           % +WidthsC, :ContentC_0
    row_3//7,              % +Attrs, +WidthsA, :ContentA_0,
                           %         +WidthsB, :ContentB_0
                           % +WidthsC, :ContentC_0
    submit_button//0,
    submit_button//1,      % :Content_0
    table//1,              % :Body_0
    table//2,              % :Header_0, :Body_0
    table//3,              % :Caption_0, :Header_0, :Body_0
    table_caption//1,      % :Caption_0
    table_content//2,      % :Cell_1, +Rows
    table_data_row//1,     % +Row
    table_data_row//2,     % :Cell_1, +Row
    table_header_row//1,   % +Row
    table_header_row//2,   % :Cell_1, +Row
    tooltip//2,            % +String, :Content_0
    vote_down//1,          % +Vote:integer
    vote_up//1             % +Vote:integer
  ]
).
:- reexport(library(http/html_head)).
:- reexport(library(http/html_write)).
:- reexport(library(http/js_write)).

/** <module> HTML extensions

Besides the reusable HTML snippets provided by this module, raw HTML
can always be included by using the following quasi-quoting notation:

```
html({|html||...|}).
```

@author Wouter Beek
@version 2017/04-2017/06
*/

:- use_module(library(apply)).
:- use_module(library(date_time)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_date_time_human)).
:- use_module(library(html/html_date_time_machine)).
:- use_module(library(http/http_server)).
:- use_module(library(http/jquery)).
:- use_module(library(lists)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(settings)).
:- use_module(library(string_ext)).
:- use_module(library(uri/uri_ext)).

% jQuery
:- set_setting(jquery:version, '3.2.1.min').

:- html_meta
   data_link(+, html, ?, ?),
   data_link(+, +, html, ?, ?),
   dropdown_menu(html, :, +, ?, ?),
   dropdown_menu(+, html, :, +, ?, ?),
   external_link(+, html, ?, ?),
   external_link(+, +, html, ?, ?),
   form(+, html, ?, ?),
   form(+, +, html, ?, ?),
   html_call(html, ?, ?),
   html_page(html, html),
   html_page(+, html, html),
   html_page_head0(html, ?, ?),
   html_seplist(html, html, ?, ?),
   html_seplist(3, html, +, ?, ?),
   ignore(html, ?, ?),
   internal_link(+, html, ?, ?),
   internal_link(+, +, html, ?, ?),
   navbar(html, html, html, ?, ?),
   navbar_dropdown_menu(+, +, 3, +, ?, ?),
   row_1(html, ?, ?),
   row_1(+, html, ?, ?),
   row_1(+, +, html, ?, ?),
   row_3(html, html, html, ?, ?),
   row_3(+, html, +, html, +, html, ?, ?),
   row_3(+, +, html, +, html, +, html, ?, ?),
   submit_button(html, ?, ?),
   table(html, ?, ?),
   table(html, html, ?, ?),
   table(html, html, html, ?, ?),
   table_caption(html, ?, ?),
   table_header(html, ?, ?),
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

:- meta_predicate
    deck(3, +, ?, ?),
    deck(+, 3, +, ?, ?),
    dropdown_menu(2, 3, +, ?, ?),
    dropdown_menu(+, 2, 3, +, ?, ?),
    html_call(3, +, ?, ?),
    html_list_item(3, +, ?, ?),
    html_maplist(3, +, ?, ?),
    html_set(3, +, ?, ?),
    table_content(3, +, ?, ?),
    table_data_cell(3, +, ?, ?),
    table_data_row(3, +, ?, ?),
    table_header_cell(3, +, ?, ?),
    table_header_row(3, +, ?, ?).

:- multifile
    html:author/1,
    html:html_hook//1,
    html:html_hook//2.





%! data_link(+Resource, :Html_0)// is det.
%! data_link(+Resource, +Attrs, :Html_0)// is det.

data_link(Resource, Html_0) -->
  data_link(Resource, [], Html_0).


data_link(Resource, Attrs, Html_0) -->
  {uri_resource(Uri, Resource)},
  internal_link(Uri, Attrs, Html_0).



%! deck(:Card_1, +Items)// is det.
%! deck(+Attrs, :Card_1, +Items)// is det.

deck(Card_1, L) -->
  deck([], Card_1, L).


deck(Attrs1, Card_1, L) -->
  {merge_attributes([class=['card-columns']], Attrs1, Attrs2)},
  html(div(Attrs2, \html_maplist(Card_1, L))).



%! dropdown_menu(:Top_0, :Item_1, +Items)// is det.
%! dropdown_menu(+Attrs, :Top_0, :Item_1, +Items)// is det.

dropdown_menu(Top_0, Item_1, L) -->
  dropdown_menu([], Top_0, Item_1, L).


dropdown_menu(Attrs1, Top_0, Item_1, L) -->
  {merge_attributes(Attrs1, [class=dropdown], Attrs2)},
  html(
    li(Attrs2, [
      a([
        'aria-expanded'=false,
        'aria-haspopup'=true,
        class='dropdown-toggle',
        'data-toggle'=dropdown,
        role=button
      ], [
        Top_0,
        \html_caret
      ]),
      ul([class='dropdown-menu'], \html_maplist(html_list_item(Item_1), L))
    ])
  ).

html_list_item(Item_1, X) -->
  html(li(\html_call(Item_1, X))).



%! external_link(+Uri)// is det.
%! external_link(+Uri, :Content_0)// is det.
%! external_link(+Uri, +Attrs, :Content_0)// is det.
%
% Generates an HTML link to an external resource.
%
% When Icon is `true` the fact that the link points to an external
% resource is indicated by a link icon (default is `false`).

external_link(Uri) -->
  external_link(Uri, Uri).


external_link(Uri, Content_0) -->
  external_link(Uri, [], Content_0).


external_link(Uri, Attrs1, Content_0) -->
  {merge_attributes(Attrs1, [href=Uri,target='_blank'], Attrs2)},
  html(a(Attrs2, Content_0)).



%! external_link_icon(+Uri)// is det.

external_link_icon(Uri) -->
  html(a([href=Uri,target='_blank'], \icon(external_link))).



%! favicon(+Spec)// is det.
%
% Generates an HTML link to a favicon.  This icon will show up in a
% Web browser's tab.

favicon(Spec) -->
  {uri_specification(Spec, Uri)},
  html(link([href=Uri,icon=Uri,rel=icon,type='image/x-icon'], [])).



%! flag_icon(+LTag)// is det.

flag_icon(LTag) -->
  {
    file_name_extension(LTag, svg, File),
    directory_file_path(flag_4x3, File, Path)
  },
  html(span(class=[label,'label-primary'], [\flag_icon_img(Path)," ",LTag])).


flag_icon_img(Path) -->
  {
    absolute_file_name(img(Path), _, [access(read)]), !,
    http_absolute_location(img(Path), Location)
  },
  html(span(class='flag-icon', img(src=Location))).
flag_icon_img(_) --> [].



%! form(+Spec, :Content_0)// is det.
%! form(+Spec, +Attrs, :Content_0)// is det.

form(Spec, Content_0) -->
  form(Spec, [], Content_0).


form(Spec, Attrs1, Content_0) -->
  {
    uri_specification(Spec, Uri),
    % By default use the HTTP GET method.
    merge_attrs([method=get], Attrs1, Attrs2),
    % Do not allow the `action' attribute to be overruled.
    merge_attrs(Attrs2, [action=Uri], Attrs3)
  },
  html(form(Attrs3, Content_0)).



%! google_analytics// is det.

google_analytics -->
  {setting(html:google_analytics_id, Id), ground(Id)}, !,
  js_script({|javascript(Id)||
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');
ga('create', Id, 'auto');
ga('send', 'pageview');
  |}).
google_analytics --> [].



%! html_call(:Html_0)// is det.
%! html_call(:Html_1, +Arg1)// is det.

html_call(Html_0, X, Y) :-
  call(Html_0, X, Y).


html_call(Html_1, Arg1, X, Y) :-
  call(Html_1, Arg1, X, Y).



%! html_caret// is det.

html_caret -->
  html(span(class=caret, [])).



%! html_date_time(+Something)// is det.
%! html_date_time(+Something, +Opts)// is det.
%
% Generates human- and machine-readable HTML for date/times.
%
% The following options are supported:
%
%   * ltag(+oneof([en,nl])
%
%     The language tag denoting the natural language that is used to
%     display human-readable content in.  The default is `en`.
%
%   * masks(+list(atom))
%
%     The following masks are supported: `none`, `year`, `month`,
%     `day`, `hour`, `minute`, `second`, `offset`.  The default is
%     `[]`.
%
%   * month_abbr(+boolean)
%
%     Whether the human-readable representation of month names should
%     use abbreviated names or not.  The default is `false`.

html_date_time(Something) -->
  {current_ltag(LTag)}, !,
  html_date_time(Something, _{ltag: LTag}).


html_date_time(Something, Opts) -->
  {
    something_to_date_time(Something, DT),
    html_date_time_machine(DT, MachineString),
    dict_get(masks, Opts, [], Masks),
    date_time_masks(Masks, DT, MaskedDT)
  },
  html(time(datetime=MachineString, \html_date_time_human(MaskedDT, Opts))).



%! html_ellipsis(+String, +MaxLen)// is det.

html_ellipsis(String, MaxLen) -->
  {string_ellipsis(String, MaxLen, Ellipsis)},
  ({String == Ellipsis} -> html(String) ; tooltip(String, Ellipsis)).



%! html_hook(+Term)// is det.
%! html_hook(+Opts, +Term)// is det.

html_hook(Term) -->
  html_hook(_{}, Term).


html_hook(Opts, Term) -->
  html:html_hook(Opts, Term), !.
html_hook(_, Term) -->
  html:html_hook(Term), !.
html_hook(_, Html_0) -->
  html_call(Html_0).

% atom
html:html_hook(A) -->
  {atom(A)}, !,
  html(A).
% code
html:html_hook(code(String)) -->
  html(code(String)).
% empty
html:html_hook(empty) -->
  html([]).
% IRI
html:html_hook(iri(Iri)) -->
  external_link(Iri).
% set
html:html_hook(set(Set)) -->
  html_set(Set).
% string
html:html_hook(String) -->
  {string(String)},
  html(String).
% thousands
html:html_hook(thousands(N)) -->
  html_thousands(N).
% URI
html:html_hook(uri(Uri)) -->
  html:html_hook(iri(Uri)).



%! html_maplist(:Html_1, +Args1) .

html_maplist(_, []) --> !, [].
html_maplist(Html_1, [H|T]) -->
  html_call(Html_1, H),
  html_maplist(Html_1, T).



%! html_nlp_string(+Name)// is det.

html_nlp_string(Name) -->
  {nlp_string(Name, String)},
  html(String).



%! html_page(:Head_0, :Body_0) is det.
%! html_page(+Context, :Head_0, :Body_0) is det.

html_page(Head_0, Body_0) :-
  html_page(cms([]), Head_0, Body_0).


html_page(Context, Head_0, Body_0) :-
  format(current_output, "X-Content-Type-Options: nosniff~n", []),
  format(current_output, "X-Frame-Options: DENY~n", []),
  format(current_output, "X-XSS-Protection: 1; mode=block~n", []),
  reply_html_page(Context, html_page_head0(Head_0), Body_0).

html_page_head0(Head_0) -->
  html(
    head([
      \meta_ie_latest0,
      \meta_viewport0,
      \html_call(Head_0)
    ])
  ).

%! meta_ie_latest0// is det.
%
% Non-standard HTTP-like header that tells Internet Explorer to use
% the most recent version of its rendering engine.

meta_ie_latest0 -->
  html(meta(['http-equiv'='X-UA-Compatible',content='IE=edge'], [])).

%! meta_viewport0// is det.
%
% `width=device-width` instructs the page to match the screen’s width
% in device-independent pixels.  This allows the page to reflow
% content to match different screen sizes.
%
% Some browsers will keep the page’s width constant when rotating to
% landscape mode, and zoom rather than reflow to fill the screen.
% Adding the attribute `initial-scale=1` instructs browsers to
% establish a 1:1 relationship between CSS pixels and
% device-independent pixels regardless of device orientation, and
% allows the page to take advantage of the full landscape width.
%
% `user-scalable=yes` allows a user to zoom in/out on the viewport for
% accessibility.
%
% @compat Use a comma to separate attributes to ensure older browsers
%         can properly parse the attributes.

meta_viewport0 -->
  html(
    meta([
      name="viewport",
      content="width=device-width,initial-scale=1,shrink-to-fit=no"
    ])
  ).



%! html_seplist(:Html_0, :Sep_0)// is det.
%! html_seplist(:Html_1, :Sep_0, +L)// is det.

html_seplist(Html_0, Sep_0) -->
  Html_0,
  Sep_0,
  html_seplist(Html_0, Sep_0).
html_seplist(Html_0, _) --> !,
  Html_0.
html_seplist(_, _) --> !, [].


html_seplist(_, _, []) --> !, [].
html_seplist(Html_1, _, [H]) --> !,
  html_call(Html_1, H).
html_seplist(Html_1, Sep_0, [H1,H2|T]) -->
  html_call(Html_1, H1),
  Sep_0,
  html_seplist(Html_1, Sep_0, [H2|T]).



%! html_set(+Args)// is det.
%! html_set(:Html_1, +Args)// is det.

html_set(Args) -->
  html_set(html_hook, Args).


html_set(Html_1, Args) -->
  html([&(123),\html_seplist(Html_1, html(","), Args),&(125)]).



%! html_space// is det.

html_space -->
  html(span(class=space, [])).



%! html_thousands(+Integer)// is det.

html_thousands(inf) --> !,
  html("∞").
html_thousands(Integer) -->
  html("~:D"-[Integer]).



%! icon(+Name)// is det.

% @tbd Use file `img/pen.svg' instead.
icon(pen) --> !,
  html(
    svg([width=14,height=14,viewBox=[0,0,300,300]],
      path([fill='#777777',d='M253 123l-77-77 46-46 77 77-46 46zm-92-61l77 77s-35 16-46 77c-62 62-123 62-123 62s-24 36-46 15l93-94c55 12 50-39 37-52s-62-21-52 37L7 277c-21-21 15-46 15-46s0-62 62-123c51-5 77-46 77-46z'], [])
    )
  ).
icon(Name) -->
  {icon_class(Name, Class)},
  html(span(class([fa,Class]), [])).



%! icon_button(+Name)// is det.
%! icon_button(+Name, +Func)// is det.

icon_button(Name) -->
  icon_button(Name, _).


icon_button(Name, Func) -->
  {
    icon_class_title(Name, Class, Title),
    (var(Func) -> Attrs = [] ; Attrs = [onclick=Func])
  },
  html(
    button([class=[btn,'btn-default',af,Class],title=Title|Attrs], [])
  ).



%! icon_class(+Name, -Class, -Title) is det.

icon_class(Name, Class) :-
  icon_class_title(Name, Class, _).



%! icon_class_title(+Name, -Class, -Title) is det.

icon_class_title(Name, Class, Title) :-
  icon_table(Name, ClassPostfix, Title),
  atomic_list_concat([fa,ClassPostfix], -, Class).



%! icon_table(?Name, ?Class, ?Title) is nondet.

% CRUD = Create, Read, Update, Delete.
icon_table(cancel,         eraser,          "Cancel").
icon_table(copy,           copy,            "Copy").
icon_table(create,         pencil,          "Create").
icon_table(delete,         trash,           "Delete").
icon_table(download,       download,        "Download").
icon_table(external_link,  'external-link', "Follow link").
icon_table(internal_link,  link,            "Follow link").
icon_table(mail,           envelope,        "Send email").
icon_table(tag,            tag,             "").
icon_table(tags,           tags,            "").
icon_table(time,           'clock-o',       "Date/time").
icon_table(user,           user,            "Log me in").
icon_table(vote_down,     'thumbs-o-down',  "Vote up").
icon_table(vote_up,       'thumbs-o-up',    "Vote down").
icon_table(web,            globe,           "Visit Web site").



%! ignore(:Html_0)// is det.

ignore(Html_0) -->
  html_call(Html_0), !.
ignore(_) --> [].



%! image(+Spec)// is det.
%! image(+Spec, +Attrs)// is det.

image(Spec) -->
  image(Spec, []).


image(Spec, Attrs1) -->
  {
    uri_specification(Spec, Uri),
    merge_attributes(Attrs1, [src=Uri], Attrs2)
  },
  html(img(Attrs2, [])).



%! internal_link(+Spec)// is det.
%! internal_link(+Spec, :Content_0)// is det.
%! internal_link(+Spec, +Attrs, :Content_0)// is det.

internal_link(Spec) -->
  internal_link(Spec, _).


internal_link(Spec, Content_0) -->
  internal_link(Spec, [], Content_0).


internal_link(Spec, Attrs1, Content0_0) -->
  {
    uri_specification(Spec, Uri),
    merge_attributes(Attrs1, [href=Uri], Attrs2),
    (var_goal(Content0_0) -> Content_0 = Uri ; Content_0 = Content0_0)
  },
  html(a(Attrs2, Content_0)).



%! language_menu(+LTags)// is det.

language_menu(LTags) -->
  {
    setting(nlp:lrange, [LTag|_]), ground(LTag), !,
    nlp_string(language, Label)
  },
  navbar_dropdown_menu(
    'language-menu',
    Label,
    language_menu_item(LTag),
    LTags
  ),
  js_script({|javascript(_)||
$( "#language-menu" ).change(function() {
  var str = "";
  $("select option:selected").each(function() {
    $.get("/change_language", {ltag: $(this).val()});
  });
});
  |}).
language_menu(_) --> [].

language_menu_item(LTag0, LTag) -->
  {(LTag0 == LTag -> T = [selected=selected] ; T = [])},
  html(option([value=LTag|T], \html_nlp_string(LTag))).



%! mail_icon(+Uri)// is det.

mail_icon(Uri) -->
  external_link(Uri, [property='foaf:mbox'], [" ",\icon(mail)]).



%! meta_authors// is det.

meta_authors -->
  {
    findall(String, html:author(String), Strings),
    atomics_to_string(Strings, ",", String)
  },
  html(meta([name="author",content=String], [])).



%! meta_description(+String)// is det.

meta_description(String) -->
  html(meta([name="description",content=String], [])).



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



%! navbar_dropdown_menu(+Name, +Label, :Item_1, +Items)// is det.
%
% @tbd What does `role(search)` do?

navbar_dropdown_menu(Name, Label, Item_1, L) -->
  html(
    form([class=['navbar-form'],id=Name,role=search],
      div(class='form-group', [
        label(for=Name, [Label,": "]),
        select([class=['form-control',selectpicker],id=Name],
          \html_maplist(Item_1, L)
        )
      ])
    )
  ).



%! open_graph(+Key, +Value)// is det.

open_graph(Key0, Val) -->
  {atomic_list_concat([og,Key0], :, Key)},
  html(meta([property=Key,content=Val], [])).



%! pipe// is det.

pipe -->
  html([" ",span(class=pipe, "|")," "]).



%! submit_button// is det.
%! submit_button(:Content_0)// is det.

submit_button -->
  submit_button("Submit").


submit_button(Content_0) -->
  button([type=submit], Content_0).



%! table(:Body_0)// is det.
%! table(:Header_0, :Body_0)// is det.
%! table(:Caption_0, :HeaderRow_0, :Body_0)// is det.

table(Body_0) -->
  table(_, Body_0).


table(Header_0, Body_0) -->
  table(_, Header_0, Body_0).


table(Caption_0, Header_0, Body_0) -->
  html(
    table(class=[block,table,'table-condensed','table-striped'], [
      \table_caption(Caption_0),
      \table_header(Header_0),
      tbody(Body_0)
    ])
  ).



%! table_caption(:Caption_0)// is det.

table_caption(Caption_0) -->
  {var_goal(Caption_0)}, !, [].
table_caption(Caption_0) -->
  html(Caption_0).



%! table_content(:Cell_1, +Rows)// is det.

table_content(Cell_1, [head(HeaderRow)|DataRows]) -->
  table(
    \table_header_row(Cell_1, HeaderRow),
    \html_maplist(table_data_row(Cell_1), DataRows)
  ).



%! table_data_cell(+Term)// is det.
%! table_data_cell(:Cell_1, +Term)// is det.

table_data_cell(Term) -->
  table_data_cell(html_hook, Term).


table_data_cell(Cell_1, Term) -->
  html(td(\html_call(Cell_1, Term))).



%! table_data_row(+Row)// is det.
%! table_data_row(:Cell_1, +Row)// is det.

table_data_row(Row) -->
  table_data_row(html_hook, Row).


table_data_row(Cell_1, Row) -->
  html(tr(\html_maplist(table_data_cell(Cell_1), Row))).



%! table_header(:Header_0)// is det.

table_header(Header_0) -->
  {var_goal(Header_0)}, !, [].
table_header(Header_0) -->
  html(thead(Header_0)).



%! table_header_cell(:Cell_1, +Term)// is det.

table_header_cell(Cell_1, Term) -->
  html(th(\html_call(Cell_1, Term))).



%! table_header_row(+Row)// is det.
%! table_header_row(:Cell_1, +Row)// is det.

table_header_row(Row) -->
  table_header_row(html_hook, Row).


table_header_row(Cell_1, Row) -->
  html(tr(\html_maplist(table_header_cell(Cell_1), Row))).



%! tooltip(+String, :Content_0)// is det.

tooltip(String, Content_0) -->
  html(span(['data-toggle'=tooltip,title=String], Content_0)).



%! vote_down(+Vote:integer)// is det.

% @tbd Show as inactive and selected.
vote_down(Vote) -->
  {Vote < 0}, !,
  icon_button(vote_down).
vote_down(Vote) -->
  {Vote =:= 0}, !,
  icon_button(vote_down).
% @tbd Show as inactive and not selected.
vote_down(Vote) -->
  {Vote > 0}, !,
  icon_button(vote_down).



%! vote_up(+Vote:integer)// is det.

% @tbd Show as inactive and selected.
vote_up(Vote) -->
  {Vote < 0}, !,
  icon_button(vote_up).
vote_up(Vote) -->
  {Vote =:= 0}, !,
  icon_button(vote_up).
% @tbd Show as inactive and not selected.
vote_up(Vote) -->
  {Vote > 0}, !,
  icon_button(vote_up).





% HELPERS %

%! merge_attributes(+Attrs1, +Attrs2, -Attrs3) is det.
%
% Merge two lists of HTML attributes into one.

merge_attributes([], L, L) :- !.
% HTTP attribute with (possibly) multiple values.
merge_attributes([Key=Val1|T1], L2a, [Key=Val3|T3]):-
  attr_multi_value(Key),
  selectchk(Key=Val2, L2a, L2b), !,
  maplist(ensure_list, [Val1,Val2], [Val1L,Val2L]),
  append(Val1L, Val2L, ValL),
  sort(ValL, Val3),
  merge_attributes(T1, L2b, T3).
% HTTP attribute with a single value.
merge_attributes([Key=_|T1], L2a, [Key=Val2|T3]) :-
  selectchk(Key=Val2, L2a, L2b), !,
  merge_attributes(T1, L2b, T3).
merge_attributes([H|T1], L2, [H|T3]):-
  merge_attributes(T1, L2, T3).

attr_multi_value(class).

ensure_list(L, L) :-
  is_list(L), !.
ensure_list(Elem, [Elem]).



%! row_1(:ContentA_0)// is det.
%! row_1(+WidthsA, :ContentA_0)// is det.
%! row_1(+Attrs, +WidthsA, :ContentA_0)// is det.

row_1(ContentA_0) -->
  row_1(12, ContentA_0).


row_1(WidthsA, ContentA_0) -->
  row_1([], WidthsA, ContentA_0).


row_1(Attrs1, WidthsA, ContentA_0) -->
  {
    merge_attributes(Attrs1, [class='container-fluid'], Attrs2),
    widths0(WidthsA, ClassesA)
  },
  html(
    div(Attrs2,
      div(class=row,
        div(class=[col|ClassesA], ContentA_0)
      )
    )
  ).



%! row_3(:ContentA_0, :ContentB_0, :ContentC_0)// is det.
%! row_3(+WidthsA, :ContentA_0, +WidthsB, :ContentB_0,
%!       +WidthsC, :ContentC_0)// is det.
%! row_3(+Attrs, +WidthsA, :ContentA_0, +WidthsB, :ContentB_0,
%!       +WidthsC, :ContentC_0)// is det.

row_3(ContentA_0, ContentB_0, ContentC_0) -->
  row_3(4, ContentA_0, 4, ContentB_0, 4, ContentC_0).


row_3(WidthsA, ContentA_0, WidthsB, ContentB_0, WidthsC, ContentC_0) -->
  row_3([], WidthsA, ContentA_0, WidthsB, ContentB_0, WidthsC, ContentC_0).


row_3(Attrs1, WidthsA, ContentA_0, WidthsB, ContentB_0,
      WidthsC, ContentC_0) -->
  {
    merge_attrs(Attrs1, [class=['container-fluid']], Attrs2),
    maplist(widths, [WidthsA,WidthsB,WidthsC], [ClassesA,ClassesB,ClassesC])
  },
  html(
    div(Attrs2,
      div(class=row, [
        div(class=[col|ClassesA], ContentA_0),
        div(class=[col,middle|ClassesB], ContentB_0),
        div(class=[col|ClassesC], ContentC_0)
      ])
    )
  ).



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

uri_specification(link_to_id(HandleId), Uri) :- !,
  uri_specification(link_to_id(HandleId,[]), Uri).
uri_specification(link_to_id(HandleId,QueryComps), Uri2) :- !,
  http_link_to_id(HandleId, QueryComps, Uri1),
  uri_remove_host(Uri1, Uri2).
uri_specification(Spec, Uri2) :-
  http_absolute_location(Spec, Uri1, []),
  uri_remove_host(Uri1, Uri2).



%! var_goal(@Term) is semidet.
%
% Succeeds for a variable or a module-prefixed variable.

var_goal(X) :-
  var(X), !.
var_goal(_:X) :-
  var(X).



%! widths0(+Widths:or([list(between(1,12)),between(1,12)]),
%!         -Classes:list(atom)) is det.
%! widths0(+Offset:boolean, +Widths:or([list(between(1,12)),between(1,12)]),
%!         -Classes:list(atom)) is det.

widths0(Widths, Classes) :-
  widths0(false, Widths, Classes).


widths0(Offset, Widths, Classes) :-
  is_list(Widths), !,
  maplist(width0(Offset), [xs,sm,md,lg,xl], Widths, Classes).
widths0(Offset, Width, Classes) :-
  widths0(Offset, [Width,Width,Width,Width,Width], Classes).

%! width0(+Offset:boolean, +Mode:oneof([lg,md,sm,xs]),
%!        +Widths:or([list(between(1,12)),between(1,12)]), -Class:atom) is det.

width0(Offset, Mode, Width, Class) :-
  (   Offset == true
  ->  Comps = [col,Mode,offset,Width]
  ;   Comps = [col,Mode,Width]
  ),
  atomic_list_concat(Comps, -, Class).
