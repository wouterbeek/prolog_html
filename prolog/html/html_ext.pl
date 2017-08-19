:- module(
  html_ext,
  [
    button//2,             % +Attributes, :Content_0
    deck//2,               % :Card_1, +Items
    deck//3,               % +Attributes, :Card_1, +Items
    dropdown_menu//3,      % :Top_0, :Item_1, +Items
    dropdown_menu//4,      % +Attributes, :Top_0, :Item_1, +Items
    external_link//1,      % +Uri
    favicon//0,
    flag_icon//1,          % +LanguageTag
    footer_panel//3,       % +Image, :Top_0, :Bottom_0
    google_analytics//0,
    html_call//1,          % :Html_0
    html_call//2,          % :Html_1, +Arg1
    html_date_time//1,     % +Something
    html_date_time//2,     % +Something, +Options
    html_ellipsis//2,      % +String, +MaxLen
    html_if_then//2,       % :If_0, :Then_0
    html_if_then_else//3,  % :If_0, :Then_0, :Else_0
    html_maplist//2,       % :Html_1, +Args1
    html_nlp_string//1,    % +Name
    html_page/2,           % :Head_0, :Body_0
    html_page/3,           % +Context, :Head_0, :Body_0
    html_page_head//0,
    html_seplist//2,       % :Html_0, :Sep_0
    html_seplist//3,       % :Html_1, :Sep_0, +Args
    html_set//1,           % +Args
    html_set//2,           % :Html_1, +Args
    html_site_init/1,      % +Dict
    html_space//0,
    html_thousands//1,     % +Integer
    icon//1,               % +Name
    icon_button//1,        % +Name
    icon_button//2,        % +Name, +Func
    ignore//1,             % :Html_0
    language_menu//1,      % +LanguageTags
    logo/1,                % -Image
    mail_icon//1,          % +Uri
    mail_link_and_icon//1, % +Uri
    menu//0,
    meta_authors//0,
    meta_description//1,   % +Desc
    meta_ie_latest//0,
    meta_viewport//0,
    navbar//3,             % :Brand_0, :Menu_0, :Right_0
    open_graph//2,         % +Key, +Value
    pipe//0,
    row_1//1,              % :ContentA_0
    row_1//2,              % +WidthsA, :ContentA_0
    row_1//3,              % +Attributes, +WidthsA, :ContentA_0
    row_3//3,              % :ContentA_0, :ContentB_0, :ContentC_0
    row_3//6,              % +WidthsA, :ContentA_0, +WidthsB, :ContentB_0,
                           % +WidthsC, :ContentC_0
    row_3//7,              % +Attributes, +WidthsA, :ContentA_0, +WidthsB
                           % :ContentB_0, +WidthsC, :ContentC_0
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
    title//1,              % +Strings
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
@version 2017/04-2017/08
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(date_time)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_date_time_human)).
:- use_module(library(html/html_date_time_machine)).
:- use_module(library(http/http_server)).
:- use_module(library(http/jquery)).
:- use_module(library(lists)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(pairs)).
:- use_module(library(settings)).
:- use_module(library(string_ext)).
:- use_module(library(uri/uri_ext)).

:- dynamic
    html:menu_item/3,
    html:menu_item/4.

:- html_meta
   button(+, html, ?, ?),
   dropdown_menu(html, :, +, ?, ?),
   dropdown_menu(+, html, :, +, ?, ?),
   footer_panel(+, html, html, ?, ?),
   html_call(html, ?, ?),
   html_if_then(0, html, ?, ?),
   html_if_then_else(0, html, html, ?, ?),
   html_page(html, html),
   html_page(+, html, html),
   html_seplist(html, html, ?, ?),
   html_seplist(3, html, +, ?, ?),
   ignore(html, ?, ?),
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
         requires([jquery,popper,tether,js('bootstrap.js')]),
         virtual(true)
       ]
     ).
:- else.
  :- html_resource(
       js(bootstrap),
       [
         ordered(true),
         requires([jquery,popper,tether,js('bootstrap.min.js')]),
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

% Popper
:- if(debugging(js(popper))).
  :- html_resource(
       js(popper),
       [requires([js('popper.js')]),virtual(true)]
     ).
:- else.
  :- html_resource(
       js(popper),
       [requires([js('popper.min.js')]),virtual(true)]
     ).
:- endif.
:- html_resource(
     popper,
     [requires([js(popper)]),virtual(true)]
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
    html_if_then(0, 2, ?, ?),
    html_if_then_else(0, 2, 2, ?, ?),
    html_list_item(3, +, ?, ?),
    html_maplist(3, +, ?, ?),
    html_set(3, +, ?, ?),
    table_content(3, +, ?, ?),
    table_data_cell(3, +, ?, ?),
    table_data_row(3, +, ?, ?),
    table_header_cell(3, +, ?, ?),
    table_header_row(3, +, ?, ?).

%! html:menu_item(?Major, ?Name, ?Label) is nondet.
%
% Adds a top-level menu item to the menu.  The menu item has a rank
% Major, an internal Name and a user-visible label Label.

%! html:menu_item(?Name, ?Minor, ?Uri, ?Label) is nondet.
%
% Adds a menu-item under a top-level menu item with the given internal
% Name.  Minor denotes the rank within the top-level menu item.

:- multifile
    html:author/1,
    html:menu_item/3,
    html:menu_item/4,
    html:html_hook//1,
    html:html_hook//2.

% jQuery
:- set_setting(jquery:version, '3.2.1.min').

:- setting(
     html:google_analytics_id,
     any,
     _,
     "Google Analytics ID."
   ).
:- setting(
     html:logo,
     atom,
     logo,
     "The base name of the website logo."
   ).





%! button(+Attributes:list(compound), :Content_0)// is det.

button(Attributes1, Content_0) -->
  {merge_attributes([class=[btn,'btn-default']], Attributes1, Attributes2)},
  html(button(Attributes2, Content_0)).



%! deck(:Card_1, +Items:list)// is det.
%! deck(+Attributes, :Card_1, +Items:list)// is det.

deck(Card_1, L) -->
  deck([], Card_1, L).


deck(Attributes1, Card_1, L) -->
  {merge_attributes([class=['card-columns']], Attributes1, Attributes2)},
  html(div(Attributes2, \html_maplist(Card_1, L))).



%! dropdown_menu(:Top_0, :Item_1, +Items:list)// is det.
%! dropdown_menu(+Attributes, :Top_0, :Item_1, +Items:list)// is det.

dropdown_menu(Top_0, Item_1, L) -->
  dropdown_menu([], Top_0, Item_1, L).


dropdown_menu(Attributes1, Top_0, Item_1, L) -->
  {merge_attributes(Attributes1, [class=dropdown], Attributes2)},
  html(
    li(Attributes2, [
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



%! external_link(+Uri:atom)// is det.

external_link(Uri) -->
  html(a([href=Uri,target='_blank'], \icon(external_link))).



%! favicon// is det.
%
% Generates an HTML link to a favicon.  This icon will show up in a
% Web browser's tab.

favicon -->
  {logo(Image)},
  html(link([href=Image,icon=Image,rel=icon,type='image/x-icon'], [])).



%! flag_icon(+LanguageTag)// is det.

flag_icon(LanguageTag) -->
  {
    file_name_extension(LanguageTag, svg, File),
    directory_file_path(flag_4x3, File, Path)
  },
  html(span(class=[label,'label-primary'], [\flag_icon_img(Path)," ",LanguageTag])).


flag_icon_img(Path) -->
  {
    absolute_file_name(img(Path), _, [access(read)]), !,
    http_absolute_location(img(Path), Location)
  },
  html(span(class='flag-icon', img(src=Location))).
flag_icon_img(_) --> [].



%! footer_panel(+Image, :Top_0, :Bottom_0)// is det.

footer_panel(Image, Top_0, Bottom_0) -->
  html([
    div(style='display: table;', [
      div([class='footer-logo',style='display: table-cell;'],
        a(href='/', img([height=60,src=Image,style='max-height: 60px;'], []))
      ),
      div(class='brand-txt', a(href='/', Top_0))
    ]),
    Bottom_0
  ]).



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



%! html_date_time(+Datetime:dt)// is det.
%! html_date_time(+Datetime:dt, +Options:list(compound))// is det.
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

html_date_time(Datetime) -->
  {current_ltag(LanguageTag)}, !,
  html_date_time(Datetime, _{ltag: LanguageTag}).


html_date_time(Datetime1, Options) -->
  {
    html_date_time_machine(Datetime1, MachineString),
    dict_get(masks, Options, [], Masks),
    date_time_masks(Masks, Datetime1, Datetime2)
  },
  html(
    time(datetime=MachineString,
      \html_date_time_human(Datetime2, Options)
    )
  ).



%! html_ellipsis(+String:string, +MaxLength:nonneg)// is det.

html_ellipsis(String, MaxLen) -->
  {string_ellipsis(String, MaxLen, Ellipsis)},
  ({String == Ellipsis} -> html(String) ; tooltip(String, Ellipsis)).



%! html_hook(+Term)// is det.
%! html_hook(+Options:list(compound), +Term)// is det.

html_hook(Term) -->
  html_hook(_{}, Term).


html_hook(Options, Term) -->
  html:html_hook(Options, Term), !.
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
  html(a([href=Iri,target='_blank'])).
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



%! html_if_then(:If_0, :Then_0)// is det.

html_if_then(If_0, Then_0) -->
  html_if_then_else(If_0, Then_0, html([])).



%! html_if_then_else(:If_0, :Then_0, :Else_0)// is det.

html_if_then_else(If_0, Then_0, Else_0) -->
  ({call(If_0)} -> html_call(Then_0) ; html_call(Else_0)).



%! html_maplist(:Html_1, +Arguments1:list(compound))// .

html_maplist(_, []) --> !, [].
html_maplist(Html_1, [H|T]) -->
  html_call(Html_1, H),
  html_maplist(Html_1, T).



%! html_nlp_string(+Name:atom)// is det.

html_nlp_string(Name) -->
  {nlp_string(Name, String)},
  html(String).



%! html_page(:Head_0, :Body_0) is det.
%! html_page(+Context, :Head_0, :Body_0) is det.

html_page(Head_0, Body_0) :-
  html_page_,
  reply_html_page(Head_0, Body_0).


html_page(Context, Head_0, Body_0) :-
  html_page_,
  reply_html_page(Context, Head_0, Body_0).

html_page_ :-
  format(current_output, "X-Content-Type-Options: nosniff~n", []),
  format(current_output, "X-Frame-Options: DENY~n", []),
  format(current_output, "X-XSS-Protection: 1; mode=block~n", []).



%! html_page_head// is det.

html_page_head -->
  html([
    \html_root_attribute(lang, en),
    meta(charset='utf-8', []),
    \meta_ie_latest,
    \meta_viewport,
    \favicon
  ]).



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



%! html_site_init(+Dict) is det.

html_site_init(Dict1) :-
  (   dict_get(site, Dict1, Dict2)
  ->  (dict_get(logo, Dict2, Base) -> set_setting(html:logo, Base) ; true),
      dict_get(authors, Dict2, [], Authors),
      maplist(init_author, Authors)
  ;   true
  ),
  (   dict_get(monitor, Dict1, Dict3)
  ->  (   dict_get('google-analytics', Dict3, Dict4)
      ->  set_setting(html:google_analytics_id, Dict4.'profile-id')
      ;   true
      )
  ;   true
  ).

init_author(Author0) :-
  atom_string(Author0, Author),
  assert(html:author(Author)).



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
    (var(Func) -> Attributes = [] ; Attributes = [onclick=Func])
  },
  html(
    button([class=[btn,'btn-default',af,Class],title=Title|Attributes], [])
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



%! language_menu(+LanguageTags)// is det.

language_menu(LanguageTags) -->
  {
    setting(nlp:lrange, [LanguageTag|_]), ground(LanguageTag), !,
    nlp_string(language, Label)
  },
  navbar_dropdown_menu(
    'language-menu',
    Label,
    language_menu_item(LanguageTag),
    LanguageTags
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

language_menu_item(LanguageTag0, LanguageTag) -->
  {(LanguageTag0 == LanguageTag -> T = [selected=selected] ; T = [])},
  html(option([value=LanguageTag|T], \html_nlp_string(LanguageTag))).



%! logo(-Image:atom) is det.

logo(Image) :-
  setting(html:logo, Base),
  file_name_extension(Base, svg, Local),
  http_absolute_location(img(Local), Image).



%! mail_icon(+Uri:atom)// is det.

mail_icon(Uri) -->
  html(a([href=Uri,property='foaf:mbox',target='_blank'], [" ",\icon(mail)])).



%! mail_link_and_icon(+Uri:atom)// is det.

mail_link_and_icon(Uri) -->
  {uri_components(Uri, uri_components(mailto,_,Label,_,_))},
  html(
    a([class=nowrap,href=Uri,property='foaf:mbox'], [
      \icon(mail),
      " ",
      code(Label)
    ])
  ).



%! menu// is det.
%
% This needs to be plugged into navbar//3 for argument Menu_0.

menu -->
  {
    http_current_request(Request),
    memberchk(request_uri(RequestUri), Request),
    major_menus(MajorMenus)
  },
  html_maplist(major_menu(RequestUri), MajorMenus).


% Flat menu item.
major_menu(RequestUri, menu_item(Handle,Label)-[]) --> !,
  {
    http_link_to_id(Handle, Uri),
    (atom_postfix(RequestUri, Uri) -> Classes = [active] ; Classes = [])
  },
  html(
    li(class='nav-item',
      a([class=['nav-link',Handle|Classes],href=Uri], Label)
    )
  ).
% Nested menu items.
major_menu(_, MajorItem-MinorItems) -->
  dropdown_menu(menu_item(MajorItem), menu_item, MinorItems).


major_menus(MajorTrees) :-
  findall(
    Major-menu_item(Handle,Label),
    (html:menu_item(Major, Handle, Label), Handle \== user),
    Pairs
  ),
  sort(1, @=<, Pairs, SortedPairs),
  pairs_values(SortedPairs, MajorNodes),
  maplist(major_node_to_menu, MajorNodes, MajorTrees).


major_node_to_menu(
  menu_item(Handle1,Label1),
  menu_item(Handle1,Label1)-MinorNodes
) :-
  findall(
    Minor-menu_item(Handle2,Label2),
    html:menu_item(Handle1, Minor, Handle2, Label2),
    Pairs
  ),
  sort(1, @=<, Pairs, SortedPairs),
  pairs_values(SortedPairs, MinorNodes).


menu_item(menu_item(HandleId,Label)) -->
  {http_link_to_id(HandleId, [], Uri)},
  html(a(href=Uri, Label)).



%! meta_authors// is det.

meta_authors -->
  {
    findall(String, html:author(String), Strings),
    atomics_to_string(Strings, ",", String)
  },
  html(meta([name="author",content=String], [])).



%! meta_description(+Description:string)// is det.

meta_description(String) -->
  html(meta([name="description",content=String], [])).



%! meta_ie_latest// is det.
%
% Non-standard HTTP-like header that tells Internet Explorer to use
% the most recent version of its rendering engine.

meta_ie_latest -->
  html(meta(['http-equiv'='X-UA-Compatible',content='IE=edge'], [])).



%! meta_viewport// is det.
%
% `width=device-width' instructs the page to match the screen’s width
% in device-independent pixels.  This allows the page to reflow
% content to match different screen sizes.
%
% Some browsers will keep the page's width constant when rotating to
% landscape mode, and zoom rather than reflow to fill the screen.
% Adding the attribute `initial-scale=1' instructs browsers to
% establish a 1:1 relationship between CSS pixels and
% device-independent pixels regardless of device orientation, and
% allows the page to take advantage of the full landscape width.
%
% `user-scalable=yes' allows a user to zoom in/out on the viewport for
% accessibility.
%
% @compat Use a comma to separate attributes to ensure older browsers
%         can properly parse the attributes.

meta_viewport -->
  html(
    meta([
      name="viewport",
      content="width=device-width,initial-scale=1,shrink-to-fit=no"
    ])
  ).



%! navbar(:Brand_0, :Menu_0, :Right_0)// is det.

navbar(Brand_0, Menu_0, Right_0) -->
  html(
    nav(class=['bg-light','fixed-top',navbar,'navbar-expand-lg','navbar-light'], [
      a([class='navbar-brand',href='/'], Brand_0),
      \navbar_toggler,
      div([class=[collapse,'navbar-collapse'],id=target], [
        ul(class=['navbar-nav','mr-auto'], Menu_0),
        ul(class='navbar-nav', Right_0)
      ])
    ])
  ).

navbar_toggler -->
  html(
    button([
      'aria-controls'=target,
      'aria-expanded'=false,
      'aria-label'="Toggle navigation",
      class='navbar-toggler',
      'data-target'='#target',
      'data-toggle'=collapse,
      type=button
    ], span(class='navbar-toggler-icon', []))
  ).



%! navbar_dropdown_menu(+Name:atom, +Label:string, :Item_1,
%!                      +Items:list)// is det.
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



%! open_graph(+Key:atom, +Value:atom)// is det.

open_graph(Key0, Val) -->
  {atomic_list_concat([og,Key0], :, Key)},
  html(meta([property=Key,content=Val], [])).



%! pipe// is det.

pipe -->
  html([" ",span(class=pipe, "|")," "]).



%! row_1(:ContentA_0)// is det.
%! row_1(+WidthsA, :ContentA_0)// is det.
%! row_1(+Attributes:list(compound), +WidthsA, :ContentA_0)// is det.

row_1(ContentA_0) -->
  row_1(12, ContentA_0).


row_1(WidthsA, ContentA_0) -->
  row_1([], WidthsA, ContentA_0).


row_1(Attributes1, WidthsA, ContentA_0) -->
  {
    merge_attributes(Attributes1, [class='container-fluid'], Attributes2),
    widths0(WidthsA, ClassesA)
  },
  html(
    div(Attributes2,
      div(class=row,
        div(class=[col|ClassesA], ContentA_0)
      )
    )
  ).



%! row_3(:ContentA_0, :ContentB_0, :ContentC_0)// is det.
%! row_3(+WidthsA, :ContentA_0, +WidthsB, :ContentB_0,
%!       +WidthsC, :ContentC_0)// is det.
%! row_3(+Attributes, +WidthsA, :ContentA_0, +WidthsB, :ContentB_0,
%!       +WidthsC, :ContentC_0)// is det.

row_3(ContentA_0, ContentB_0, ContentC_0) -->
  row_3(4, ContentA_0, 4, ContentB_0, 4, ContentC_0).


row_3(WidthsA, ContentA_0, WidthsB, ContentB_0, WidthsC, ContentC_0) -->
  row_3([], WidthsA, ContentA_0, WidthsB, ContentB_0, WidthsC, ContentC_0).


row_3(Attributes1, WidthsA, ContentA_0, WidthsB, ContentB_0,
      WidthsC, ContentC_0) -->
  {
    merge_attributes(Attributes1, [class=['container-fluid']], Attributes2),
    maplist(widths0, [WidthsA,WidthsB,WidthsC], [ClassesA,ClassesB,ClassesC])
  },
  html(
    div(Attributes2,
      div(class=row, [
        div(class=[col|ClassesA], ContentA_0),
        div(class=[col,middle|ClassesB], ContentB_0),
        div(class=[col|ClassesC], ContentC_0)
      ])
    )
  ).



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



%! table_content(:Cell_1, +Rows:list)// is det.

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



%! table_data_row(+Row:list)// is det.
%! table_data_row(:Cell_1, +Row:list)// is det.

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



%! table_header_row(+Row:list)// is det.
%! table_header_row(:Cell_1, +Row:list)// is det.

table_header_row(Row) -->
  table_header_row(html_hook, Row).


table_header_row(Cell_1, Row) -->
  html(tr(\html_maplist(table_header_cell(Cell_1), Row))).



%! title(+Stings:list(string))// is det.

title(Strings) -->
  {atomics_to_string(Strings, " ⎯ ", String)},
  html(title(String)).



%! tooltip(+String:string, :Content_0)// is det.

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

%! merge_attributes(+Attributes1:list(compound), +Attributes2:list(compound),
%!                  -Attributes3:list(compound)) is det.
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
