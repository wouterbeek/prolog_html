:- module(
  html_date_time_human,
  [
    html_date_time_human//2, % +Datetime, +Options
    html_today_human//1      % +Options
  ]
).

/** <module> Human-readable HTML date/time formats

DCG rules for parsing/generating human-readable HTML5 dates.

@author Wouter Beek
@version 2017/05, 2017/08
*/

:- use_module(library(apply)).
:- use_module(library(date_time)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(nlp/nlp_lang)).





%! html_date_time_human(+Datetime:dt, +Options:list(compound))// is det.

html_date_time_human(dt(Y,Mo,Da,H,Mi,S,Off), Options) -->
  (   {ground(date(Y,Mo,Da,H,Mi,S,Off))}
  ->  global_date_and_time(Y, Mo, Da, H, Mi, S, Off, Options)
  ;   {ground(date(Y,Mo,Da,H,Mi,S))}
  ->  floating_date_and_time(Y, Mo, Da, H, Mi, S, Options)
  ;   {ground(date(Y,Mo,Da))}
  ->  date(Y, Mo, Da, Options)
  ;   {ground(date(H,Mi,S))}
  ->  time(H, Mi, S, Options)
  ;   {ground(date(Mo,Da))}
  ->  yearless_date(Mo, Da, Options)
  ;   {ground(date(Y,Mo))}
  ->  month(Y, Mo, Options)
  ;   {ground(date(Y))}
  ->  year(Y, Options)
  ;   {ground(date(Off))}
  ->  timezone_offset(Off)
  ).



%! html_today_human(+Options:list(compound))// is det.

html_today_human(Options) -->
  {now(DT)},
  html_date_time_human(DT, Options).



%! date(+Year:integer, +Month:between(1,12), +Day:between(1,31),
%!      +Options:list(compound))// is det.

date(Y, Mo, Da, Options) -->
  html([\month_day(Da, Options)," ",\month(Y, Mo, Options)]).



%! floating_date_and_time(+Year:integer, +Month:between(1,12),
%!                        +Day:between(1,31), +Hour:between(0,24),
%!                        +Minute:between(0,59), +Second:float,
%!                        +Options:list(compound))// is det.

floating_date_and_time(Y, Mo, Da, H, Mi, S, Options) -->
  html([\date(Y, Mo, Da, Options)," ",\time(H, Mi, S, Options)]).



%! global_date_and_time(+Year:integer, +Month:between(1,12),
%!                      +Day:between(1,31), +Hour:between(0,24),
%!                      +Minute:between(0,59), +Second:float,
%!                      +Offset:between(-840,840),
%!                      +Options:list(compound))// is det.

global_date_and_time(Y, Mo, Da, H, Mi, S, Off, Options) -->
  floating_date_and_time(Y, Mo, Da, H, Mi, S, Options),
  timezone_offset(Off).



%! hour(+Hour:between(0,24), +Options:list(compound))// is det.

hour(H, _) -->
  html(span(class=hour, [\padding_zero(H),H])).



%! minute(+Minute:between(0,59), +Options:list(compound))// is det.

minute(Mi, _) -->
  html(span(class=minute, [\padding_zero(Mi),Mi])).



%! month(+Month:between(1,12), +Options:list(compound))// is det.

month(Mo, Options) -->
  {
    dict_get(ltag, Options, en, LTag),
    dict_get(month_abbr, Options, false, IsAbbr),
    once(month_name(Mo, LTag, Abbr, Full)),
    (IsAbbr == true -> Month = Abbr ; Month = Full)
  },
  html(span(class=month, Month)).



%! month(+Year:integer, +Month:between(1,12),
%!       +Options:list(compound))// is det.

month(Y, Mo, Options) -->
  html([\month(Mo, Options)," ",\year(Y, Options)]).



%! month_day(+Day:between(1,31), +Options:list(compound)) is det.

month_day(Da, Options) -->
  html(span(class='month-day', \month_day_inner(Da, Options))).

month_day_inner(Da, Options) -->
  {dict_get(ltag, Options, nl)}, !,
  html(Da).
month_day_inner(Da, Options) -->
  ordinal(Da, Options).



%! ordinal(+N:nonneg, +Options:list(compound))// is det.

ordinal(N, Options) -->
  {
    dict_get(ltag, Options, en, LTag),
    ordinal_suffix(N, LTag, Suffix)
  },
  html([N, sup([], Suffix)]).



%! second(+Second:float, +Options:list(compound))// is det.

second(S0, _) -->
  {S is floor(S0)},
  html(span(class=second, [\padding_zero(S),S])).



%! sign(+N:number)// is det.

sign(N) -->
  {N < 0}, !,
  html("-").
sign(_)  -->
  html("+").



%! time(+Hour:between(0,24), +Minute:between(0,59), +Second:float,
%!      +Options:list(compound))// is det.

time(H, Mi, S, Options) -->
  html(
    span(class=time, [
      \hour(H, Options),
      ":",
      \minute(Mi, Options),
      ":",
      \second(S, Options)
    ])
  ).



%! timezone_offset(+Offset:between(-840,840))// is det.

timezone_offset(Off) -->
  html(span(class='timezone-offset', \timezone_offset_(Off))).

timezone_offset_(0) --> !,
  html("Z").
timezone_offset_(Off) -->
  {
    H is Off // 60,
    dcg_with_output_to(string(H0), generate_as_digits(H, 2)),
    Mi is Off mod 60,
    dcg_with_output_to(string(Mi0), generate_as_digits(Mi, 2))
  },
  html([\sign(Off),H0,":",Mi0]).



%! year(+Year:integer, +Options:list(compound))// is det.

year(Y, _) -->
  html(span(class=year, Y)).



%! yearless_date(+Month:between(1,12), +Day:between(1,31),
%!               +Options:list(compound))// is det.

yearless_date(Mo, Da, Options) -->
  html(
    span(class='yearless-date', [
      \month(Mo, Options),
      \month_day(Da, Options)
    ])
  ).





% HELPERS %

%! padding_zero(+N:between(0,9))// is det.

padding_zero(N) -->
  {N =< 9}, !,
  html("0").
padding_zero(_) --> [].
