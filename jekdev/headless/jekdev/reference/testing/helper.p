/**
 * Helpers for the report generators in the package testing.
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekdev/reference/testing)).

:- module(helper, []).
:- use_module(library(basic/lists)).
:- use_module(library(system/uri)).
:- use_module(library(system/xml)).
:- use_module(library(system/locale)).

:- op(700, xfx, is_atom).

/**
 * A is_atom E:
 * The predicate succeeds when A unifies with the value of E.
 */
% -Atom is_atom +Expr
_ is_atom A :-
   var(A),
   throw(error(instantiation_error,_)).
X is_atom A+B :- !,
   Y is_atom A,
   Z is_atom B,
   atom_concat(Y, Z, X).
X is_atom encode(A) :- !,
   Y is_atom A,
   uri_encode(Y, X).
X is_atom escape(A) :- !,
   Y is_atom A,
   text_escape(Y, X).
X is_atom uri(A,B) :- !,
   Y is_atom A,
   Z is_atom B,
   make_uri(Y, '', Z, X).
X is_atom atom_format(F,C) :- !,
   atom_format(F, [C], X).

X is_atom indicator(M:F,A) :- !,
   X is_atom package(M)+ : +F+ / +A.
X is_atom indicator(F,A) :- !,
   X is_atom F+ / +A.
X is_atom package(A/B) :- !,
   X is_atom package(A)+ / +B.
X is_atom package(A) :- !,
   X is_atom A.

X is_atom Y :-
   number(Y), !,
   number_codes(Y, H),
   atom_codes(X, H).
X is_atom X.

/**
 * write_atom(E):
 * The predicate succeeds in writing the value of E to the current output.
 */
% write_atom(+Expr)
write_atom(A) :-
   X is_atom A,
   write(X).

/**
 * numbered_solution(G, N):
 * The predicate succeeds whenever G succeeds numbering the solutions in N.
 */
% numbered_solution(+Goal, -Integer)
:- meta_predicate numbered_solution(0,?).
numbered_solution(Goal, Index) :-
   term_variables(Goal, Vars),
   findall(Vars, Goal, List),
   nth1(Index, List, Vars).

% report_begin_html(+Atom, +Atom)
report_begin_html(P, T) :-
   open(P, write, X),
   set_output(X),
   write('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">'), nl,
   write('<html author=''7''>'), nl,
   write('  <head>'), nl,
   write('    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">'), nl,
   write('    <title editable="comment">'),
   write_atom(escape(T)),
   write('</title>'), nl,
   write('  </head>'), nl,
   write('  <body>'), nl.

% report_end_html
report_end_html :-
   write('  </body>'), nl,
   write('</html>'), nl,
   current_output(X),
   current_prolog_flag(sys_disp_output, Y),
   set_output(Y),
   close(X).

% html_zebra_row(+Integer)
html_zebra_row(N) :-
   1 =:= N mod 2, !,
   write('  <tr class="normrow">'), nl.
html_zebra_row(_) :-
   write('  <tr class="oddrow">'), nl.

% html_pairs_data(+Pair)
html_pairs_data(A-B) :-
   write('    <td style="text-align: right;">'),
   write(A),
   write('</td>'), nl,
   write('    <td style="text-align: right;">'),
   write(B),
   write('</td>'), nl,
   S is A+B,
   A1 is (A*200+S)//(2*S),
   B1 is 100-A1,
   write('    <td style="text-align: left;">'),
   write('<div style=''float:left; width: '),
   write_atom(A1),
   write('%; background-color:#50D050''>&#8203;</div>'),
   write('<div style=''float:left; width: '),
   write_atom(B1),
   write('%; background-color:#D05050''>&#8203;</div>'),
   write('</td>'), nl.

% html_functor_indicator(+Atom, +Integer)
html_functor_indicator(F, A) :-
   A < 0, !,
   A2 is -A-1,
   write_atom(escape(indicator(F,A2))).
html_functor_indicator(F, A) :-
   write_atom(escape(indicator(F,A))).

% html_functor_type(+Integer)
html_functor_type(A) :-
   A < 0, !,
   sys_get_lang(testing, P),
   get_property(P, 'result.item.eval', V1),
   write_atom(escape(V1)).
html_functor_type(_) :-
   sys_get_lang(testing, P),
   get_property(P, 'result.item.pred', V1),
   write_atom(escape(V1)).
