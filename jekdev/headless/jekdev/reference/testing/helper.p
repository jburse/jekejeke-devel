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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekdev/reference/testing)).

:- module(helper, []).

:- use_module(library(basic/lists)).
:- use_module(library(system/uri)).
:- use_module(library(system/locale)).
:- use_module(library(system/file)).
:- use_module(library(stream/xml)).
:- use_module(library(inspection/notation)).
:- use_module(library(inspection/base)).
:- use_module(runner).
:- use_module(tracker).
:- sys_load_resource(testing).

/***************************************************************/
/* Collecting Statistics                                       */
/***************************************************************/

% sys_add_oknok(+OkNok, +OkNok, -OkNok)
sys_add_oknok(A-B, C-D, E-F) :-
   E is A+C,
   F is B+D.

% sys_sum_oknok(+List, -OkNok)
sys_sum_oknok([X|Y], Z) :-
   sys_sum_oknok(Y, H),
   sys_add_oknok(X, H, Z).
sys_sum_oknok([], 0-0).

/***************************************************************/
/* Report Files                                                */
/***************************************************************/

% report_begin_html(+Atom, +Atom, -Stream)
report_begin_html(P, T, Y) :-
   current_output(Y),
   open(P, write, X),
   set_output(X),
   write('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">'), nl,
   write('<html author=''7''>'), nl,
   write('  <head>'), nl,
   write('    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">'), nl,
   write('    <title editable="comment">'),
   html_escape(T),
   write('</title>'), nl,
   write('  </head>'), nl,
   write('  <body>'), nl.

% report_end_html(+Stream)
report_end_html(Y) :-
   write('  </body>'), nl,
   write('</html>'), nl,
   current_output(X),
   close(X),
   set_output(Y).

/***************************************************************/
/* String Evaluation                                           */
/***************************************************************/

% html_functor_type(+Integer)
html_functor_type(A) :-
   A < 0, !,
   get_properties(testing, P),
   get_property(P, 'result.item.eval', V1),
   html_escape(V1).
html_functor_type(_) :-
   get_properties(testing, P),
   get_property(P, 'result.item.pred', V1),
   html_escape(V1).

/***************************************************************/
/* Zebra Tables                                                */
/***************************************************************/

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
   write('    <td style="text-align: center;">'),
   write('<div class="perfrom" style=''width: '),
   write(A1),
   write('%''>&#8203;</div>'),
   write('<div class="perto" style=''width: '),
   write(B1),
   write('%''>&#8203;</div>'),
   write('<div class="perover">'),
   write(A1),
   write('%</div>'),
   write('</td>'), nl.
