/**
 * This module allows the batch reporting of test results. Beforehand
 * the module runner needs to be used to produce the test results. The
 * predicate result_batch/1 can then be used to generate a number of
 * files that list and summarize the results in HTML format. The reporting
 * tool makes an additional assumption about the suite names:
 *
 * suite --> package "_" module.
 *
 * The first level HTML page will thus present the results grouped by
 * packages. The second level HTML page will thus present the results of
 * a package grouped by modules. The current implementation shows success
 * and failure counts not only as numbers but also as coloured bars.
 * Furthermore links to the original test cases will be generated.
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

:- module(result, []).

:- use_module(library(basic/lists)).
:- use_module(library(system/locale)).
:- use_module(library(system/zone)).
:- use_module(library(advanced/sequence)).
:- use_module(runner).
:- use_module(helper).
:- sys_load_resource(testing).

/**
 * result_batch(R):
 * The predicate generates a number of files into the location pointed
 * by the base_url Prolog flag. Links to the test cases are generated
 * relative to the argument R.
 */
% result_batch(+RelUrl)
:- public result_batch/1.
result_batch(Z) :- result_summary, result_packages,
   result_suites(Z).

/*************************************************************/
/* HTML Result Summary                                       */
/*************************************************************/

% result_summary
:- private result_summary/0.
result_summary :-
   write('Generating '),
   write('.'), nl,
   sys_get_lang(testing, P),
   get_property(P, 'result.summary.title', V),
   setup_call_cleanup(report_begin_html('package.html', V, Y),
      html_list_summary,
      report_end_html(Y)).

% html_list_summary
:- private html_list_summary/0.
html_list_summary :-
   write('<h1 date='''),
   get_time(S),
   write_atom(format_atom('%1$tF %1$tT',S)),
   sys_get_lang(testing, P),
   get_property(P, 'result.summary.h1', V1),
   write('''>'),
   write_atom(escape(V1)),
   write('</h1>'), nl,
   call_nth(bagof(N, U^result_suite_view(D, N, U), L), I),
   findall(W, (  member(N, L),
                 result_suite_view(D, N, W)), V),
   sys_sum_oknok(V, Z),
   R is_atom '0'+I+'_'+D+'/package.html',
   write('<a name="'),
   write_atom(escape(encode(D))),
   write('"></a>'),
   get_property(P, 'result.summary.h2', V2),
   write('<h2>'),
   write_atom(escape(V2)),
   write(' '),
   write_atom(escape(D)),
   write('</h2>'), nl,
   write('<table class="rowtable">'), nl,
   write('  <tr class="headrow">'), nl,
   get_property(P, 'result.summary.table.1', V3),
   write('  <th style="width: 20em">'),
   write_atom(escape(V3)),
   write('</th>'), nl,
   get_property(P, 'result.summary.table.2', V4),
   write('  <th style="width: 4em">'),
   write_atom(escape(V4)),
   write('</th>'), nl,
   get_property(P, 'result.summary.table.3', V5),
   write('  <th style="width: 4em">'),
   write_atom(escape(V5)),
   write('</th>'), nl,
   get_property(P, 'result.summary.table.4', V6),
   write('  <th style="width: 12em">'),
   write_atom(escape(V6)),
   write('</th>'), nl,
   write('  </tr>'), nl,
   html_list_element(R, D, L),
   write('  <tr class="headrow">'), nl,
   write('  <td>Total</td>'), nl,
   html_pairs_data(Z),
   write('  </tr>'), nl,
   write('</table>'), nl, fail.
html_list_summary.

% html_list_element(+Atom, +Atom, +List)
:- private html_list_element/3.
html_list_element(R, D, L) :-
   call_nth(member(N, L), Z),
   result_suite_view(D, N, P),
   html_zebra_row(Z),
   write('  <td><a href="'),
   write_atom(escape(encode(uri(R,N)))),
   write('">'),
   write_atom(escape(N)),
   write('</a></td>'), nl,
   html_pairs_data(P),
   write('  </tr>'), nl, fail.
html_list_element(_, _, _).

/*************************************************************/
/* HTML Packages                                             */
/*************************************************************/

% result_packages.
:- private result_packages/0.
result_packages :-
   call_nth(bagof(N, U^result_suite_view(D, N, U), L), I),
   Q is_atom '0'+I+'_'+D+'/package.html',
   write('Generating '),
   write('.'/D), nl,
   setup_call_cleanup(report_begin_html(Q, D, Y),
      html_list_package(D, L),
      report_end_html(Y)), fail.
result_packages.

% html_list_package(+Atom, +List)
:- private html_list_package/2.
html_list_package(D, L) :-
   write('<h1 date='''),
   get_time(S),
   write_atom(format_atom('%1$tF %1$tT',S)),
   sys_get_lang(testing, P),
   get_property(P, 'result.package.h1', V1),
   write('''>'),
   write_atom(escape(V1)),
   write(' '),
   write_atom(escape(D)),
   write('</h1>'), nl,
   call_nth(member(N, L), J),
   T is_atom D+'_'+N,
   result_suite(T, U),
   R is_atom '0'+J+'_'+N+'.html',
   write('<a name="'),
   write_atom(escape(encode(N))),
   write('"></a>'),
   get_property(P, 'result.package.h2', V2),
   write('<h2>'),
   write_atom(escape(V2)),
   write(' '),
   write_atom(escape(N)),
   write('</h2>'), nl,
   write('<table class="rowtable">'), nl,
   write('  <tr class="headrow">'), nl,
   get_property(P, 'result.package.table.1', V3),
   write('  <th style="width: 20em">'),
   write_atom(escape(V3)),
   write('</th>'), nl,
   get_property(P, 'result.package.table.2', V4),
   write('  <th style="width: 4em">'),
   write_atom(escape(V4)),
   write('</th>'), nl,
   get_property(P, 'result.package.table.3', V5),
   write('  <th style="width: 4em">'),
   write_atom(escape(V5)),
   write('</th>'), nl,
   get_property(P, 'result.package.table.4', V6),
   write('  <th style="width: 12em">'),
   write_atom(escape(V6)),
   write('</th>'), nl,
   write('  </tr>'), nl,
   html_list_member(R, T),
   write('  <tr class="headrow">'), nl,
   write('  <td>Total</td>'), nl,
   html_pairs_data(U),
   write('  </tr>'), nl,
   write('</table>'), nl, fail.
html_list_package(_, _).

% html_list_member(+Atom, +Atom)
:- private html_list_member/2.
html_list_member(R, T) :-
   call_nth(result_predicate(F, A, T, P), Z),
   html_zebra_row(Z),
   write('  <td><a href="'),
   write_atom(escape(encode(uri(R,indicator(F,A))))),
   write('">'),
   html_functor_indicator(F, A),
   write('</a></td>'), nl,
   html_pairs_data(P),
   write('  </tr>'), nl, fail.
html_list_member(_, _).

/*************************************************************/
/* HTML Suites                                               */
/*************************************************************/

% result_suites(+RelUrl)
:- private result_suites/1.
result_suites(Z) :-
   call_nth(bagof(N, U^result_suite_view(D, N, U), L), I),
   call_nth(member(N, L), J),
   T is_atom D+'_'+N,
   P is_atom '0'+I+'_'+D+ / +'0'+J+'_'+N+'.html',
   write('Generating '),
   write('.'/D/N), nl,
   setup_call_cleanup(report_begin_html(P, N, Y),
      html_list_suite(T, N, Z),
      report_end_html(Y)), fail.
result_suites(_).

% html_list_suite(+Atom, +Atom, +RelUrl)
:- private html_list_suite/3.
html_list_suite(T, N, Z) :-
   write('<h1 date='''),
   get_time(S),
   write_atom(format_atom('%1$tF %1$tT',S)),
   sys_get_lang(testing, P),
   get_property(P, 'result.suite.h1', V1),
   write('''>'),
   write_atom(escape(V1)),
   write(' '),
   write_atom(escape(N)),
   write('</h1>'), nl,
   result_predicate(F, A, T, U),
   write('<a name="'),
   write_atom(escape(encode(indicator(F,A)))),
   write('"></a>'),
   write('<h2>'),
   html_functor_type(A),
   write(' '),
   html_functor_indicator(F, A),
   write('</h2>'), nl,
   write('<table class="rowtable">'), nl,
   write('  <tr class="headrow">'), nl,
   get_property(P, 'result.suite.table.1', V2),
   write('  <th style="width: 20em">'),
   write_atom(escape(V2)),
   write('</th>'), nl,
   get_property(P, 'result.suite.table.2', V3),
   write('  <th style="width: 4em">'),
   write_atom(escape(V3)),
   write('</th>'), nl,
   get_property(P, 'result.suite.table.3', V4),
   write('  <th style="width: 4em">'),
   write_atom(escape(V4)),
   write('</th>'), nl,
   get_property(P, 'result.suite.table.4', V5),
   write('  <th style="width: 12em">'),
   write_atom(escape(V5)),
   write('</th>'), nl,
   write('  </tr>'), nl,
   html_list_predicate(F, A, T, Z),
   write('  <tr class="headrow">'), nl,
   write('  <td>Total</td>'), nl,
   html_pairs_data(U),
   write('  </tr>'), nl,
   write('</table>'), nl, fail.
html_list_suite(_, _, _).

% html_list_predicate(+Atom, +Integer, +Atom, +RelUrl)
:- private html_list_predicate/4.
html_list_predicate(F, A, T, Z) :-
   split_suite(T, D, M),
   R is_atom Z+D+ / +M+'.html',
   call_nth(result(F, A, T, N, P), I),
   html_zebra_row(I),
   write('  <td><a href="'),
   write_atom(escape(encode(uri(R,N)))),
   write('">'),
   write_atom(escape(N)),
   write('</a></td>'), nl,
   html_pairs_data(P),
   write('  </tr>'), nl, fail.
html_list_predicate(_, _, _, _).
