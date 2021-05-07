/**
 * This module allows the batch reporting of a summary. The report
 * shows the suite summary of the test results and the package
 * summary of the coverage analysis. Beforehand the module runner
 * needs to be used to produce the test results and the module
 * tracker needs to be used to produce the coverage analysis.
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

:- module(summary, []).

:- use_module(library(basic/lists)).
:- use_module(library(system/locale)).
:- use_module(library(system/zone)).
:- use_module(library(advanced/sequence)).
:- use_module(library(stream/xml)).
:- use_module(library(system/uri)).
:- use_module(library(advanced/signal)).
:- use_module(runner).
:- use_module(tracker).
:- use_module(helper).
:- sys_load_resource(testing).

/**
 * summary_batch:
 * summary_batch(C):
 * The predicate generates a file into the location pointed
 * by the base_url Prolog flag. The unary predicate allows
 * specifying the coverage flag C.
 */
% summary_batch
:- public summary_batch/0.
summary_batch :-
   summary_batch(true).

% summary_batch(+Atom)
:- public summary_batch/1.
summary_batch(C) :- var(C),
   throw(error(instantiation_error, _)).
summary_batch(C) :-
   write('Generating '), write('.'), nl,
   get_properties(testing, P),
   get_property(P, 'summary.summary.title', V),
   setup_call_cleanup(report_begin_html('06_summary.html', V, Y),
      html_page(C),
      report_end_html(Y)).

% html_page(+Atom)
:- private html_page/1.
html_page(true) :- !,
   write('<h1 date='''), get_time(T),
   format_atom('%1$tF %1$tT', [T], TStr), write(TStr),
   get_properties(testing, P),
   get_property(P, 'summary.cover_and_result.h1', V1),
   write('''>'), html_escape(V1), write('</h1>'), nl,
   html_cover_list,
   html_result_list.
html_page(_) :-
   write('<h1 date='''), get_time(T),
   format_atom('%1$tF %1$tT', [T], TStr), write(TStr),
   get_properties(testing, P),
   get_property(P, 'summary.result.h1', V1),
   write('''>'), html_escape(V1), write('</h1>'), nl,
   html_result_list.

/*************************************************************/
/* Result Summary                                            */
/*************************************************************/

% html_result_list
:- private html_result_list/0.
html_result_list :-
   get_properties(testing, P),
   get_property(P, 'summary.result.h2', V1),
   write('<h2>'), html_escape(V1), write('</h2>'), nl,
   result_summary(Z),
   write('<table class="rowtable">'), nl,
   write('  <tr class="headrow">'), nl,
   get_property(P, 'summary.result.table.1', V3),
   write('  <th style="width: 20em">'), html_escape(V3), write('</th>'), nl,
   get_property(P, 'summary.result.table.2', V4),
   write('  <th style="width: 4em">'), html_escape(V4), write('</th>'), nl,
   get_property(P, 'summary.result.table.3', V5),
   write('  <th style="width: 4em">'), html_escape(V5), write('</th>'), nl,
   get_property(P, 'summary.result.table.4', V6),
   write('  <th style="width: 12em">'), html_escape(V6), write('</th>'), nl,
   write('  </tr>'), nl,
   html_result_member,
   write('  <tr class="headrow">'), nl,
   write('  <td>Total</td>'), nl,
   html_pairs_data(Z),
   write('  </tr>'), nl,
   write('</table>'), nl,
   fail.
html_result_list.

% html_result_member
:- private html_result_member/0.
html_result_member :-
   call_nth(bagof(N, U^result_suite_view(D, N, U), L), I),
   findall(W, (member(N, L), result_suite_view(D, N, W)), V),
   sys_sum_oknok(V, Z),
   html_zebra_row(I),
   make_uri('09_results/package.html', '', D, DUri),
   uri_encode(DUri, DUriEnc),
   write('  <td><a href="'), html_escape(DUriEnc),
   write('">'), html_escape(D), write('</a></td>'), nl,
   html_pairs_data(Z),
   write('  </tr>'), nl,
   fail.
html_result_member.

/*************************************************************/
/* Cover Summary                                             */
/*************************************************************/

% html_cover_list
:- private html_cover_list/0.
html_cover_list :-
   get_properties(testing, P),
   get_property(P, 'summary.cover.h2', V1),
   write('<h2>'), html_escape(V1), write('</h2>'), nl,
   cover_summary(Z),
   write('<table class="rowtable">'), nl,
   write('  <tr class="headrow">'), nl,
   get_property(P, 'summary.cover.table.1', V3),
   write('  <th style="width: 20em">'), html_escape(V3), write('</th>'), nl,
   get_property(P, 'summary.cover.table.2', V4),
   write('  <th style="width: 4em">'), html_escape(V4), write('</th>'), nl,
   get_property(P, 'summary.cover.table.3', V5),
   write('  <th style="width: 4em">'), html_escape(V5), write('</th>'), nl,
   get_property(P, 'summary.cover.table.4', V6),
   write('  <th style="width: 12em">'), html_escape(V6), write('</th>'), nl,
   write('  </tr>'), nl,
   html_cover_member,
   write('  <tr class="headrow">'), nl,
   write('  <td>Total</td>'), nl,
   html_pairs_data(Z),
   write('  </tr>'), nl,
   write('</table>'), nl,
   fail.
html_cover_list.

% html_cover_member
:- private html_cover_member/0.
html_cover_member :-
   call_nth(bagof(S, U^N^cover_source_view(S, D, N, U), L), I),
   findall(W, (member(S, L), cover_source_view(S, D, N, W)), V),
   sys_sum_oknok(V, Z),
   html_zebra_row(I),
   make_uri('07_coverage/package.html', '', D, DUri),
   uri_encode(DUri, DUriEnc),
   write('  <td><a href="'), html_escape(DUriEnc),
   write('">'), html_escape(D), write('</a></td>'), nl,
   html_pairs_data(Z),
   write('  </tr>'), nl,
   fail.
html_cover_member.

