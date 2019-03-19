/**
 * This module allows the batch reporting of coverage analysis. Beforehand
 * the module tracker needs to be used to produce the coverage analysis.
 * The predicate cover_batch/0 can then be used to generate a number of
 * files that list and summarize the results in HTML format. The reporting
 * tool makes an additional assumption about the source names:
 *
 * source --> package "/" module.
 *
 * The first level HTML page will thus present the analysis grouped by
 * packages. The second level HTML page will thus present the analysis of
 * apackage grouped by modules. The current implementation shows hit and
 * miss counts not only as numbers but also as coloured bars. Furthermore
 * links to the original source clauses will be generated.
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

:- module(cover, []).

:- use_module(library(basic/lists)).
:- use_module(library(system/locale)).
:- use_module(library(system/zone)).
:- use_module(library(advanced/sequence)).
:- use_module(library(stream/xml)).
:- use_module(library(inspection/base)).
:- use_module(library(system/uri)).
:- use_module(tracker).
:- use_module(helper).
:- sys_load_resource(testing).

/**
 * cover_batch(R):
 * The predicate generates a number of files into the location pointed
 * by the base_url Prolog flag. Links to the source code are generated
 * relative to the argument R.
 */
% cover_batch(+RelUrl)
:- public cover_batch/1.
cover_batch(Z) :- cover_summary, cover_packages,
   cover_sources(Z).

/*************************************************************/
/* HTML Cover Summary                                        */
/*************************************************************/

% cover_summary
:- private cover_summary/0.
cover_summary :-
   write('Generating '),
   write('.'), nl,
   sys_get_lang(testing, P),
   get_property(P, 'cover.summary.title', V),
   setup_call_cleanup(report_begin_html('package.html', V, Y),
      html_list_summary,
      report_end_html(Y)).

% html_list_summary
:- private html_list_summary/0.
html_list_summary :-
   write('<h1 date='''),
   get_time(T),
   format_atom('%1$tF %1$tT', [T], TStr),
   write(TStr),
   sys_get_lang(testing, P),
   get_property(P, 'cover.summary.h1', V1),
   write('''>'),
   html_escape(V1),
   write('</h1>'), nl,
   call_nth(bagof(S, U^N^cover_source_view(S, D, N, U), L), I),
   findall(W, (  member(S, L),
                 cover_source_view(S, _, _, W)), V),
   sys_sum_oknok(V, Z),
   uri_encode(D, DEnc),
   write('<a name="'),
   html_escape(DEnc),
   write('"></a>'),
   get_property(P, 'cover.summary.h2', V2),
   write('<h2>'),
   html_escape(V2),
   write(' '),
   html_escape(D),
   write('</h2>'), nl,
   write('<table class="rowtable">'), nl,
   write('  <tr class="headrow">'), nl,
   get_property(P, 'cover.summary.table.1', V3),
   write('  <th style="width: 20em">'),
   html_escape(V3),
   write('</th>'), nl,
   get_property(P, 'cover.summary.table.2', V4),
   write('  <th style="width: 4em">'),
   html_escape(V4),
   write('</th>'), nl,
   get_property(P, 'cover.summary.table.3', V5),
   write('  <th style="width: 4em">'),
   html_escape(V5),
   write('</th>'), nl,
   get_property(P, 'cover.summary.table.4', V6),
   write('  <th style="width: 12em">'),
   html_escape(V6),
   write('</th>'), nl,
   write('  </tr>'), nl,
   atom_number(IStr, I),
   atom_split(R, '', ['0',IStr,'_',D,'/package.html']),
   html_list_element(R, L),
   write('  <tr class="headrow">'), nl,
   write('  <td>Total</td>'), nl,
   html_pairs_data(Z),
   write('  </tr>'), nl,
   write('</table>'), nl, fail.
html_list_summary.

% html_list_element(+Atom, +List)
:- private html_list_element/2.
html_list_element(R, L) :-
   call_nth(member(S, L), Z),
   cover_source_view(S, _, N, P),
   html_zebra_row(Z),
   make_uri(R, '', N, RNUri),
   uri_encode(RNUri, RRNUriEnc),
   write('  <td><a href="'),
   html_escape(RRNUriEnc),
   write('">'),
   html_escape(N),
   write('</a></td>'), nl,
   html_pairs_data(P),
   write('  </tr>'), nl, fail.
html_list_element(_, _).

/*************************************************************/
/* HTML Packages                                             */
/*************************************************************/

% cover_packages.
:- private cover_packages/0.
cover_packages :-
   call_nth(bagof(S, U^N^cover_source_view(S, D, N, U), L), I),
   atom_number(IStr, I),
   atom_split(Q, '', ['0',IStr,'_',D,'/package.html']),
   write('Generating '),
   write('.'/D), nl,
   setup_call_cleanup(report_begin_html(Q, D, Y),
      html_list_package(D, L),
      report_end_html(Y)), fail.
cover_packages.

% html_list_package(+Atom, +List)
:- private html_list_package/2.
html_list_package(D, L) :-
   write('<h1 date='''),
   get_time(T),
   format_atom('%1$tF %1$tT', [T], TStr),
   write(TStr),
   sys_get_lang(testing, P),
   get_property(P, 'cover.package.h1', V1),
   write('''>'),
   html_escape(V1),
   write(' '),
   html_escape(D),
   write('</h1>'), nl,
   call_nth(member(S, L), J),
   cover_source_view(S, _, N, U),
   uri_encode(N, NEnc),
   write('<a name="'),
   html_escape(NEnc),
   write('"></a>'),
   get_property(P, 'cover.package.h2', V2),
   write('<h2>'),
   html_escape(V2),
   write(' '),
   html_escape(N),
   write('</h2>'), nl,
   write('<table class="rowtable">'), nl,
   write('  <tr class="headrow">'), nl,
   get_property(P, 'cover.package.table.1', V3),
   write('  <th style="width: 20em">'),
   html_escape(V3),
   write('</th>'), nl,
   get_property(P, 'cover.package.table.2', V4),
   write('  <th style="width: 4em">'),
   html_escape(V4),
   write('</th>'), nl,
   get_property(P, 'cover.package.table.3', V5),
   write('  <th style="width: 4em">'),
   html_escape(V5),
   write('</th>'), nl,
   get_property(P, 'cover.package.table.4', V6),
   write('  <th style="width: 12em">'),
   html_escape(V6),
   write('</th>'), nl,
   write('  </tr>'), nl,
   atom_number(JStr, J),
   atom_split(R, '', ['0',JStr,'_',N,'.html']),
   html_list_member(R, S),
   write('  <tr class="headrow">'), nl,
   write('  <td>Total</td>'), nl,
   html_pairs_data(U),
   write('  </tr>'), nl,
   write('</table>'), nl, fail.
html_list_package(_, _).

% html_list_member(+Atom, +Atom)
:- private html_list_member/2.
html_list_member(R, S) :-
   call_nth(cover_predicate(F, A, S, P), Z),
   html_zebra_row(Z),
   sys_provable_hash(F/A, S, K),
   term_atom(K, FAStr, [quoted(true)]),
   make_uri(R, '', FAStr, RFAUri),
   uri_encode(RFAUri, RFAUriEnc),
   write('  <td><a href="'),
   html_escape(RFAUriEnc),
   write('">'),
   html_functor_indicator(F, A, S),
   write('</a></td>'), nl,
   html_pairs_data(P),
   write('  </tr>'), nl, fail.
html_list_member(_, _).

/*************************************************************/
/* HTML Sources                                              */
/*************************************************************/

% cover_sources(+RelUrl)
:- private cover_sources/1.
cover_sources(Z) :-
   call_nth(bagof(S, U^N^cover_source_view(S, D, N, U), L), I),
   atom_number(IStr, I),
   call_nth(member(S, L), J),
   atom_number(JStr, J),
   cover_source_view(S, D, N, _),
   atom_split(P, '', ['0',IStr,'_',D,/,'0',JStr,'_',N,'.html']),
   write('Generating '),
   write('.'/D/N), nl,
   setup_call_cleanup(report_begin_html(P, N, Y),
      html_list_source(S, N, Z),
      report_end_html(Y)), fail.
cover_sources(_).

% html_list_source(+Atom, +Atom, +RelUrl)
:- private html_list_source/3.
html_list_source(S, N, Z) :-
   write('<h1 date='''),
   get_time(T),
   format_atom('%1$tF %1$tT', [T], TStr),
   write(TStr),
   sys_get_lang(testing, P),
   get_property(P, 'cover.source.h1', V1),
   write('''>'),
   html_escape(V1),
   write(' '),
   html_escape(N),
   write('</h1>'), nl,
   cover_predicate(F, A, S, U),
   sys_provable_hash(F/A, S, K),
   term_atom(K, FAStr, [quoted(true)]),
   uri_encode(FAStr, FAStrEnc),
   write('<a name="'),
   html_escape(FAStrEnc),
   write('"></a>'),
   write('<h2>'),
   html_functor_type(A),
   write(' '),
   html_functor_indicator(F, A, S),
   write('</h2>'), nl,
   write('<table class="rowtable">'), nl,
   write('  <tr class="headrow">'), nl,
   get_property(P, 'cover.source.table.1', V2),
   write('  <th style="width: 20em">'),
   html_escape(V2),
   write('</th>'), nl,
   get_property(P, 'cover.source.table.2', V3),
   write('  <th style="width: 4em">'),
   html_escape(V3),
   write('</th>'), nl,
   get_property(P, 'cover.source.table.3', V4),
   write('  <th style="width: 4em">'),
   html_escape(V4),
   write('</th>'), nl,
   get_property(P, 'cover.source.table.4', V5),
   write('  <th style="width: 12em">'),
   html_escape(V5),
   write('</th>'), nl,
   write('  </tr>'), nl,
   html_list_predicate(F, A, S, Z),
   write('  <tr class="headrow">'), nl,
   write('  <td>Total</td>'), nl,
   html_pairs_data(U),
   write('  </tr>'), nl,
   write('</table>'), nl, fail.
html_list_source(_, _, _).

% html_list_predicate(+Atom, +Integer, +Atom, +RelUrl)
:- private html_list_predicate/4.
html_list_predicate(F, A, S, Z) :-
   cover_source_view(S, D, M, _),
   atom_split(R, '', [Z,D,/,M,'.html']),
   call_nth(cover(F, A, S, N, P), I),
   html_zebra_row(I),
   write('  <td>'),
   atom_number(NStr, N),
   atom_split(U, '', [o,NStr]),
   make_uri(R, '', U, RUUri),
   uri_encode(RUUri, RUUriEnc),
   write('<a href="'),
   html_escape(RUUriEnc),
   write('">'),
   html_escape(NStr),
   write('</a></td>'), nl,
   html_pairs_data(P),
   write('  </tr>'), nl, fail.
html_list_predicate(_, _, _, _).

/********************************************************/
/* Module Ops                                           */
/********************************************************/

% html_functor_indicator(+Atom, +Integer, +Context)
:- private html_functor_indicator/3.
html_functor_indicator(F, A, C) :-
   A < 0, !,
   B is -A-1,
   sys_provable_hash(F/B, C, K),
   term_atom(K, FBStr, [quoted(true)]),
   html_escape(FBStr).
html_functor_indicator(F, A, C) :-
   sys_provable_hash(F/A, C, K),
   term_atom(K, FAStr, [quoted(true)]),
   html_escape(FAStr).
