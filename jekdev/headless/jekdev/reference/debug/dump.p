/**
 * The shape of the clause index depends on the call pattern history
 * of the predicate. We do not provide a programming interface to
 * selectively inspect the clause index. Instead the end-user can
 * use the predicates jiti_list/[0,1] to list the clause index for
 * predicates in one go. Further the predicates jiti_summary/[0,1] allow
 * displaying a summary.
 *
 * Example:
 * ?- [user].
 * p(7, a).
 * p(7, b).
 * p(9, c).
 * ^D
 * Yes
 * ?- p(7, a).
 * Yes
 *
 * The query will deterministically succeed. This is an indicative that
 * a clause index has been built that covers multiple arguments. Clause
 * indexing based on first argument indexing only would not be able
 * to detect this determinism. Although the clause index is multi
 * argument, it does so only for the key “7”:
 *
 * Example:
 * ?- jiti_list.
 * -------- p/2 ---------
 * length=3
 * at=1
 *    key=7, length=2
 *       at=2
 *          key=a, length=1
 *          key=b, length=1
 *    key=9, length=1
 *
 * Yes
 * ?- jiti_summary.
 * 1	2	150%
 * 1+2	2	100%
 * Yes
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

:- package(library(jekdev/reference/debug)).
:- use_package(foreign(jekdev/reference/debug)).

:- module(dump, []).
:- use_module(library(inspection/provable)).
:- use_module(library(debug/friendly)).
:- use_module(library(basic/lists)).

/**
 * jiti_list:
 * jiti_list(P):
 * The predicate dumps the clause indexes of the clauses of the
 * user predicates. The unary predicate allows specifying a predicate
 * indicator P. See the API documentation for a description
 * of the listed attributes.
 */
% jiti_list
:- public jiti_list/0.
:- sys_notrace jiti_list/0.
jiti_list :-
   jiti_inspect(_, 0).

% jiti_list(+Pattern)
:- public jiti_list/1.
:- sys_notrace jiti_list/1.
jiti_list(I) :-
   jiti_inspect(I, 0).

/**
 * jiti_skipped:
 * jiti_skipped(P):
 * Works like the predicates jiti_list/[0,1] except that the execution
 * skipped variants of the indexes are shown.
 */
% jiti_skipped
:- public jiti_skipped/0.
:- sys_notrace jiti_skipped/0.
jiti_skipped :-
   jiti_inspect(_, 1).

% jiti_skipped(+Pattern)
:- public jiti_skipped/1.
:- sys_notrace jiti_skipped/1.
jiti_skipped(I) :-
   jiti_inspect(I, 1).

% jiti_inspect(+Pattern, +Integer)
:- private jiti_inspect/2.

jiti_inspect(I, F) :- ground(I), !,
   jiti_inspect2(I, F).
jiti_inspect(I, F) :-
   setof(I, U^(sys_listing_user(U),
      sys_intermediate_item_idx(U, I),
      sys_has_set(I)), B),
   member(I, B),
   sys_intermediate_item_sep(I),
   sys_jiti_inspect(I, F), nl,
   fail.
jiti_inspect(_, _).

% jiti_inspect2(+Indicator, +Integer)
:- private jiti_inspect2/2.
jiti_inspect2(I, F) :-
   sys_intermediate_item_chk(I, U),
   sys_listing_user_chk(U),
   sys_has_set(I),
   sys_intermediate_item_sep(I),
   sys_jiti_inspect(I, F), nl,
   fail.
jiti_inspect2(_, _).

% sys_jiti_inspect(+Indicator, +Integer)
:- private sys_jiti_inspect/2.
:- special(sys_jiti_inspect/2, 'SpecialDump', 0).

/**
 * jiti_summary:
 * jiti_summary(P):
 * Works like the predicates jiti_list/[0,1] except that an overall statistics
 * about the Prolog indexes is reported. See the API documentation for a description
 * of the listed columns.
 */
% jiti_summary
:- public jiti_summary/0.
:- sys_notrace jiti_summary/0.
jiti_summary :-
   jiti_summary(_).

% jiti_summary(+Pattern)
:- public jiti_summary/1.
:- sys_notrace jiti_summary/1.
jiti_summary(I) :-
   sys_averager_new(M),
   jiti_summary(I, M),
   sys_averager_show(M).

% jiti_summary(+Pattern, +Map)
:- private jiti_summary/2.
jiti_summary(I, M) :- ground(I), !,
   jiti_summary2(I, M).
jiti_summary(I, M) :-
   setof(I, U^(sys_listing_user(U),
      sys_intermediate_item_idx(U, I),
      sys_has_set(I)), B),
   member(I, B),
   sys_jiti_recap(I, M),
   fail.
jiti_summary(_, _).

% jiti_summary2(+Indicator, +Map)
:- private jiti_summary2/2.
jiti_summary2(I, M) :-
   sys_intermediate_item_chk(I, U),
   sys_listing_user_chk(U),
   sys_has_set(I),
   sys_jiti_recap(I, M),
   fail.
jiti_summary2(_, _).

/**
 * jiti_report:
 * jiti_report(P):
 * Works like the predicates jiti_list/[0,1] except that a statistics
 * about the Prolog indexes is reported.
 */
% jiti_report
:- public jiti_report/0.
:- sys_notrace jiti_report/0.
jiti_report :-
   jiti_report(_).

% jiti_report(+Pattern)
:- public jiti_report/1.
:- sys_notrace jiti_report/1.
jiti_report(I) :- ground(I), !,
   jiti_report2(I).
jiti_report(I) :-
   setof(I, U^(sys_listing_user(U),
      sys_intermediate_item_idx(U, I),
      sys_has_set(I)), B),
   member(I, B),
   sys_averager_new(M),
   sys_jiti_recap(I, M),
   sys_averager_has(M),
   sys_intermediate_item_sep(I),
   sys_averager_show(M), nl,
   fail.
jiti_report(_).

% jiti_report2(+Indicator)
:- private jiti_report2/1.
jiti_report2(I) :-
   sys_intermediate_item_chk(I, U),
   sys_listing_user_chk(U),
   sys_has_set(I),
   sys_averager_new(M),
   sys_jiti_recap(I, M),
   sys_averager_has(M),
   sys_intermediate_item_sep(I),
   sys_averager_show(M), nl,
   fail.
jiti_report2(_).

% sys_jiti_recap(+Indicator, +Map)
:- private sys_jiti_recap/2.
:- special(sys_jiti_recap/2, 'SpecialDump', 1).

% sys_averager_new(-Map)
:- private sys_averager_new/1.
:- special(sys_averager_new/1, 'SpecialDump', 2).

% sys_averager_show(+Map)
:- private sys_averager_show/1.
:- special(sys_averager_show/1, 'SpecialDump', 3).

% sys_averager_has(+Map)
:- private sys_averager_has/1.
:- special(sys_averager_has/1, 'SpecialDump', 4).

% sys_has_set(+Indicator)
:- special(sys_has_set/1, 'SpecialDump', 5).
