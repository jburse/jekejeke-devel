/**
 * Static, dynamic and thread local predicates can be consulted.
 * Dynamic and thread local predicates on the other hand can be
 * also accessed and modified by the predicates here in. The predicate
 * clause/2 matches clauses, the predicate retract/1 matches and
 * removes clauses and the predicates asserta/1 and assertz/1
 * add a clause.
 *
 * Examples:
 * ?- assertz(p(a)).
 * Yes
 * ?- p(X).
 * X = a
 * ?- clause(p(X),Y).
 * X = a,
 * Y = true
 *
 * The predicates clause/2, retract/1 and retractall/1 can only
 * match clauses that are visible from the head predicate that is
 * used in the search. The predicates asserta/1 and assertz/1 cannot
 * redefine a predicate. Instead it must be marked multi-file. The
 * clauses of a dynamic predicate are seen by all threads. A thread
 * local predicate on the other hand has its own set of clauses
 * for each thread.
 *
 * Examples:
 * ?- abolish(foo/1).
 * Yes
 * ?- abolish(infix(=>)).
 * Yes
 *
 * The predicate abolish/1 allows a predicate turning it non-existent
 * again. The same predicate can be also used to remove operators
 * by using indicators prefix/1, postfix/1 and infix/1. The predicate
 * will attempt to remove own user clauses of the predicate, and only
 * remove it if no system or foreign user clauses remain. If still
 * some of the aforementioned clauses remain the predicate stays
 * existent and non-empty.
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

:- use_package(foreign(jekpro/reference/runtime)).

:- module(user, []).
:- use_module(library(experiment/ref)).
:- use_module(library(experiment/simp)).

:- public prefix(dynamic).
:- op(1150, fx, dynamic).

:- public prefix(thread_local).
:- op(1150, fx, thread_local).

:- public prefix(group_local).
:- op(1150, fx, group_local).

/***************************************************************/
/* Predicate Types                                             */
/***************************************************************/

/**
 * dynamic P, ...: [ISO 7.4.2.1]
 * The predicate sets the predicate P to dynamic.
 */
% dynamic +Indicators
:- public (dynamic)/1.
dynamic [P|Q] :- !, sys_dynamic(P), dynamic(Q).
dynamic P, Q :- !, sys_dynamic(P), dynamic(Q).
dynamic [] :- !.
dynamic P :- sys_dynamic(P).

:- private sys_dynamic/1.
sys_dynamic(I) :-
   sys_make_indicator(F, _, I),
   callable_property(F, sys_context(C)),
   sys_ensure_shared_dynamic(I),
   set_predicate_property(I, sys_dynamic(C)),
   sys_check_style_body(I).

:- private sys_ensure_shared_dynamic/1.
:- special(sys_ensure_shared_dynamic/1, 'SpecialDynamic', 0).

/**
 * thread_local P, …:
 * The predicate sets the predicate P to thread local.
 */
% thread_local +Indicators
:- public (thread_local)/1.
thread_local [P|Q] :- !, sys_thread_local(P), thread_local(Q).
thread_local P, Q :- !, sys_thread_local(P), thread_local(Q).
thread_local [] :- !.
thread_local P :- sys_thread_local(P).

:- private sys_thread_local/1.
sys_thread_local(I) :-
   sys_make_indicator(F, _, I),
   callable_property(F, sys_context(C)),
   sys_ensure_thread_local(I),
   set_predicate_property(I, sys_thread_local(C)),
   sys_check_style_body(I).

:- private sys_ensure_thread_local/1.
:- special(sys_ensure_thread_local/1, 'SpecialDynamic', 1).

/**
 * group_local P, …:
 * The predicate sets the predicate P to group local.
 */
% group_local +Indicators
:- public (group_local)/1.
group_local [P|Q] :- !, sys_group_local(P), group_local(Q).
group_local P, Q :- !, sys_group_local(P), group_local(Q).
group_local [] :- !.
group_local P :- sys_group_local(P).

:- private sys_group_local/1.
sys_group_local(I) :-
   sys_make_indicator(F, _, I),
   callable_property(F, sys_context(C)),
   sys_ensure_group_local(I),
   set_predicate_property(I, sys_group_local(C)),
   sys_check_style_body(I).

:- private sys_ensure_group_local/1.
:- special(sys_ensure_group_local/1, 'SpecialDynamic', 2).

% first defined in special.p
% sys_declaration_indicator(+Declaration, -Indicator).
:- public sys_declaration_indicator/2.
:- multifile sys_declaration_indicator/2.
sys_declaration_indicator(dynamic(I), I).
sys_declaration_indicator(thread_local(I), I).
sys_declaration_indicator(group_local(I), I).

/**
 * clause(H, B): [ISO 8.8.1]
 * The predicate succeeds with the clauses that unify H :- B.
 * The head predicate must be dynamic, thread local or group
 * local.
 */
% clause(-Term, -Goal)
:- public clause/2.
:- meta_predicate clause(-1, 0).
:- special(clause/2, 'SpecialDynamic', 3).

/**
 * sys_rule(I, H, B):
 * The predicate succeeds with the clauses of the predicate indicator
 * I that unify H :- B. The predicate indicator can be static.
 */
% sys_rule(+Indicator, -Term, -Goal)
:- public sys_rule/3.
:- special(sys_rule/3, 'SpecialDynamic', 4).

/**
 * retract(C): [ISO 8.9.3]
 * The predicate succeeds with and removes the user clauses that
 * match C. The head predicate must be dynamic, thread local or group local.
 */
% retract(-Term)
:- public retract/1.
:- meta_predicate retract(-1).
retract(C) :- var(C),
   throw(error(instantiation_error, _)).
retract((H :- B)) :- !,
   clause_ref(H, B, R),
   erase_ref(R).
retract(H) :-
   clause_ref(H, true, R),
   erase_ref(R).

/**
 * retractall(H): [Corr.2 8.9.5]
 * The predicate succeeds and removes all the user clauses that match
 * the head H. The head predicate must be dynamic, thread local or group local.
 */
% retractall(+Callable)
:- public retractall/1.
:- meta_predicate retractall(-1).
retractall(H) :-
   clause_ref(H, _, R),
   erase_ref(R),
   fail.
retractall(_).

/**
 * asserta(C): [ISO 8.9.1]
 * The predicate inserts the clause C at the top. The head predicate
 * must be dynamic, thread local or group local.
 */
% asserta(+Term)
:- public asserta/1.
:- meta_predicate asserta(-1).
:- special(asserta/1, 'SpecialDynamic', 5).

/**
 * assertz(C): [ISO 8.9.2]
 * The predicate inserts the clause C at the bottom. The head predicate
 * must be dynamic, thread local or group local.
 */
% assertz(+Term)
:- public assertz/1.
:- meta_predicate assertz(-1).
:- special(assertz/1, 'SpecialDynamic', 6).

/**
 * abolish(P): [ISO 8.9.4]
 * The predicate removes the predicate, evaluable function or
 * syntax operator P.
 */
% abolish(+Indicator)
:- public abolish/1.
abolish(prefix(X)) :- !,
   sys_abolish_operator(prefix(X)).
abolish(infix(X)) :- !,
   sys_abolish_operator(infix(X)).
abolish(postfix(X)) :- !,
   sys_abolish_operator(postfix(X)).
abolish(X) :-
   sys_abolish_predicate(X).

:- private sys_abolish_predicate/1.
:- special(sys_abolish_predicate/1, 'SpecialDynamic', 7).

:- private sys_abolish_operator/1.
:- special(sys_abolish_operator/1, 'SpecialDynamic', 8).

/****************************************************************/
/* Clause Listing                                               */
/****************************************************************/

/**
 * sys_has_clause(I, U):
 * The predicate succeeds when the predicate I has clauses for the source U.
 */
% sys_has_clause(+Indicator, +Source)
:- public sys_has_clause/2.
:- special(sys_has_clause/2, 'SpecialDynamic', 9).

% sys_show_clauses(+Indicator, +Source)
:- public sys_show_clauses/2.
sys_show_clauses(I, U) :-
   sys_rule(I, H, B),
   callable_property(H, sys_context(U)),
   callable_property(H, sys_variable_names(N)),
   sys_make_clause(H, B, J),
   rebuild_term(J, T),
   term_variables(T, L),
   term_singletons(T, R),
   sys_number_variables(L, N, R, M),
   write_term('.'(T), [context(-1), quoted(true),
      format(newline), annotation(makedot),
      variable_names(M), source(U)]),
   fail.
sys_show_clauses(_, _).

% sys_make_clause(+Term, +Goal, -Term)
:- public sys_make_clause/3.
sys_make_clause(H, true, H) :- !.
sys_make_clause(H, B, (H :- B)).
