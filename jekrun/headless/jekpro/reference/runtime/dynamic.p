/**
 * Static, dynamic and thread local predicates can be consulted.
 * Dynamic and thread local predicates on the other hand can be
 * also accessed and modified by the predicates here in. The predicate
 * clause/2 matches clauses, the predicate retract/1 matches and
 * removes clauses and the predicates asserta/1 and assertz/1
 * add a clause.
 *
 * The predicates clause/2, retract/1 and retractall/1 can only
 * match clauses that are visible from the head predicate that is
 * used in the search. The predicates asserta/1 and assertz/1 cannot
 * redefine a predicate. Instead it must be marked multi-file. The
 * clauses of a dynamic predicate are seen by all threads. A thread
 * local predicate on the other hand has its own set of clauses
 * for each thread.
 *
 * The predicate abolish/1 allows removing a predicate turning it
 * into non-existent again. The predicate abolish/1 can be also used
 * to remove operators.
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

:- use_package(foreign(jekpro/reference/runtime)).

:- module(user, []).
:- use_module(library(experiment/ref)).

:- public prefix(dynamic).
:- op(1150, fx, dynamic).

:- public prefix(thread_local).
:- op(1150, fx, thread_local).

/***************************************************************/
/* Predicate Types                                             */
/***************************************************************/

/**
 * dynamic P, ...: [ISO 7.4.2.1]
 * The predicate sets the predicate P to dynamic.
 */
% dynamic +Indicators
:- public (dynamic)/1.
dynamic [P|Q] :- !,
   sys_dynamic(P),
   (dynamic Q).
dynamic P,Q :- !,
   sys_dynamic(P),
   (dynamic Q).
dynamic [] :- !.
dynamic P :-
   sys_dynamic(P).

:- private sys_dynamic/1.
sys_dynamic(I) :-
   sys_ensure_shared_dynamic(I),
   sys_make_indicator(F, _, I),
   sys_get_context(F, C),
   set_predicate_property(I, sys_dynamic(C)),
   sys_check_style_predicate(I).

:- private sys_ensure_shared_dynamic/1.
:- special(sys_ensure_shared_dynamic/1, 'SpecialDynamic', 0).

/**
 * thread_local P, …:
 * The predicate sets the predicate P to thread local.
 */
% thread_local +Indicator
:- public (thread_local)/1.
thread_local [P|Q] :- !,
   sys_thread_local(P),
   (thread_local Q).
thread_local P,Q :- !,
   sys_thread_local(P),
   (thread_local Q).
thread_local [] :- !.
thread_local P :-
   sys_thread_local(P).

:- private sys_thread_local/1.
sys_thread_local(I) :-
   sys_ensure_thread_local(I),
   sys_make_indicator(F, _, I),
   sys_get_context(F, C),
   set_predicate_property(I, sys_thread_local(C)),
   sys_check_style_predicate(I).

:- private sys_ensure_thread_local/1.
:- special(sys_ensure_thread_local/1, 'SpecialDynamic', 1).

% first defined in special.p
% sys_declaration_indicator(+Declaration, -Indicator).
:- public sys_declaration_indicator/2.
:- multifile sys_declaration_indicator/2.
sys_declaration_indicator((dynamic I), I).
sys_declaration_indicator((thread_local I), I).

/**
 * clause(H, B): [ISO 8.8.1]
 * The predicate succeeds with the user clauses that match H :- B.
 * The head predicate must be dynamic or thread local.
 */
% clause(-Term, -Goal)
:- public clause/2.
:- meta_predicate clause(-1,0).
:- set_predicate_property(clause/2, sys_noexpand).
:- special(clause/2, 'SpecialDynamic', 2).

/**
 * retract(C): [ISO 8.9.3]
 * The predicate succeeds with and removes the user clauses that
 * match C. The head predicate must be dynamic or thread local.
 */
% retract(-Term)
:- public retract/1.
:- meta_predicate retract(-1).
:- set_predicate_property(retract/1, sys_noexpand).
retract(V) :-
   var(V),
   throw(error(instantiation_error,_)).
retract((H :- B)) :- !,
   clause_ref(H, B, R),
   evict_ref(R).
retract(H) :-
   clause_ref(H, true, R),
   evict_ref(R).

:- private evict_ref/1.
evict_ref(R) :-
   erase_ref(R), !.
evict_ref(_).

/**
 * retractall(H): [Corr.2 8.9.5]
 * The predicate succeeds and removes the user clauses that match
 * the head H. The head predicate must be dynamic or thread local.
 */
:- public retractall/1.
:- meta_predicate retractall(-1).
:- set_predicate_property(retractall/1, sys_noexpand).
retractall(H) :-
   clause_ref(H, _, R),
   erase_ref(R), fail.
retractall(_).

/**
 * asserta(C): [ISO 8.9.1]
 * The predicate inserts the clause C at the top. The head predicate
 * must be dynamic or thread local.
 */
% asserta(+Term)
:- public asserta/1.
:- meta_predicate asserta(-1).
:- special(asserta/1, 'SpecialDynamic', 3).

/**
 * assertz(C): [ISO 8.9.2]
 * The predicate inserts the clause C at the bottom. The head predicate
 * must be dynamic or thread local.
 */
% assertz(+Term)
:- public assertz/1.
:- meta_predicate assertz(-1).
:- special(assertz/1, 'SpecialDynamic', 4).

/**
 * abolish(P): [ISO 8.9.4]
 * The predicate removes the predicate, evaluable function or
 * syntax operator P.
 */
% abolish(+Indicator)
:- public abolish/1.
abolish(prefix(X)) :- !,
   sys_abolish_oper(prefix(X)).
abolish(infix(X)) :- !,
   sys_abolish_oper(infix(X)).
abolish(postfix(X)) :- !,
   sys_abolish_oper(postfix(X)).
abolish(X) :-
   sys_abolish_predicate(X).

:- private sys_abolish_predicate/1.
:- special(sys_abolish_predicate/1, 'SpecialDynamic', 5).

:- private sys_abolish_oper/1.
:- special(sys_abolish_oper/1, 'SpecialDynamic', 6).