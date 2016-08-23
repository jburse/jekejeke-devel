/**
 * For debugging purposes it might be necessary to access predicates
 * and evaluable functions that are not accessible from the top-level
 * by the module system visibility rules. We provide predicates that
 * allow direct access.
 *
 * The directly accessible predicates can be tested and enumerated
 * by the predicate current_provable/1. The predicates provable_property/2,
 * set_provable_property/2 and reset_provable_property/2 are responsible
 * for accessing and modifying properties of directly
 * accessible predicates.
 *
 * The predicates sys_callable_colon/2 can be used to explicitly invoke
 * the colon (:)/2 and colon colon (::)/2 notation conversion for a
 * callable. The colon colon notation is analogously handled to the
 * colon notation by prepending the receiver and combining the receiver
 * module name. The predicate sys_indicator_colon/2 can be used to
 * explicitly invoke to colon notation conversion for a
 * predicate indicator.
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

:- package(library(jekdev/reference/inspection)).
:- use_package(foreign(jekdev/reference/inspection)).

:- module(provable, []).

/********************************************************************/
/* Predicates                                                       */
/********************************************************************/

/**
 * current_provable(P):
 * The predicate succeeds for the directly accessible predicates P.
 */
% current_provable(-Indicator)
:- public current_provable/1.
current_provable(I) :-
   ground(I), !,
   sys_current_provable_chk(I).
current_provable(I) :-
   sys_current_provable(L),
   sys_member(I, L).

% already defined in load.p of runtime
% :- private sys_current_provable/1.
% :- special(sys_current_provable/1, 'SpecialProvable', 0).

:- private sys_current_provable_chk/1.
:- special(sys_current_provable_chk/1, 'SpecialProvable', 1).

/**
 * provable_property(I, Q):
 * The predicate succeeds for the properties Q of the predicate I. The predicate
 * will also try to access invisible predicates.
 */
% provable_property(-Indicator, -Property)
:- public provable_property/2.
provable_property(I, R) :-
   var(R), !,
   sys_provable_property(I, P),
   sys_member(R, P).
provable_property(I, R) :-
   functor(R, F, A),
   sys_provable_property_chk(I, F/A, P),
   sys_member(R, P).

:- private sys_provable_property/2.
:- special(sys_provable_property/2, 'SpecialProvable', 2).

% already defined in load.p of runtime
% :- private sys_provable_property_chk/3.
% :- special(sys_provable_property_chk/3, 'SpecialProvable', 3).

/**
 * set_provable_property(I, Q):
 * The predicate assigns the property Q to the predicate I. The predicate will
 * also try to access invisible predicates.
 */
% set_provable_property(+Oper, +Property)
:- public set_provable_property/2.
:- special(set_provable_property/2, 'SpecialProvable', 4).

/**
 * reset_provable_property(I, Q):
 * The predicate deassigns the property Q from the predicate I. The predicate will
 * also try to access invisible predicates.
 */
% reset_provable_property(+Oper, +Property)
:- public reset_provable_property/2.
:- special(reset_provable_property/2, 'SpecialProvable', 5).

/**
 * sys_callable_colon(S, T):
 * The predicate succeeds when S is a callable of the form
 * ‘pk-1%pk’(X1, .., Xm) and T is a colon notation callable of the
 * form p1:..:pk(X1, .., Xm), for 1 ≤ k and 0 ≤ m.
 */
% sys_callable_colon(+-Callable, -+Term):
:- special(sys_callable_colon/2, 'SpecialProvable', 6).
:- set_predicate_property(sys_callable_colon/2, visible(public)).

/**
 * sys_indicator_colon(S, T):
 * The predicate succeeds when S is an indicator of the form
 * ‘pk-1%pk’/m and T is a colon notation indicator of the form
 * p1:..:pk/m, for 1 ≤ k and 0 ≤ m.
 */
% sys_indicator_colon(+-Indicator, -+Term):
:- special(sys_indicator_colon/2, 'SpecialProvable', 7).
:- set_predicate_property(sys_indicator_colon/2, visible(public)).
