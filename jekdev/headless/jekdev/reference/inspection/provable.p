/**
 * For debugging purposes it might be necessary to access predicates
 * and evaluable functions that are not accessible from the top-level
 * by the module system visibility rules. We provide predicates that
 * allow direct access. The access is call-site independent, requires
 * structured module names with package prefixes resolved and the
 * module already loaded.
 *
 * Examples:
 * ?- current_predicate(basic/lists:member2/3).
 * No
 * ?- current_provable(basic/lists:member2/3).
 * Yes
 *
 * The directly accessible predicates can be tested and enumerated
 * by the predicate current_provable/1. The predicates provable_property/2,
 * set_provable_property/2 and reset_provable_property/2 are responsible
 * for accessing and modifying properties of directly
 * accessible predicates.
 *
 * Example:
 * ?- length(X, 0), callable_property(X, Y), writeq(Y), nl, fail; true.
 * sys_context('<path>/lists.px')
 * source_file('<path>/lists.px')
 * line_no(136)
 * Yes
 *
 * Context and pretty printing information of an atom can be accessed and
 * modified by the predicates callable_property/2, set_callable_property/3
 * and reset_callable_property/3. The predicates defined here generalize
 * the predicates sys_context_property/2 and sys_replace_site/3 from
 * the Jekejeke Prolog runtime.
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

:- package(library(jekdev/reference/inspection)).
:- use_package(foreign(jekdev/reference/inspection)).

:- module(provable, []).

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
   var(I), !,
   sys_provable_property_idx(R, P),
   sys_member(I, P).
provable_property(I, R) :-
   functor(R, F, A),
   sys_provable_property_chk(I, F/A, P),
   sys_member(R, P).

:- private sys_provable_property/2.
:- special(sys_provable_property/2, 'SpecialProvable', 2).

% already defined in load.p of runtime
% :- private sys_provable_property_chk/3.
% :- special(sys_provable_property_chk/3, 'SpecialProvable', 3).

:- private sys_provable_property_idx/2.
:- special(sys_provable_property_idx/2, 'SpecialProvable', 4).

/**
 * set_provable_property(I, Q):
 * The predicate assigns the property Q to the predicate I. The predicate will
 * also try to access invisible predicates.
 */
% set_provable_property(+Oper, +Property)
:- public set_provable_property/2.
:- special(set_provable_property/2, 'SpecialProvable', 5).

/**
 * reset_provable_property(I, Q):
 * The predicate deassigns the property Q from the predicate I. The predicate will
 * also try to access invisible predicates.
 */
% reset_provable_property(+Oper, +Property)
:- public reset_provable_property/2.
:- special(reset_provable_property/2, 'SpecialProvable', 6).

/**
 * callable_property(C, Q):
 * The predicate succeeds for the properties Q of the callable C.
 */
% callable_property(+Callable, -Property)
:- public callable_property/2.
callable_property(I, R) :-
   var(R), !,
   sys_callable_property(I, P),
   sys_member(R, P).
callable_property(I, R) :-
   functor(R, F, A),
   sys_callable_property_chk(I, F/A, P),
   sys_member(R, P).

:- private sys_callable_property/2.
:- special(sys_callable_property/2, 'SpecialProvable', 7).

:- private sys_callable_property_chk/3.
:- special(sys_callable_property_chk/3, 'SpecialProvable', 8).

/**
 * set_callable_property(B, Q, A):
 * The predicate succeeds for a new callable B which is a clone of
 * the callable A except for the property Q which is now set.
 */
% reset_callable_property(-Callable, +Property, +Callable)
:- public set_callable_property/3.
:- special(set_callable_property/3, 'SpecialProvable', 9).

/**
 * reset_callable_property(B, Q, A):
 * The predicate succeeds for a new callable B which is a clone of
 * the callable A except for the property Q which is now reset.
 */
% reset_callable_property(-Callable, +Property, +Callable)
:- public reset_callable_property/3.
:- special(reset_callable_property/3, 'SpecialProvable', 10).
