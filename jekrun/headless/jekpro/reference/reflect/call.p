/**
 * Context and pretty printing information of an atom can be accessed and
 * modified by the predicates callable_property/2, set_callable_property/3
 * and reset_callable_property/3.
 *
 * Example:
 * ?- length(X, 0), callable_property(X, Y), writeq(Y), nl, fail; true.
 * sys_context('<path>/basic/lists.px')
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

:- sys_callable_property_chk(here, sys_context/1, [sys_context(C)]),
   set_source_property(C, use_package(foreign(jekpro/reference/reflect))).
:- sys_callable_property_chk(here, sys_context/1, [sys_context(C)]),
   reset_source_property(C, sys_source_visible(public)).

/**
 * callable_property(C, Q):
 * The predicate succeeds for the properties Q of the callable C.
 */
% callable_property(+Callable, -Property)
callable_property(I, R) :- var(R), !,
   sys_callable_property(I, P),
   sys_member(R, P).
callable_property(I, R) :-
   functor(R, F, A),
   sys_callable_property_chk(I, F/A, P),
   sys_member(R, P).
:- set_predicate_property(callable_property/2, visible(public)).

:- special(sys_callable_property/2, 'SpecialCall', 0).
:- set_predicate_property(sys_callable_property/2, visible(private)).

% already defined in special.p
% :- special(sys_callable_property_chk/3, 'SpecialCall', 1).
% :- set_predicate_property(sys_callable_property_chk/3, visible(private)).

/**
 * set_callable_property(B, Q, A):
 * The predicate succeeds for a new callable B which is a clone of
 * the callable A except for the property Q which is now set.
 */
% reset_callable_property(-Callable, +Property, +Callable)
:- special(set_callable_property/3, 'SpecialCall', 2).
:- set_predicate_property(set_callable_property/3, visible(public)).

/**
 * reset_callable_property(B, Q, A):
 * The predicate succeeds for a new callable B which is a clone of
 * the callable A except for the property Q which is now reset.
 */
% reset_callable_property(-Callable, +Property, +Callable)
:- special(reset_callable_property/3, 'SpecialCall', 3).
:- set_predicate_property(reset_callable_property/3, visible(public)).
