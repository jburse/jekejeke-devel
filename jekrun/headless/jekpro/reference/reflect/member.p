/**
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

:- sys_context_property(here, C),
   set_source_property(C, use_package(foreign(jekpro/reference/reflect))).
:- sys_context_property(here, C),
   reset_source_property(C, sys_source_visible(public)).

/*************************************************************************/
/* List Helper                                                           */
/*************************************************************************/

% sys_member(-Elem, +List)
sys_member(X, [Y|Z]) :-
   sys_member2(Z, X, Y).
:- set_predicate_property(sys_member/2, visible(public)).

% sys_member2(+List, -Elem, +Elem)
sys_member2(_, X, X).
sys_member2([Y|Z], X, _) :-
   sys_member2(Z, X, Y).
:- set_predicate_property(sys_member2/3, visible(private)).

% sys_oneof(+List, -Elem, -List)
sys_oneof([X|Y], Z, T) :-
   sys_oneof2(Y, Z, T, X).
:- set_predicate_property(sys_oneof/3, visible(public)).

% sys_oneof2(+List, +Elem, -List, -Elem)
sys_oneof2(Y, X, Y, X).
sys_oneof2([X|Y], Z, [W|T], W) :-
   sys_oneof2(Y, Z, T, X).
:- set_predicate_property(sys_oneof2/4, visible(private)).

% =(+Term, +Term)
:- special(= /2, 'SpecialMember', 0).
:- set_predicate_property(= /2, visible(public)).

% sys_eq(+Term, +Term)
:- special(sys_eq/2, 'SpecialMember', 0).
:- set_predicate_property(sys_eq/2, visible(public)).
:- set_predicate_property(sys_eq/2, sys_notrace).
:- set_predicate_property(sys_eq/2, sys_noexpand).

/*************************************************************************/
/* Univ Helper                                                           */
/*************************************************************************/

% var(+Term)
:- special(var/1, 'SpecialMember', 1).
:- set_predicate_property(var/1, visible(public)).

% nonvar(+Term)
:- special(nonvar/1, 'SpecialMember', 2).
:- set_predicate_property(nonvar/1, visible(public)).

% ground(+Term)
:- special(ground/1, 'SpecialMember', 3).
:- set_predicate_property(ground/1, visible(public)).

% functor(+-Term, -+Atomic, -+Integer)
functor(T, F, A) :-
   var(T), !,
   sys_functor_to_term(F, A, T).
functor(T, F, A) :-
   sys_term_to_functor(T, F, A).
:- set_predicate_property(functor/3, visible(public)).

:- special(sys_functor_to_term/3, 'SpecialMember', 4).
:- set_predicate_property(sys_functor_to_term/3, visible(private)).

:- special(sys_term_to_functor/3, 'SpecialMember', 5).
:- set_predicate_property(sys_term_to_functor/3, visible(private)).

/*************************************************************************/
/* Compare Helper                                                        */
/*************************************************************************/

% >(+Expr, +Expr)
:- special(> /2, 'SpecialMember', 6).
:- set_predicate_property(> /2, meta_predicate(>(#(1), #(1)))).
:- sys_context_property(here, C),
   set_predicate_property(> /2, sys_meta_predicate(C)).
:- set_predicate_property(> /2, visible(public)).
