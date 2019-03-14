/**
 * This module  provides accessing thread stack frames via
 * the predicate frame_property/2.
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

:- module(frame, []).
:- use_module(library(system/thread)).
:- use_module(library(stream/console)).

% some frame property
:- public sys_call_goal/1.
:- meta_predicate sys_call_goal(0).
sys_call_goal(_) :-
   throw(error(existence_error(body,sys_call_goal/1),_)).

/**
 * rule_ref(H, B, F):
 * The predicate succeeds with the user clauses that match H :- B. The
 * predicate also unifies F with the clause reference for the
 * found clauses.
 */
% rule_ref(+Term, -Goal, -Frame)
:- public rule_ref/3.
:- meta_predicate rule_ref(-1,0,?).
:- set_predicate_property(rule_ref/3, sys_noexpand).
:- special(rule_ref/3, 'SpecialFrame', 0).

/**
 * frame_property(F, P):
 * The predicate succeeds for the properties P of the frame F.
 */
% frame_property(+Frame, -Property)
:- public frame_property/2.
:- meta_predicate frame_property(?,0).
:- set_predicate_property(frame_property/2, sys_noexpand).
frame_property(I, R) :-
   var(R), !,
   sys_frame_property(I, P),
   sys_member(R, P).
frame_property(I, R) :-
   functor(R, F, A),
   sys_frame_property_chk(I, F/A, P),
   sys_member(R, P).

:- private sys_frame_property/2.
:- special(sys_frame_property/2, 'SpecialFrame', 2).

:- private sys_frame_property_chk/3.
:- special(sys_frame_property_chk/3, 'SpecialFrame', 3).

