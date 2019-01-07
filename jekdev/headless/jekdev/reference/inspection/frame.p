/**
 * t.b.d.
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

% some frame property
:- public sys_call_goal/1.
:- meta_predicate sys_call_goal(0).
sys_call_goal(_) :-
   throw(error(existence_error(body,sys_call_goal/1),_)).

/**
 * rule_frame(H, B, F):
 * The predicate succeeds with the user clauses that match H :- B. The
 * predicate also unifies F with the new frame reference for the
 * found clauses.
 */
% rule_frame(+Term, -Goal, -Frame)
:- public rule_frame/3.
:- meta_predicate rule_frame(-1,0,?).
:- set_predicate_property(rule_frame/3, sys_noexpand).
:- special(rule_frame/3, 'SpecialFrame', 0).

/**
 * sys_instance_clause(R, F):
 * The predicate succeeds with a new frame reference F for the
 * rule, fact or directive referenced by R.
 */
% sys_instance_clause(+Reference, -Frame)
:- public sys_instance_clause/2.
:- special(sys_instance_clause/2, 'SpecialFrame', 1).

/**
 * frame_property(F, P):
 * The predicate succeeds for the properties P of the clause referenced by F.
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

/**
 * set_frame_property(F, P):
 * The predicate assigns the property P to the clause referenced by F.
 */
% set_frame_property(+Frame, +Property)
:- public set_frame_property/2.
:- special(set_frame_property/2, 'SpecialFrame', 4).

/**
 * reset_frame_property(F, P):
 * The predicate de-assigns the property P from the clause referenced by F.
 */
% reset_frame_property(+Frame, +Property)
:- public reset_frame_property/2.
:- special(reset_frame_property/2, 'SpecialFrame', 5).
