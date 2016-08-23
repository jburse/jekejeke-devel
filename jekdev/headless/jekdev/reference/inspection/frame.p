/**
 * When a clause is instantiated a new frame for the variables is
 * created. Practical Prolog systems store additional information
 * in a frame. We provide access to frame properties via frame
 * references. The predicates clause_frame/3 and retract_frame/2
 * and searches a clause and return the frame reference. The
 * predicate retract_frame/2 additionally removes a found clause.
 * These predicates are able to retrieve clauses unrestrictedly,
 * namely it is possible to retrieve clauses from dynamic or
 * static predicates.
 *
 * The predicates frame_property/2, set_frame_property/2 and
 * reset_frame_property/2 allow inspecting and modifying frame
 * properties. Not only the predicates clause/3 and retract/2
 * return a frame reference. A reference to a frame can also be
 * created from a clause reference via the predicate
 * sys_instance_clause/2.
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

:- use_package(foreign(jekdev/reference/inspection)).

:- module(user, []).

/**
 * clause_frame(H, B, F):
 * The predicate succeeds with the user clauses that match H :- B. The
 * predicate also unifies F with the new frame reference for the
 * found clauses.
 */
% clause_frame(+Term, -Goal, -Frame)
:- public clause_frame/3.
:- meta_predicate clause_frame(-1,0,?).
:- set_predicate_property(clause_frame/3, sys_noexpand).
:- special(clause_frame/3, 'SpecialFrame', 0).

/**
 * retract_frame(C, F):
 * The predicate removes and succeeds with the user clauses that
 * match C. The predicate also unifies F with the new frame reference
 * for the found clauses.
 */
% retract_frame(+Term, -Frame)
:- public retract_frame/2.
:- meta_predicate retract_frame(-1,?).
:- set_predicate_property(retract_frame/2, sys_noexpand).
:- special(retract_frame/2, 'SpecialFrame', 1).

/**
 * sys_instance_clause(R, F):
 * The predicate succeeds with a new frame reference F for the
 * rule, fact or directive referenced by R.
 */
% sys_instance_clause(+Reference, -Frame)
:- public sys_instance_clause/2.
:- special(sys_instance_clause/2, 'SpecialFrame', 2).

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
:- special(sys_frame_property/2, 'SpecialFrame', 3).

:- private sys_frame_property_chk/3.
:- special(sys_frame_property_chk/3, 'SpecialFrame', 4).

% a frame property
:- public sys_clause_term/1.
:- meta_predicate sys_clause_term(-1).
sys_clause_term(_) :-
   throw(error(existence_error(body,sys_clause_term/1),_)).

/**
 * set_frame_property(F, P):
 * The predicate assigns the property P to the clause referenced by F.
 */
% set_frame_property(+Frame, +Property)
:- public set_frame_property/2.
:- special(set_frame_property/2, 'SpecialFrame', 5).

/**
 * reset_frame_property(F, P):
 * The predicate de-assigns the property P from the clause referenced by F.
 */
% reset_frame_property(+Frame, +Property)
:- public reset_frame_property/2.
:- special(reset_frame_property/2, 'SpecialFrame', 6).



