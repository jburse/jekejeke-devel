/**
 * When a goal belonging to a defined predicate is invoked a new frame
 * is created. When the interpreter encounters a cut (!) it will remove
 * the choice points inside the frame. To instruct the interpreter that
 * cuts can nevertheless propagate a defined predicate can be marked as
 * cut transparent via the predicate property sys_nobarrier/1.
 *
 * Examples:
 * ?- X = a; X = b.
 * X = a ;
 * X = b
 * ?- X = a, !; X = b.
 * X = a
 *
 * Some of the predefined logical predicates are cut transparent in all
 * arguments. This includes the predicates (,)/2 and (;)/2. Others are
 * only cut transparent in a few arguments. This includes the predicates
 * (->)/2 and (*->)/2, and also the special forms in connection with the
 * predicate (;)/2. Others are not cut transparent at all. This includes
 * the predicates sys_call/1, call/1, once/1 and (\+)/1.
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

:- public infix('|').
:- op(1105, xfy, '|').

:- public infix(;).
:- op(1100, xfy, ;).
:- set_oper_property(infix(;), nspl).

:- public infix(->).
:- op(1050, xfy, ->).

:- public infix(*->).
:- op(1050, xfy, *->).

:- set_prolog_flag(sys_body_convert, off).

/**
 * A; B: [ISO 7.8.6]
 * The predicate succeeds whenever A or B succeeds. Both goal
 * arguments A and B are cut transparent.
 */
/**
 * A -> B;C: [ISO 7.8.8]
 * The predicate succeeds when A succeeds and then whenever B succeeds, or
 * else whenever C succeeds. Only the goal arguments B and C are
 * cut transparent.
 */
/**
 * A *-> B; C:
 * The predicate succeeds whenever A and B succeed, or else if B didn’t
 * succeed whenever C succeeds. Only the goal arguments B and C are
 * cut transparent.
 */
% +Goal ; +Goal
:- public ;/2.
:- meta_predicate ;(0,0).
:- set_predicate_property(;/2, sys_body).
:- set_predicate_property(;/2, sys_notrace).
:- set_predicate_property(;/2, sys_nobarrier).
A -> B; C :- sys_local_cut,
   sys_cond(A, B, C).
A *-> B; C :- sys_local_cut,
   sys_soft_cond(A, B, C).
A; _ :- A.                                        % Proto
_; B :- B.                                        % Proto

% sys_cond(+Goal, +Goal, +Goal)
:- private sys_cond/3.
:- meta_predicate sys_cond(0,0,0).
:- set_predicate_property(sys_cond/3, sys_nobarrier).
sys_cond(A, B, _) :-
   sys_safe(A), sys_local_cut, B.                              % Proto
sys_cond(_, _, C) :- C.                           % Proto

% sys_soft_cond(+Goal, +Goal, +Goal)
:- private sys_soft_cond/3.
:- meta_predicate sys_soft_cond(0,0,0).
:- set_predicate_property(sys_soft_cond/3, sys_nobarrier).
sys_soft_cond(A, B, _) :-
   sys_safe(A), sys_soft_local_cut, B.                         % Proto
sys_soft_cond(_, _, C) :- C.                      % Proto

/**
 * sys_soft_local_cut:
 * The predicate marks the choice point of the direct parent, if there is any
 * at all, as non-redo able and then succeeds once.
 */
:- private sys_soft_local_cut/0.
:- special(sys_soft_local_cut/0, 'SpecialLogic', 1).

/**
 * A -> B: [ISO 7.8.7]
 * The predicate succeeds when A succeeds and then whenever B
 * succeeds. Only the goal argument B is cut transparent.
 */
% +Goal -> +Goal
:- public -> /2.
:- meta_predicate ->(0,0).
:- set_predicate_property(-> /2, sys_body).
:- set_predicate_property(-> /2, sys_notrace).
:- set_predicate_property(-> /2, sys_nobarrier).
A -> B :-
   sys_safe(A), sys_local_cut, B.                              % Proto

/**
 * sys_local_cut:
 * The predicate removes pending choice points between the direct parent
 * goal invocation and this goal and then succeeds once.
 */
:- private sys_local_cut/0.
:- special(sys_local_cut/0, 'SpecialLogic', 0).

/**
 * A *-> B:
 * The predicate succeeds whenever A and B succeed. Only the goal
 * argument B is cut transparent.
 */
% +Goal *-> +Goal
:- public *-> /2.
:- meta_predicate *->(0,0).
:- set_predicate_property(*-> /2, sys_body).
:- set_predicate_property(*-> /2, sys_notrace).
:- set_predicate_property(*-> /2, sys_nobarrier).
A *-> B :-
   sys_safe(A), B.                                % Proto

/**
 * sys_safe(A):
 * The predicate succeeds whenever A succeeds. The goal
 * argument A is not converted before calling.
 */
% sys_safe(+Goal)
:- private sys_safe/1.
:- special(sys_safe/1, 'SpecialLogic', 2).
:- meta_predicate sys_safe(0).

:- set_prolog_flag(sys_body_convert, on).

/**
 * repeat: [ISO 8.15.3]
 * The predicate succeeds repeatedly.
 */
:- public repeat/0.
repeat.
repeat :- repeat.
