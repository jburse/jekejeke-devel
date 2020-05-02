/**
 * When a goal belonging to a defined predicate is invoked a new
 * frame is created. When the interpreter encounters a cut (!) it
 * will remove all choice points inside this frame. The cut (!) will
 * also remove choice points that were created by disjunction (;)/2.
 * On these grounds the predicate (;)/2 is called cut transparent.
 *
 * Examples:
 * ?- X = a; X = b.
 * X = a ;
 * X = b
 * ?- X = a, !; X = b.
 * X = a
 *
 * Some of the predefined logical predicates are cut transparent in
 * all arguments. This includes the predicates (,)/2 and (;)/2.
 * Others are only cut transparent in the second argument. This
 * includes the predicates (->)/2 and (*->)/2. Other predicates,
 * such as call/1, once/1 and (\+)/1, are not cut transparent at all.
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
:- set_oper_property(infix(;), sys_nspl).

:- public infix(->).
:- op(1050, xfy, ->).

:- public infix(*->).
:- op(1050, xfy, *->).

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
:- sys_notrace ;/2.
:- meta_predicate ;(0, 0).
_; _ :- throw(error(existence_error(body, ;/2), _)).

/**
 * A -> B: [ISO 7.8.7]
 * The predicate succeeds when A succeeds and then whenever B
 * succeeds. Only the goal argument B is cut transparent.
 */
% +Goal -> +Goal
:- public -> /2.
:- sys_notrace -> /2.
:- meta_predicate ->(0, 0).
_ -> _ :- throw(error(existence_error(body, -> /2), _)).

/**
 * A *-> B:
 * The predicate succeeds whenever A and B succeed. Only the goal
 * argument B is cut transparent.
 */
% +Goal *-> +Goal
:- public *-> /2.
:- sys_notrace *-> /2.
:- meta_predicate *->(0, 0).
_ *-> _ :- throw(error(existence_error(body, *-> /2), _)).

/**
 * repeat: [ISO 8.15.3]
 * The predicate succeeds repeatedly.
 */
% repeat
:- public repeat/0.
repeat.
repeat :- repeat.

/**
 * forall(A,B): [N208 8.10.4]
 * The predicate succeeds when there is no success of A
 * such that B fails. Otherwise the predicate fails.
 */
:- public forall/2.
:- meta_predicate forall(0, 0).
forall(A, B) :- \+ (A, \+ B).

/**
 * findall(T, G, L): [ISO 8.10.1]
 * findall(T, G, L, R):
 * The predicate first finds all the solutions to the goal G, whereby
 * collecting copies of the template T in a list. The predicate then
 * succeeds when L unifies with the list.
 */
% findall(+Template, +Goal, -List)
:- public findall/3.
:- meta_predicate findall(?, 0, ?).
:- special(findall/3, 'SpecialLogic', 0).

% findall(+Template, +Goal, -List, +List)
:- public findall/4.
:- meta_predicate findall(?, 0, ?, ?).
:- special(findall/4, 'SpecialLogic', 1).
