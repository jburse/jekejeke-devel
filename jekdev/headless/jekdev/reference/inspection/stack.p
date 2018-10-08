/**
 * When a clause is invoked a special kind of a frame is created.
 * Such a stack frame might have empty variable bindings or not yet
 * a parent, so that the corresponding property might return null/0
 * elements or missing.
 *
 * Besides that a stack frame constitutes an element of the ancestor
 * goal list. It therefore provides additional properties such accessing
 * the parent stack frame or the current call goal.
 *
 * A stack frame where all the variable bindings are initially empty
 * where there is no parent can be created with the predicate
 * sys_prepare_clause/2. Such a stack frame can be executed by
 * the predicate sys_unfold_body/1.
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

:- module(stack, []).

% some stack property
:- public sys_call_goal/1.
:- meta_predicate sys_call_goal(0).
sys_call_goal(_) :-
   throw(error(existence_error(body,sys_call_goal/1),_)).

/**
 * sys_prepare_clause(R, F):
 * The predicate succeeds with a new stack frame reference F
 * for the rule, fact or directive referenced by R.
 */
% sys_prepare_clause(+Reference, -Frame)
:- public sys_prepare_clause/2.
:- special(sys_prepare_clause/2, 'SpecialStack', 1).

/**
 * sys_unfold_body(F):
 * The predicate succeeds whenever the directive of the stack frame F succeeds.
 */
% sys_unfold_body(+Frame)
:- public sys_unfold_body/1.
:- special(sys_unfold_body/1, 'SpecialStack', 1).
