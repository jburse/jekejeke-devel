/**
 * This module provides some choice operators. Although the operators
 * are just goals, they are not supposed to be used in the condition
 * part of a forward chaining rule, rather in the action part.
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

:- package(library(jekmin/reference/minimal)).

:- module(asp, []).
:- use_module(library(minimal/delta)).

/**
 * choice(A, B):
 * choice(A, B, G):
 * The construct does A and then B before further solving or only A
 * before further solving or it does B before further solving.
 */
% choice(+Goal, +Goal)
:- public choice/2.
choice(A, B) :- A,
   choice2(B).
choice(_, B) :- B.

% choice2(+Goal)
:- private choice2/1.
choice2(B) :- B.
choice2(_).

% choice(+Goal, +Goal, +Goal)
:- public choice/3.
choice(A, B, G) :-
   call(A, choice2(B,G)).
choice(_, B, G) :-
   call(B, G).

% choice2(+Goal, +Goal)
:- private choice2/2.
choice2(B, G) :-
   call(B, G).
choice2(_, G) :- G.

/**
 * post_absent(A):
 * post_absent(A, G):
 * The construct posts A if A is absent.
 */
% post_absent(+Term)
:- public post_absent/1.
post_absent(A) :-
   clause(A, true), !.
post_absent(A) :-
   post(A).

% post_absent(+Term, +Goal)
:- public post_absent/2.
post_absent(A, G) :-
   clause(A, true), !, G.
post_absent(A, G) :-
   post(A, G).

