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
:- reexport(library(minimal/delta)).

/**
 * choose(L):
 * choose(L, G):
 * The construct posts each from L before further solving.
 */
% choose(+List)
:- public choose/1.
choose(L) :-
   sys_least_one(L), !.
choose([A|L]) :-
   choose2(L, A).

% choose2(+List, +Term)
:- private choose2/2.
choose2([], A) :- !,
   post(A).
choose2([_|_], A) :-
   post(A).
choose2([A|L], _) :-
   choose2(L, A).

% choose(+List, +Goal)
:- public choose/2.
:- meta_predicate choose(?,0).
choose(L, G) :-
   sys_least_one(L), !, G.
choose([A|L], G) :-
   choose2(L, A, G).

% choose2(+List, +Term, +Goal)
:- private choose2/3.
:- meta_predicate choose2(?,-1,0).
choose2([], A, G) :- !,
   post(A, G).
choose2([_|_], A, G) :-
   post(A, G).
choose2([A|L], _, G) :-
   choose2(L, A, G).

% sys_least_one(+List)
:- private sys_least_one/1.
sys_least_one([A|_]) :-
   clause(A, true), !.
sys_least_one([_|L]) :-
   sys_least_one(L).
