/**
 * This module provides persistent lists. Prolog Lists are written
 * as [x1,..,xn] and are internally constructed by the pairing
 * constructor [h|t] and the empty constructor []. The length
 * of such a list is n and the i-th element is xi. Most predicates
 * are implemented such that they leave as few as possible choice
 * points.
 *
 * Example:
 * ?- last([1,2,3], X).
 * X = 3
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

:- package(library(jekpro/frequent/basic)).

:- module(lists, []).
:- use_module(library(advanced/arith)).

/**
 * append(L1, L2, L3):
 * The predicate succeeds whenever L3 unifies with the concatenation of L1 and L2.
 */
% append(+List, +List, -List)
:- public append/3.
append([], X, X).
append([X|Y], Z, [X|T]) :-
   append(Y, Z, T).

/**
 * reverse(L1, L2):
 * The predicate succeeds whenever L2 unifies with the reverse of L1. The current
 * implementation does not terminate on redo for an input L2 and an output L1.
 */
% reverse(+List, -List)
:- public reverse/2.
reverse(X, Y) :-
   reverse2(X, [], Y).

% reverse2(+List, +List, -List)
:- private reverse2/3.
reverse2([], X, X).
reverse2([X|Y], Z, T) :-
   reverse2(Y, [X|Z], T).

/**
 * member(E, L):
 * The predicate succeeds for every member E of the list L.
 */
% member(-Elem, +List)
:- public member/2.
member(X, [Y|Z]) :-
   member2(Z, X, Y).

% member2(+List, +Elem, -Elem)
:- private member2/3.
member2(_, X, X).
member2([Y|Z], X, _) :-
   member2(Z, X, Y).

/**
 * select(E, L, R):
 * The predicate succeeds for every member E of the L with remainder list R.
 */
% select(-Elem, +List, -List)
:- public select/3.
select(Z, [X|Y], T) :-
   select2(Y, X, Z, T).

% select2(+List, +Elem, -Elem, -List)
:- private select2/4.
select2(Y, X, X, Y).
select2([X|Y], W, Z, [W|T]) :-
   select2(Y, X, Z, T).

/**
 * last(L, E):
 * The predicate succeeds with E being the last element of the list L.
 */
% last(+List, -Elem)
:- public last/2.
last([X|Y], Z) :-
   last2(Y, X, Z).

% last2(+List, +Elem, -Elem)
:- private last2/3.
last2([], X, X).
last2([X|Y], _, Z) :-
   last2(Y, X, Z).

/**
 * last(L, E, R):
 * The predicate succeeds with E being the last element of the list L
 * and R being the remainder of the list.
 */
% last(+List, -Elem, -List)
:- public last/3.
last([X|Y], Z, T) :-
   last2(Y, X, Z, T).

% last2(+List, +Elem, -Elem, -List)
:- private last2/4.
last2([], X, X, []).
last2([X|Y], U, Z, [U|T]) :-
   last2(Y, X, Z, T).

/**
 * length(L, N):
 * The predicate succeeds with N being the length of the list L.
 */
% length(+List, -Integer)
:- public length/2.
length(L, N) :-
   var(N), !,
   length2(L, N).
length(L, N) :-
   integer(N), !,
   N >= 0,
   length3(N, L).
length(_, N) :-
   throw(error(type_error(integer,N),_)).

% length2(+List, -Integer)
:- private length2/2.
length2([], 0).
length2([_|Y], N) :-
   length2(Y, M),
   succ(M, N).

% length3(+Integer, -List)
:- private length3/2.
length3(0, []) :- !.
length3(0, _) :- !, fail.
length3(N, [_|Y]) :-
   M is N-1,
   length3(M, Y).

/**
 * nth0(I, L, E):
 * The predicate succeeds with E being the (I+1)-th element of the list L.
 */
% nth0(+Integer, +List, -Elem)
:- public nth0/3.
nth0(N, L, E) :-
   var(N), !,
   L = [X|Y],
   nth02(Y, X, E, N).
nth0(N, L, E) :-
   integer(N), !,
   N >= 0,
   nth03(N, L, E).
nth0(N, _, _) :-
   throw(error(type_error(integer,N),_)).

% nth02(+List, +Elem, -Elem, -Integer)
:- private nth02/4.
nth02(_, X, X, 0).
nth02([X|Y], _, Z, N) :-
   nth02(Y, X, Z, M),
   succ(M, N).

% nth03(+Integer, -List, -Elem)
:- private nth03/3.
nth03(0, [X|_], X) :- !.
nth03(0, _, _) :- !, fail.
nth03(N, [_|Y], X) :-
   M is N-1,
   nth03(M, Y, X).

/**
 * nth0(I, L, E, R):
 * The predicate succeeds with E being the (I+1)-th element of the list L
 * and R being the remainder of the list.
 */
% nth0(+Integer, +List, -Elem, -List)
:- public nth0/4.
nth0(N, L, E, R) :-
   var(N), !,
   L = [X|Y],
   nth02(Y, X, E, N, R).
nth0(N, L, E, R) :-
   integer(N), !,
   N >= 0,
   nth03(N, L, E, R).
nth0(N, _, _, _) :-
   throw(error(type_error(integer,N),_)).

% nth02(+List, +Elem, -Elem, -Integer, -List)
:- private nth02/5.
nth02(Y, X, X, 0, Y).
nth02([X|Y], H, Z, N, [H|T]) :-
   nth02(Y, X, Z, M, T),
   succ(M, N).

% nth03(+Integer, -List, -Elem, -List)
:- private nth03/4.
nth03(0, [X|Y], X, Y) :- !.
nth03(0, _, _, _) :- !, fail.
nth03(N, [H|Y], X, [H|T]) :-
   M is N-1,
   nth03(M, Y, X, T).

/**
 * nth1(I, L, E):
 * The predicate succeeds with E being the I-th element of the list L.
 */
% nth1(+Integer, +List, -Elem)
:- public nth1/3.
nth1(N, L, E) :-
   var(N), !,
   L = [X|Y],
   nth12(Y, X, E, N).
nth1(N, L, E) :-
   integer(N), !,
   N >= 1,
   nth13(N, L, E).
nth1(N, _, _) :-
   throw(error(type_error(integer,N),_)).

% nth12(+List, +Elem, -Elem, -Integer)
:- private nth12/4.
nth12(_, X, X, 1).
nth12([X|Y], _, Z, N) :-
   nth12(Y, X, Z, M),
   succ(M, N).

% nth13(+Integer, -List, -Elem)
:- private nth13/3.
nth13(1, [X|_], X) :- !.
nth13(1, _, _) :- !, fail.
nth13(N, [_|Y], X) :-
   M is N-1,
   nth13(M, Y, X).

/**
 * nth1(I, L, E, R):
 * The predicate succeeds with E being the I-th element of the list L
 * and R being the remainder of the list.
 */
% nth1(+Integer, +List, -Elem, -List)
:- public nth1/4.
nth1(N, L, E, R) :-
   var(N), !,
   L = [X|Y],
   nth12(Y, X, E, N, R).
nth1(N, L, E, R) :-
   integer(N), !,
   N >= 1,
   nth13(N, L, E, R).
nth1(N, _, _, _) :-
   throw(error(type_error(integer,N),_)).

% nth12(+List, +Elem, -Elem, -Integer, -List)
:- private nth12/5.
nth12(Y, X, X, 1, Y).
nth12([X|Y], H, Z, N, [H|T]) :-
   nth12(Y, X, Z, M, T),
   succ(M, N).

% nth13(+Integer, -List, -Elem, -List)
:- private nth13/4.
nth13(1, [X|Y], X, Y) :- !.
nth13(1, _, _, _) :- !, fail.
nth13(N, [H|Y], X, [H|T]) :-
   M is N-1,
   nth13(M, Y, X, T).
