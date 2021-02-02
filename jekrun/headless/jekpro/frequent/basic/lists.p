/**
 * This module provides persistent lists. Prolog Lists are written
 * as [x1,..,xn] and are internally constructed by the pairing
 * constructor [h|t] and the empty constructor []. The length
 * of such a list is n and the i-th element is xi. Most predicates
 * are implemented such that they leave as few as possible choice
 * points.
 *
 * Examples:
 * ?- last([1,2,3], X).
 * X = 3
 *
 * ?- last([1,2,3], X, Y).
 * X = 3,
 * Y = [1,2]
 *
 * The predicates append/3, reverse/2, member/2, select/3, last/2
 * and last/3 work directly with lists. The predicates length/2,
 * nth0/3, nth0/4, nth1/3 and nth1/4 take also a length respective
 * index into account. The predicates subtract/3, intersection/3,
 * union/3 and symdiff/3 provide further list operations. To work
 * correctly they require ground lists.
 *
 * Examples:
 * ?- intersection([2,3],[1,2],X).
 * X = [2]
 *
 * ?- union([2,3],[1,2],X).
 * X = [3, 1, 2]
 *
 * Further ground list based predicates are subset/2, disjoint/2 and
 * equal/2. The implementations for the ground list based predicates
 * might differ from other implementations since they do not simply
 * fail or loop if the leading argument is not a ground list. Instead,
 * they throw either an instantiation error or type error to
 * provide more safety.
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

:- package(library(jekpro/frequent/basic)).

:- module(lists, []).

/**
 * append(L1, L2, L3):
 * The predicate succeeds whenever L3 unifies with the concatenation of L1 and L2.
 */
% append(+List, +List, -List)
:- public append/3.
append([], X, X).
append([X|Y], Z, [X|T]) :- append(Y, Z, T).

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
reverse2(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
reverse2([], X, R) :- !,
   R = X.
reverse2([X|Y], Z, T) :- !,
   reverse2(Y, [X|Z], T).
reverse2(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * member(E, L):
 * The predicate succeeds for every member E of the list L.
 */
% member(-Elem, +List)
:- public member/2.
member(X, [Y|Z]) :- member2(Z, X, Y).

% member2(+List, +Elem, -Elem)
:- private member2/3.
member2(_, X, X).
member2([Y|Z], X, _) :- member2(Z, X, Y).

/**
 * select(E, L, R):
 * The predicate succeeds for every member E of the L with remainder list R.
 */
% select(-Elem, +List, -List)
:- public select/3.
select(X, [Y|Z], T) :- select2(Z, X, Y, T).

% select2(+List, +Elem, -Elem, -List)
:- private select2/4.
select2(Y, X, X, Y).
select2([Y|Z], X, W, [W|T]) :- select2(Z, X, Y, T).

/**
 * last(L, E):
 * The predicate succeeds with E being the last element of the list L.
 */
% last(+List, -Elem)
:- public last/2.
last([X|Y], Z) :- last2(Y, Z, X).

% last2(+List, +Elem, -Elem)
:- private last2/3.
last2([], X, X).
last2([X|Y], Z, _) :- last2(Y, Z, X).

/**
 * last(L, E, R):
 * The predicate succeeds with E being the last element of the list L
 * and R being the remainder of the list.
 */
% last(+List, -Elem, -List)
:- public last/3.
last([X|Y], Z, T) :- last2(Y, Z, X, T).

% last2(+List, +Elem, -Elem, -List)
:- private last2/4.
last2([], X, X, []).
last2([X|Y], Z, U, [U|T]) :- last2(Y, Z, X, T).

/**
 * length(L, N):
 * The predicate succeeds with N being the length of the list L.
 */
% length(+List, -Integer)
:- public length/2.
length(L, N) :- var(N), !, length2(L, 0, N).
length(L, N) :- integer(N), !, N >= 0, length3(N, L).
length(_, N) :- throw(error(type_error(integer, N), _)).

% length2(+List, +Integer, -Integer)
:- private length2/3.
length2([], N, N).
length2([_|Y], N, M) :- H is N+1, length2(Y, H, M).

% length3(+Integer, -List)
:- private length3/2.
length3(0, R) :- !, R = [].
length3(N, [_|Y]) :- M is N-1, length3(M, Y).

/**
 * nth0(I, L, E):
 * The predicate succeeds with E being the (I+1)-th element of the list L.
 */
% nth0(+Integer, +List, -Elem)
:- public nth0/3.
nth0(N, L, E) :- var(N), !, L = [X|Y], nth02(Y, X, E, 0, N).
nth0(N, L, E) :- integer(N), !, N >= 0, nth03(N, L, E).
nth0(N, _, _) :- throw(error(type_error(integer, N), _)).

% nth02(+List, +Elem, -Elem, +Integer, -Integer)
:- private nth02/5.
nth02(_, X, X, N, N).
nth02([X|Y], _, Z, N, M) :- H is N+1, nth02(Y, X, Z, H, M).

% nth03(+Integer, -List, -Elem)
:- private nth03/3.
nth03(0, R, X) :- !, R = [X|_].
nth03(N, [_|Y], X) :- M is N-1, nth03(M, Y, X).

/**
 * nth0(I, L, E, R):
 * The predicate succeeds with E being the (I+1)-th element of the list L
 * and R being the remainder of the list.
 */
% nth0(+Integer, +List, -Elem, -List)
:- public nth0/4.
nth0(N, L, E, R) :- var(N), !, L = [X|Y], nth02(Y, X, E, 0, N, R).
nth0(N, L, E, R) :- integer(N), !, N >= 0, nth03(N, L, E, R).
nth0(N, _, _, _) :- throw(error(type_error(integer, N), _)).

% nth02(+List, +Elem, -Elem, +Integer, -Integer, -List)
:- private nth02/6.
nth02(Y, X, X, N, N, Y).
nth02([X|Y], H, Z, N, M, [H|T]) :- J is N+1, nth02(Y, X, Z, J, M, T).

% nth03(+Integer, -List, -Elem, -List)
:- private nth03/4.
nth03(0, R, X, Y) :- !, R = [X|Y].
nth03(N, [H|Y], X, [H|T]) :- M is N-1, nth03(M, Y, X, T).

/**
 * nth1(I, L, E):
 * The predicate succeeds with E being the I-th element of the list L.
 */
% nth1(+Integer, +List, -Elem)
:- public nth1/3.
nth1(N, L, E) :- var(N), !, L = [X|Y], nth02(Y, X, E, 1, N).
nth1(N, L, E) :- integer(N), !, N >= 1, H is N-1, nth03(H, L, E).
nth1(N, _, _) :- throw(error(type_error(integer, N), _)).

/**
 * nth1(I, L, E, R):
 * The predicate succeeds with E being the I-th element of the list L
 * and R being the remainder of the list.
 */
% nth1(+Integer, +List, -Elem, -List)
:- public nth1/4.
nth1(N, L, E, R) :- var(N), !, L = [X|Y], nth02(Y, X, E, 1, N, R).
nth1(N, L, E, R) :- integer(N), !, N >= 1, H is N-1, nth03(H, L, E, R).
nth1(N, _, _, _) :- throw(error(type_error(integer, N), _)).

/*******************************************************************/
/* Set Operations                                               */
/*******************************************************************/

/**
 * subtract(S, T, R):
 * The predicate succeeds when R unifies with the subtract of S by T.
 */
% subtract(+List, +List, -List)
:- public subtract/3.
subtract(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
subtract([X|Y], Z, T) :-
   member(X, Z), !,
   subtract(Y, Z, T).
subtract([X|Y], Z, R) :- !,
   R = [X|T],
   subtract(Y, Z, T).
subtract([], _, R) :- !,
   R = [].
subtract(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * intersection(S, T, R):
 * The predicate succeeds when R unifies with the intersection of S and T.
 */
% intersection(+List, +List, -List)
:- public intersection/3.
intersection(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
intersection([X|L], R, T) :-
   member(X, R), !,
   T = [X|S],
   intersection(L, R, S).
intersection([_|L], R, S) :- !,
   intersection(L, R, S).
intersection([], _, T) :- !,
   T = [].
intersection(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * union(S, T, R):
 * The predicate succeeds when R unifies with the union of S and T.
 */
% union(+List, +List, -List)
:- public union/3.
union(X, _, _) :- var(X),
   throw(error(instantiation_error, _)).
union([X|L], R, S) :-
   member(X, R), !,
   union(L, R, S).
union([X|L], R, T) :- !,
   T = [X|S],
   union(L, R, S).
union([], R, T) :- !,
   T = R.
union(X, _, _) :-
   throw(error(type_error(list, X), _)).

/**
 * symdiff(S, T, R):
 * The predicate succeeds when R unifies with the symmetric subtract of S and T.
 */
% symdiff(+List, +List, -List)
:- public symdiff/3.
symdiff(X, Y, Z) :-
   subtract(X, Y, H),
   subtract(Y, X, J),
   append(H, J, Z).

/*******************************************************************/
/* Set Tests                                                       */
/*******************************************************************/

/**
 * subset(S, T):
 * The predicate succeeds when S is a subset of T.
 */
% subset(+List, +List)
:- public subset/2.
subset(X, _) :- var(X),
   throw(error(instantiation_error, _)).
subset([X|Y], Z) :-
   member(X, Z), !,
   subset(Y, Z).
subset([_|_], _) :- !, fail.
subset([], _) :- !.
subset(X, _) :-
   throw(error(type_error(list, X), _)).

/**
 * disjoint(S, T):
 * The predicate succeeds when S is disjoint to T.
 */
% disjoint(+List, +List)
:- public disjoint/2.
disjoint(X, _) :- var(X),
   throw(error(instantiation_error, _)).
disjoint([X|_], Z) :-
   member(X, Z), !, fail.
disjoint([_|Y], Z) :- !,
   disjoint(Y, Z).
disjoint([], _) :- !.
disjoint(X, _) :-
   throw(error(type_error(list, X), _)).

/**
 * equal(S, T):
 * The predicate succeeds when S is equal to T.
 */
% equal(+List, +List)
:- public equal/2.
equal(X, Y) :-
   subset(X, Y),
   subset(Y, X).
