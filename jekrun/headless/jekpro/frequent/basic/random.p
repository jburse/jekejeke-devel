/**
 * The evaluable functions random/0 and random/1 generate uniformly
 * distributed members of the arithmetic domains. Each knowledge base
 * has its own pre-allocated random number generator which can be
 * accessed concurrently. Random number generator objects can be
 * created with the predicates random_new/1 and random_new/2.
 *
 * Examples:
 * random           --> 0.6011883752343405
 * random(100)      --> 61
 *
 * The result type of the evaluable function random/0 is always a
 * Prolog float, which amounts to a Java double. The result type of
 * the evaluable function random/1 reflects the type of the argument.
 * The predicates random_next/2 and random_next/3 do the same, except
 * that they take an additional random number generator object as
 * a first parameter.
 *
 * The predicate counter_new/1 can be used to create a counter whch
 * will be initialized to zero. The counter can then be incremented
 * via the counter_next/2 whereby the old value is returned. The later
 * predicate is implemented with the help of an atomic integer.
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
:- use_package(foreign(jekpro/frequent/basic)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(java/util)).

:- module(random, []).

/****************************************************************/
/* Knowledge Base Random Numbers                                */
/****************************************************************/

/**
 * random(F):
 * The predicate succeeds for a continuous uniform random
 * number F in the interval [0..1) from the knowledgebase
 * random number generator.
 */
% random(-Float)
:- public random/1.
random(F) :-
   current_prolog_flag(sys_random, R),
   random_next(R, F).

/**
 * random(M, N):
 * The predicate succeeds for a uniform random number N in the
 * interval [0..M) for M>0 from the knowledgebase random number
 * generator. The distribution is discrete when M is discrete
 * and continuous otherwise.
 */
% random(+Number, -Number)
:- public random/2.
random(M, N) :-
   current_prolog_flag(sys_random, R),
   random_next(R, M, N).

/****************************************************************/
/* Object Parameter Random Numbers                              */
/****************************************************************/

/**
 * random_new(R):
 * The predicate succeeds for a new random number generator
 * R with a randomized seed.
 */
% random_new(-Random)
:- public random_new/1.
:- foreign_constructor(random_new/1, 'Random', new).

/**
 * random_new(S, R):
 * The predicate succeeds for a new random number generator
 * R with seed S.
 */
% random_new(+Integer, -Random)
:- public random_new/2.
:- foreign_constructor(random_new/2, 'Random', new(long)).

/**
 * random_next(R, F):
 * The predicate succeeds for a continuous uniform random
 * number F in the interval [0..1) from the random number
 * generator R.
 */
% random_next(+Random, -Float)
:- public random_next/2.
:- virtual random_next/2.
:- foreign(random_next/2, 'Random', nextDouble).

/**
 * random_next(R, M, N):
 * The predicate succeeds for a uniform random number N in the
 * interval [0..M) for M>0 from the random number generator R.
 * The distribution is discrete when M is discrete and continuous
 * otherwise.
 */
% random_next(+Random, +Number, -Number)
:- public random_next/3.
:- foreign(random_next/3, 'ForeignRandom',
      sysRandomNext('Random','Number')).

/****************************************************************/
/* Counter Object                                               */
/****************************************************************/

/**
 * counter_new(C):
 * The predicate succeeds for a new counter C.
 */
% counter_new(-Counter)
:- public counter_new/1.
:- foreign_constructor(counter_new/1, 'Counter', new).

/**
 * counter_next(C, V):
 * The predicate succeeds for incrementing the
 * counter C and unifying the old value V
 */
% counter_next(+Counter, -Integer)
:- public counter_next/2.
:- virtual counter_next/2.
:- foreign(counter_next/2, 'Counter', next).

/****************************************************************/
/* Advanced Predicates                                          */
/****************************************************************/

/**
 * random_permutation(L, R):
 * random_permutation(G, L, R):
 * The predicate succeeds in R with a random permutation of L.
 * The ternary predicate allows specifying a random generator G.
 */
% random_permutation(+List, -List)
:- public random_permutation/2.
random_permutation(L, R) :-
   current_prolog_flag(sys_random, G),
   random_permutation(G, L, R).

% random_permutation(+Random, +List, -List)
:- public random_permutation/3.
random_permutation(G, L, R) :-
   add_random_keys(L, G, H),
   keysort(H, J),
   remove_keys(J, R).

% add_random_keys(+List, +Random, -Pairs)
:- private add_random_keys/3.
add_random_keys(X, _, _) :-
   var(X),
   throw(error(instantiation_error,_)).
add_random_keys([X|L], G, [K-X|R]) :- !,
   random_next(G, K),
   add_random_keys(L, G, R).
add_random_keys([], _, []) :- !.
add_random_keys(X, _, _) :-
   throw(error(type_error(list,X),_)).

% remove_keys(+Pairs, -List)
:- private remove_keys/2.
remove_keys([_-X|L], [X|R]) :-
   remove_keys(L, R).
remove_keys([], []).

/**
 * randset(K, N, L):
 * randset(G, K, N, L):
 * The predicate succeeds in L with a ordered random set of K
 * elements from the interval 1..N. The quarternary predicate
 * allows specifying a random generator G.
 */
% randset(+Integer, +Integer, -List)
:- public randset/3.
randset(K, N, L) :-
   current_prolog_flag(sys_random, G),
   randset(G, K, N, L).

% randset(+Random, +Integer, +Integer, -List)
:- public randset/4.
randset(G, K, N, L) :-
   randset_build(K, G, N, [], L).

% randset_build(+Integer, +Random, +Integer, +List, -List)
:- private randset_build/5.
randset_build(0, _, _, L, R) :- !,
   R = L.
randset_build(K, G, N, L, R) :-
   random_next(G, N, Y),
   X is Y+1,
   adjust_element(X, L, H),
   J is K-1,
   M is N-1,
   randset_build(J, G, M, H, R).

% adjust_element(+Integer, +List, -List)
:- private adjust_element/3.
adjust_element(X, [Y|Z], L) :-
   X < Y, !,
   L = [X,Y|Z].
adjust_element(X, [Y|Z], [Y|T]) :-
   U is X+1,
   adjust_element(U, Z, T).
adjust_element(X, [], [X]).

/**
 * randseq(K, N, L):
 * The predicate succeeds in L with a random permutation of K
 * elements from the interval 1..N. The quarternary predicate
 * allows specifying a random generator G.
 */
% randseq(+Integer, +Integer, -List)
:- public randseq/3.
randseq(K, N, L) :-
   current_prolog_flag(sys_random, G),
   randseq(G, K, N, L).

% randseq(+Random, +Integer, +Integer, -List)
:- public randseq/4.
randseq(G, K, N, L) :-
   randset(G, K, N, H),
   random_permutation(G, H, L).
