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
