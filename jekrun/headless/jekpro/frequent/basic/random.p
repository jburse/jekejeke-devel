/**
 * The evaluable functions random/0 and random/1 generate uniformly
 * distributed members of the arithmetic domains. Each knowledge base
 * has its own random number generator which can be accessed
 * concurrently.
 *
 * Examples:
 * random           --> 0.6011883752343405
 * random(100)      --> 61
 *
 * The result type of the evaluable function random/0 is always a
 * float. The result type of the evaluable function random/1 reflects
 * the type of the argument.
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

:- module(random, []).

/**
 * random:
 * The function returns a continuous uniform random
 * number in the interval [0..1).
 */
% random : float
:- public random/1.
:- foreign_fun(random/1, 'ForeignRandom',
      sysRandom('Interpreter')).

/**
 * random(M):
 * The function returns a uniform random number in the interval [0..M)
 * for M>0. The distribution is discrete when M is discrete and
 * continuous otherwise.
 */
% random : integer -> integer
% random : float -> float
% random : decimal -> decimal
:- public random/2.
:- foreign_fun(random/2, 'ForeignRandom',
      sysRandom('Interpreter','Number')).
