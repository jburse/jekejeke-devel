/**
 * This module provides hyperbolic functions. The evaluable functions
 * sinh/1, cosh/1 and tanh/1 compute the hyperbolic sinus, cosine and
 * tangent respectively. The evaluable functions asinh/1, acosh/1 and
 * atanh/1 compute their inverse.
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

:- module(hyper, []).

/**
 * sinh(X):
 * Returns the float representation of the hyperbolic sinus of X.
 */
:- public sinh/2.
:- foreign_fun(sinh/2, 'Math', sinh(double)).

/**
 * cosh(X):
 * Returns the float representation of the hyperbolic cosine of X.
 */
:- public cosh/2.
:- foreign_fun(cosh/2, 'Math', cosh(double)).

/**
 * tanh(X):
 * Returns the float representation of the hyperbolic tangent of X.
 */
:- public tanh/2.
:- foreign_fun(tanh/2, 'Math', tanh(double)).

/**
 * asinh(X):
 * Returns the float representation of the arcus hyperbolic sinus of X.
 */
:- public asinh/2.
:- foreign_fun(asinh/2, 'ForeignHyper', asinh(double)).

/**
 * acosh(X):
 * Returns the float representation of the arcus hyperbolic cosine of X.
 */
:- public acosh/2.
:- foreign_fun(acosh/2, 'ForeignHyper', acosh(double)).

/**
 * atanh(X):
 * Returns the float representation of the arcus hyperbolic tangent of X.
 */
:- public atanh/2.
:- foreign_fun(atanh/2, 'ForeignHyper', atanh(double)).
