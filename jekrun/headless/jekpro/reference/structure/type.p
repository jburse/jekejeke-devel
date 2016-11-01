/**
 * Type testing allows checking terms for their type without an
 * attempt to instantiate. These predicates are therefore meta-logical.
 * The basic data types of the ISO Prolog core standard are variable,
 * atom, integer, float and compound. The Jekejeke Prolog system adds to
 * these data types the data types reference and decimal.
 *
 * Example:
 * ?- callable(p(X,Y)).
 * Yes
 * ?- callable(1).
 * No
 *
 * We find elementary test predicates such as var/1, atom/1, integer/1,
 * float/1 and compound/1. For the Jekejeke Prolog specific data types
 * we find the test predicates reference/1, decimal/1 float32/1 and
 * float64/1. We find also test predicates that group different data
 * types together such as nonvar/1, atomic/1, number/1 and callable/1.
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

:- use_package(foreign(jekpro/reference/structure)).

:- module(user, []).

/**
 * integer(X): [ISO 8.3.3]
 * The predicate succeeds when X is an integer.
 */
:- public integer/1.
:- special(integer/1, 'SpecialType', 0).

/**
 * float(X): [ISO 8.3.4]
 * The predicate succeeds when X is a float.
 */
:- public float/1.
:- special(float/1, 'SpecialType', 1).

/**
 * atom(X): [ISO 8.3.2]
 * The predicate succeeds when X is an atom.
 */
:- public atom/1.
:- special(atom/1, 'SpecialType', 2).

/**
 * compound(X): [ISO 8.3.6]
 * The predicate succeeds when X is a compound.
 */
:- public compound/1.
:- special(compound/1, 'SpecialType', 3).

/**
 * reference(X):
 * The predicate succeeds when X is a reference.
 */
:- public reference/1.
:- special(reference/1, 'SpecialType', 4).

/**
 * decimal(X):
 * The predicate succeeds when X is a decimal.
 */
:- public decimal/1.
:- special(decimal/1, 'SpecialType', 5).

/**
 * number(X): [ISO 8.3.8]
 * The predicate succeeds when X is a number, i.e. an integer, a float or a decimal.
 */
:- public number/1.
:- special(number/1, 'SpecialType', 6).

/**
 * callable(X): [TC2 8.3.9]
 * The predicate succeeds when X is callable, i.e. an atom or a compound.
 */
:- public callable/1.
:- special(callable/1, 'SpecialType', 7).

/**
 * atomic(X): [ISO 8.3.5]
 * The predicate succeeds when X is a constant, i.e. an atom, a number
 * or a reference.
 */
:- public atomic/1.
:- special(atomic/1, 'SpecialType', 8).

/**
 * var(X): [ISO 8.3.1]
 * The predicate succeeds when X is a variable.
 */
% already defined in member

/**
 * nonvar(X): [ISO 8.3.7]
 * The predicate succeeds when X is not a variable, i.e. atomic or compound.
 */
% already defined in member

/**
 * float32(X):
 * The predicate succeeds when X is a float32.
 */
:- public float32/1.
:- special(float32/1, 'SpecialType', 9).

/**
 * float64(X):
 * The predicate succeeds when X is a float64.
 */
:- public float64/1.
:- special(float64/1, 'SpecialType', 10).
