/**
 * We provide a couple of additional bitwise operations. The evaluable
 * functions setbit/2 and clearbit/2 update the given integer in a
 * more efficient way than would be possible with existing logical
 * and shift operations:
 *
 * Examples:
 * ?- testbit(-100,3).
 * Yes
 * ?- testbit(100,3).
 * No
 *
 * The predicate  testbit/2 tests a particular bit in a given integer, again
 * the implementation is more efficient than would be possible with
 * existing logical, shift and test operations. The evaluable functions
 * and the predicate require a positive or zero shift.
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

:- package(library(jekmin/reference/misc)).
:- use_package(foreign(jekmin/reference/misc)).

:- module(bits, []).

/**
 * setbit(X, Y, Z):
 * If X and Y are integers than the function returns X \/ (1 << Y).
 */
:- public setbit/3.
:- special(setbit/3, 'SupplementBits', 3).

/**
 * clearbit(X, Y, Z):
 * If X and Y are integers than the function returns X /\ \ (1 << Y).
 */
:- public clearbit/3.
:- special(clearbit/3, 'SupplementBits', 4).

/**
 * testbit(X, Y):
 * The predicate succeeds when X /\ (1 << Y) =\= 0.
 */
:- public testbit/2.
:- special(testbit/2, 'SpecialBits', 0).

