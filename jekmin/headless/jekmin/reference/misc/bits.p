/**
 * We provide a couple of additional bitwise operations. The evaluable
 * functions bitcount/1, bitlength/1 and lowestsetbit/1 deal with the
 * determination of certain bits of the given integer. The implementation
 * is more efficient than would be possible with existing logical,
 * shift and test operations.
 *
 * Examples:
 * bitlength(333)               --> 9
 *
 * The evaluable functions setbit/2 and clearbit/2 update the given
 * integer in a more efficient way than would be possible with existing
 * logical and shift operations. The predicate testbit/2 tests a
 * particular bit in a given integer, again the implementation is more
 * efficient than would be possible with existing logical, shift and
 * test operations.
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
 * bitcount(X, N):
 * Predicate succeeds in N with the number of non-zero bits of X.
 */
:- public bitcount/2.
:- special(bitcount/2, 'SupplementBits', 0).

/**
 * bitlength(X, N):
 * Predicate succeeds in N with the highest non-zero bit of X.
 */
:- public bitlength/2.
:- special(bitlength/2, 'SupplementBits', 1).

/**
 * lowestsetbit(X, N):
 * Predicate succeeds in N with the lowest non-zero bit of X.
 */
:- public lowestsetbit/2.
:- special(lowestsetbit/2, 'SupplementBits', 2).

/**
 * setbit(X, Y, Z):
 * The predicate succeeds in Z with Y \/ (1 << X).
 */
:- public setbit/3.
:- special(setbit/3, 'SupplementBits', 3).

/**
 * clearbit(X, Y, Z):
 * The predicate succeeds in Z with Y /\ \ (1 << X).
 */
:- public clearbit/3.
:- special(clearbit/3, 'SupplementBits', 4).

/**
 * testbit(X, Y):
 * The predicate succeeds when Y /\ (1 << X) =\= 0.
 */
:- public testbit/2.
:- special(testbit/2, 'SpecialBits', 0).

