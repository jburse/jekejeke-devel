/**
 * We provide a couple of additional bitwise operations. The evaluable
 * functions bitcount/1, bitlength/1 and lowestsetbit/1 deal with the
 * determination of certain bits of the given integer. The evaluable
 * functions setbit/2 and clearbit/2 update the given integer in a
 * more efficient way than would be possible with existing logical
 * and shift operations.
 *
 * The predicate sys_test_bit/2 tests a particular bit in a given
 * integer, again the implementation is more efficient than would
 * be possible with existing logical, shift and test operations.
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

:- package(library(jekmin/reference/mics)).
:- use_package(foreign(jekmin/reference/misc)).

:- module(bits, []).

/**
 * bitcount(X):
 * If X is an integer returns the number of non-zero bits.
 */
:- public bitcount/2.
:- special(bitcount/2, 'SupplementBits', 0).

/**
 * bitlength(X):
 * If X is an integer returns the index of the highest non-zero bit.
 */
:- public bitlength/2.
:- special(bitlength/2, 'SupplementBits', 1).

/**
 * lowestsetbit(X):
 * If X is an integer returns the index of the lowest non-zero bit.
 */
:- public lowestsetbit/2.
:- special(lowestsetbit/2, 'SupplementBits', 2).

/**
 * setbit(X, Y):
 * If X and Y are integers returns Y with the bit at index X set.
 */
:- public setbit/3.
:- special(setbit/3, 'SupplementBits', 3).

/**
 * clearbit(X,Y):
 * If X and Y are integers returns Y with the bit at index X cleared.
 */
:- public clearbit/3.
:- special(clearbit/3, 'SupplementBits', 4).

/**
 * sys_test_bit(X, Y):
 * Succeeds when Y has the bit at index X set, otherwise fails.
 */
:- public sys_test_bit/2.
:- special(sys_test_bit/2, 'SpecialBits', 0).
