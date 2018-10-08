/**
 * This module will allow accessing Prolog interpreters during
 * debugging. It does complement the other inspection modules in
 * that it supports the thread view of a debugged Prolog interpreter.
 * For this purpose we need a stop the world event, because it is
 * unsafe to access the stack of a Prolog interpreter while it runs.
 *
 * Further, in case that variables of the stack are accessed, we
 * need ways to serialize and deserialize the results, since mixing
 * non-ground Prolog terms from different Prolog interpreters leads
 * to unexpected results, when the stop the world event is over.
 * For the current thread stop the world will be not needed.
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

:- package(library(jekdev/reference/inspection)).
:- use_package(foreign(jekdev/reference/inspection)).
:- use_package(foreign(jekpro/tools/call)).

:- module(fence, []).
