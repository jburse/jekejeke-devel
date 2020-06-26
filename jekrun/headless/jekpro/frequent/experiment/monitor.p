/**
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

:- package(library(jekpro/frequent/experiment)).
:- use_package(foreign(jekpro/frequent/experiment)).

:- module(monitor, []).

/**
 * new(O):
 * The predicate succeeds in O with a new object.
 */
% new(-Object)
:- public new/1.
:- special(new/1, 'SpecialMonitor', 0).

/**
 * synchronized(O, G):
 * The predicate succeeds whenever the goal G succeeds. The
 * first, next and cut are synchronized on the object O.
 */
% synchronized(+Object, +Goal)
:- public synchronized/2.
:- meta_predicate synchronized(?, 0).
:- special(synchronized/2, 'SpecialMonitor', 1).

/**
 * wait(O):
 * The predicate waits on the object O.
 */
% wait(+Object)
:- public wait/1.
:- special(wait/1, 'SpecialMonitor', 2).

/**
 * notify(O):
 * The predicate notifies the object O.
 */
% notify(+Object)
:- public notify/1.
:- special(notify/1, 'SpecialMonitor', 3).
