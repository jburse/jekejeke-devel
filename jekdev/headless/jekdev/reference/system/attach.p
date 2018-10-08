/**
 * Trace level spy & break points.
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

:- package(library(jekdev/reference/system)).
:- use_package(foreign(jekdev/reference/system)).

:- module(attach, []).

/***********************************************************************/
/* Spy & Break Points                                                  */
/***********************************************************************/

/**
 * tdebugging:
 * The predicate shows the engine spy points and the engine break points.
 */
% tdebugging
:- public tdebugging/0.
:- special(tdebugging/0, 'SpecialAttach', 0).
:- set_predicate_property(tdebugging/0, sys_notrace).

/**
 * tspy(P):
 * The predicate adds the predicates P to the engine spy points.
 */
% tspy(+Indicator)
:- public tspy/1.
:- special(tspy/1, 'SpecialAttach', 1).

/**
 * tnospy(P):
 * The predicate removes the predicates P from the engine spy points.
 */
% nospy(+Indicator)
:- public tnospy/1.
:- special(tnospy/1, 'SpecialAttach', 2).

/**
 * tbreak(F, L):
 * The predicate adds the file F and the line number L to the engine break points.
 */
% tbreak(+Atom, +Integer)
:- public tbreak/2.
tbreak(P, L) :-
   absolute_file_name(P, Q),
   sys_tbreak(Q, L).

% sys_tbreak(+Pin, +Integer)
:- private sys_tbreak/2.
:- special(sys_tbreak/2, 'SpecialAttach', 3).

/**
 * tnobreak(F, L):
 * The predicate removes the file F and the line number L from the engine break points.
 */
% tnobreak(+Atom, +Integer)
:- public tnobreak/2.
tnobreak(P, L) :-
   absolute_file_name(P, Q),
   sys_tnobreak(Q, L).

% sys_tnobreak(+Pin, +Integer)
:- private sys_tnobreak/2.
:- special(sys_tnobreak/2, 'SpecialAttach', 4).
