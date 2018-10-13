/**
 * This module provide debugger attachment for a Prolog engine. The
 * ordinary spy points and break points from the default debugger
 * are stored on the knowledge base level. The spy points and break
 * points provided by this module are stored in the current engine.
 *
 * The predicate tdebugging/0 allows listing all the Prolog engine
 * locale spy points and break points. The predicates tspy/1 and tbreak/2
 * allow adding spy points respectively break points. The predicates
 * tnospy/1 and tnobreak/2 allow removing them.
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
:- use_module(library(stream/console)).

/***********************************************************************/
/* Spy & Break Points                                                  */
/***********************************************************************/

/**
 * tdebugging:
 * The predicate shows the engine spy points and the engine break points.
 */
% tdebugging
:- public tdebugging/0.
tdebugging :-
   tspying(X),
   ttywrite_term((:-tspy(X)), [quoted(true),context(0)]),
   ttywrite('.'), ttynl, fail.
tdebugging :-
   tbreaking(X, Y),
   ttywrite_term((:-tbreak(X,Y)), [quoted(true),context(0)]),
   ttywrite('.'), ttynl, fail.
tdebugging.

/**
 * tspy(P):
 * The predicate adds the predicate P to the engine spy points.
 */
% tspy(+Indicator)
:- public tspy/1.
:- special(tspy/1, 'SpecialAttach', 0).

/**
 * tnospy(P):
 * The predicate removes the predicates P from the engine spy points.
 */
% nospy(+Indicator)
:- public tnospy/1.
:- special(tnospy/1, 'SpecialAttach', 1).

/**
 * tspying(P):
 * The predicate succeeds in P for every engine spy point.
 */
% tspying(-Indicator)
:- public tspying/1.
tspying(I) :-
   sys_tspying(L),
   sys_member(I, L).

% sys_tspying(-List)
:- private sys_tspying/1.
:- special(sys_tspying/1, 'SpecialAttach', 2).

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

/**
 * tbreaking(F, L):
 * For every engine break point the predicate succeeds
 * with the file F and the line number L.
 */
% tbreaking(-Callable, -Integer)
:- public tbreaking/2.
tbreaking(P, L) :-
   sys_tbreaking(R),
   sys_member(Q-L, R),
   absolute_file_name(P, Q).

% sys_tbreaking(+List)
:- private sys_tbreaking/1.
:- special(sys_tbreaking/1, 'SpecialAttach', 5).
