/**
 * The programming interface allows mode changes from a secondary
 * thread. The secondary thread acts as a controller of the primary
 * thread. By changing the mode it controls the behav-iour of the
 * primary thread. A typical example of a secondary thread is the
 * user interface event thread. The primary threads are then the
 * console window Prolog threads. The secondary thread can then
 * for example start tracing a console window Prolog thread by
 * changing into an appropriate mode.
 *
 * The normal flow allows that mode changes can happen at any
 * time. Their effect is also not delimited to the call port of
 * a predicate or to an interrupted blocking operation. This is
 * not very problematic except when inside a trace handler. A mode
 * change might cause an undesired invocation of the trace handler.
 * The system predicate sys_ignore/1 allows calling a goal with
 * the mode cloak mask temporarily set off.
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

:- use_package(foreign(jekdev/reference/system)).

:- module(user, []).

/**
 * sys_ignore(A):
 * The predicate succeeds whenever A succeeds. The goal A is invoked
 * with the mode cloak temporarily set to off.
 */
% sys_ignore(+Goal)
:- public sys_ignore/1.
:- meta_predicate sys_ignore(0).
:- special(sys_ignore/1, 'SpecialMode', 0).
:- set_predicate_property(sys_ignore/1, sys_notrace).

% The instrumentation hooks

/******************************************************************/
/* sys_in/0                                                       */
/*   call: 0                                                      */
/*   fail: 1                                                      */
/******************************************************************/

:- public sys_in/0.
:- static sys_in/0.
:- set_predicate_property(sys_in/0, sys_noinstrument).
:- set_predicate_property(sys_in/0, sys_nowakeup).
:- set_predicate_property(sys_in/0, sys_nostack).
sys_in :-
   sys_notrace_chk(0), !.
sys_in :-
   sys_port_show(0), sys_in2.

:- private sys_in2/0.
:- static sys_in2/0.
:- set_predicate_property(sys_in2/0, sys_noinstrument).
:- set_predicate_property(sys_in2/0, sys_nowakeup).
:- set_predicate_property(sys_in2/0, sys_nostack).
sys_in2.
sys_in2 :-
   sys_port_show(1), fail.

/******************************************************************/
/* sys_out/0                                                      */
/*   exit: 2                                                      */
/*   redo: 3                                                      */
/******************************************************************/

:- public sys_out/0.
:- static sys_out/0.
:- set_predicate_property(sys_out/0, sys_noinstrument).
:- set_predicate_property(sys_out/0, sys_nowakeup).
:- set_predicate_property(sys_out/0, sys_nostack).
sys_out :-
   sys_notrace_chk(2), !.
sys_out :-
   sys_port_show(2), sys_out2.

:- private sys_out2/0.
:- static sys_out2/0.
:- set_predicate_property(sys_out2/0, sys_noinstrument).
:- set_predicate_property(sys_out2/0, sys_nowakeup).
:- set_predicate_property(sys_out2/0, sys_nostack).
sys_out2.
sys_out2 :-
   sys_port_show(3), fail.

/******************************************************************/
/* sys_at/0                                                       */
/*   head: 4                                                      */
/*   chop: 5                                                      */
/******************************************************************/

:- public sys_at/0.
:- static sys_at/0.
:- set_predicate_property(sys_at/0, sys_noinstrument).
:- set_predicate_property(sys_at/0, sys_nowakeup).
:- set_predicate_property(sys_at/0, sys_nostack).
sys_at :-
   sys_notrace_chk(4), !.
sys_at :-
   sys_port_show(4), sys_at2.

:- private sys_at2/0.
:- static sys_at2/0.
:- set_predicate_property(sys_at2/0, sys_noinstrument).
:- set_predicate_property(sys_at2/0, sys_nowakeup).
:- set_predicate_property(sys_at2/0, sys_nostack).
sys_at2.
sys_at2 :-
   sys_port_show(5), fail.

:- private sys_notrace_chk/1.
:- special(sys_notrace_chk/1, 'SpecialMode', 1).

:- private sys_deter_chk/1.
:- special(sys_deter_chk/1, 'SpecialMode', 2).

:- private sys_port_show/1.
:- special(sys_port_show/1, 'SpecialMode', 3).
