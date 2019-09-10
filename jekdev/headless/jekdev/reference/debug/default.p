/**
 * We can distinguish a couple of debugging modes. In any debugging mode
 * the instrumented fork of a clause will be executed. Depending on
 * the debugging mode the instrumentation will check a certain
 * condition and when this condition is met the current execution
 * is suspended and a callback is called. The debugging modes are:
 *
 * Mode:       Condition:
 * off         Run until paused or aborted
 * step_in     Run until next port
 * step_over   Run until predicate is executed
 * step_out    Run until parent predicate is executed
 * on          Run until next spy or break point
 *
 * When the current goal is shown additional information about the
 * debugging mode, the invoca-tion depth and the current port is shown
 * as well. Last call optimization is currently defunct while debugging,
 * the invocation depth is therefore larger than usual. The information
 * is provided in the following format on the display console:
 *
 * <mode><depth><port> <goal> ?
 *
 * -: The debug mode is off
 *  : The debug mode is step in
 * =: The debug mode is step over
 * >: The debug mode is step out
 * *: The debug mode is on
 *
 * Predicates that have the sys_notrace predicate property set are
 * ignored in any debugging mode. A couple of predicates that
 * resemble commands, e.g. listing/1, trace/1, etc., have the
 * sys_notrace predicate property by default set. Predicates that
 * are invoked from within a source which has the sys_notrace source
 * property set are as well ignored in any debugging mode. The
 * system sources have the sys_notrace source property by default set.
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

:- use_package(foreign(jekdev/reference/debug)).

:- module(user, []).

:- use_module(library(inspection/provable)).
:- use_module(library(stream/console)).
:- use_module(library(system/locale)).
:- use_module(library(system/thread)).
:- use_module(library(experiment/simp)).
:- use_module(library(inspection/frame)).
:- use_module(library(inspection/store)).
:- use_module(library(system/attach)).
:- sys_load_resource(debug).

/***********************************************************************/
/* Debugging Mode                                                      */
/***********************************************************************/

/**
 * nodebug:
 * The predicate switches to the off mode.
 */
:- public nodebug/0.
nodebug :-
   set_prolog_flag(debug, off),
   thread_current(Thread),
   set_thread_flag(Thread, sys_tprompt, off).
:- set_predicate_property(nodebug/0, sys_notrace).

/**
 * trace:
 * The predicate switches to the step in mode.
 */
:- public trace/0.
trace :-
   set_prolog_flag(debug, step_in),
   thread_current(Thread),
   set_thread_flag(Thread, sys_tprompt, off).
:- set_predicate_property(trace/0, sys_notrace).

/**
 * skip:
 * The predicate switches to the step over mode.
 */
:- public skip/0.
skip :-
   set_prolog_flag(debug, step_over),
   thread_current(Thread),
   set_thread_flag(Thread, sys_tprompt, off).
:- set_predicate_property(skip/0, sys_notrace).

/**
 * up:
 * The predicate switches to the step out mode.
 */
:- public up/0.
up :-
   set_prolog_flag(debug, step_out),
   thread_current(Thread),
   set_thread_flag(Thread, sys_tprompt, off).
:- set_predicate_property(up/0, sys_notrace).

/**
 * debug:
 * The predicate switches to the on mode.
 */
:- public debug/0.
debug :-
   set_prolog_flag(debug, on),
   thread_current(Thread),
   set_thread_flag(Thread, sys_tprompt, off).
:- set_predicate_property(debug/0, sys_notrace).

/**
 * visible(L):
 * Show the ports that are listed in L, hide the ports that are not
 * listed in L. In debug mode, hidden ports are not further debugged
 * but simply continue. The following mnemonics work for the predicate.
 */
% visible(+AtomOrList)
:- public visible/1.
visible(Name) :-
   var(Name),
   throw(error(instantiation_error,_)).
visible(Name) :-
   sys_name_flags(Name, Flags), !,
   set_prolog_flag(sys_visible, Flags).
visible(Flags) :-
   set_prolog_flag(sys_visible, Flags).
:- set_predicate_property(visible/1, sys_notrace).

% sys_name_flags(+Atom, -List)
:- public sys_name_flags/2.
sys_name_flags(off, []).
sys_name_flags(loose, [call]).
sys_name_flags(half, [call,redo]).
sys_name_flags(tight, [call,redo,fail]).
sys_name_flags(full, [call,exit,redo,fail]).
sys_name_flags(all, [call,exit,redo,fail,head]).

/***********************************************************************/
/* Spy & Break Points                                                  */
/***********************************************************************/

/**
 * debugging:
 * The predicate shows the debug mode, the spy points and the break points.
 */
% debugging
:- public debugging/0.
debugging :-
   current_prolog_flag(debug, X),
   write_term((:-set_prolog_flag(debug,X)), [context(0)]),
   write('.'), nl, fail.
debugging :-
   current_prolog_flag(sys_visible, X),
   write_term((:-visible(X)), [context(0)]),
   write('.'), nl, fail.
debugging :-
   current_prolog_flag(sys_leash, X),
   write_term((:-leash(X)), [context(0)]),
   write('.'), nl, fail.
debugging :-
   spying(X),
   write_term((:-spy(X)), [context(0)]),
   write('.'), nl, fail.
debugging :-
   breaking(X, Y),
   write_term((:-break(X,Y)), [context(0)]),
   write('.'), nl, fail.
debugging.
:- set_predicate_property(debugging/0, sys_notrace).

/**
 * spy(P):
 * The predicate adds the predicate P to the spy points.
 */
% spy(+Indicator)
:- public spy/1.
:- special(spy/1, 'SpecialDefault', 0).
:- set_predicate_property(spy/1, sys_notrace).

/**
 * nospy(P):
 * The predicate removes the predicate P from the spy points.
 */
% nospy(+Indicator)
:- public nospy/1.
:- special(nospy/1, 'SpecialDefault', 1).
:- set_predicate_property(nospy/1, sys_notrace).

/**
 * spying(P):
 * The predicate succeeds in P for every spy point.
 */
% spying(-Indicator)
:- public spying/1.
spying(I) :-
   sys_spying(L),
   sys_member(I, L).

% sys_spying(-List)
:- private sys_spying/1.
:- special(sys_spying/1, 'SpecialDefault', 2).

/**
 * break(F, L):
 * The predicate adds the file F and the line number L to the break points.
 */
% break(+Callable, +Integer)
:- public break/2.
break(P, L) :-
   absolute_file_name(P, Q),
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_store, Store),
   set_store_property(Store, sys_break(Q,L)),
   change_store(Store).
:- set_predicate_property(break/2, sys_notrace).

/**
 * nobreak(F, L):
 * The predicate removes the file F and the line number L from the break points.
 */
% nobreak(+Callable, +Integer)
:- public nobreak/2.
nobreak(P, L) :-
   absolute_file_name(P, Q),
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_store, Store),
   reset_store_property(Store, sys_break(Q,L)),
   change_store(Store).
:- set_predicate_property(nobreak/2, sys_notrace).

/**
 * breaking(F, L):
 * For every break point the predicate succeeds
 * with the file F and the line number L.
 */
% breaking(-Callable, -Integer)
:- public breaking/2.
breaking(P, L) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_store, Store),
   store_property(Store, sys_break(Q,L)),
   absolute_file_name(P, Q).

/***********************************************************************/
/* Debugging Prompt                                                    */
/***********************************************************************/

/**
 * sys_debug(P, F):
 * The predicate displays the port P and the goal of frame F,
 * and then continues debugging.
 */
% sys_debug(+Atom, +Frame)
sys_debug(Port, Frame) :-
   sys_goal_show(Port, Frame), nl.

/**
 * sys_debug_ask(P, F):
 * The predicate displays the port P and the goal of frame F,
 * prompts the user for actions, and then continues debugging.
 */
% sys_debug_ask(+Atom, +Frame)
sys_debug_ask(Port, Frame) :- repeat,
   sys_trap(sys_debug_prompt(Port, Frame), E,
      (  sys_error_type(E, system_error(_))
      -> sys_raise(E)
      ;  sys_error_message(E), fail)), !.

% sys_debug_prompt(+Atom, +Frame)
:- private sys_debug_prompt/2.
sys_debug_prompt(Port, Frame) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_tprompt, Prompt),
   setup_call_cleanup(
      set_thread_flag(Thread, sys_tprompt, ask_debug),
      sys_debug_show(Port, Frame),
      set_thread_flag(Thread, sys_tprompt, Prompt)).

% sys_debug_prompt(+Atom, +Frame)
:- private sys_debug_prompt/2.
sys_debug_show(Port, Frame) :-
   sys_goal_show(Port, Frame),
   write(' ? '), flush_output,
   (  read_line(L) -> true; exit),
   thread_current(Thread),
   (  L == ''
   -> set_thread_flag(Thread, sys_tprompt, off)
   ;  L == ? -> sys_debug_help
   ;  term_atom(G, L, [terminator(period)]),
      once(sys_ignore(G))),
   current_thread_flag(Thread, sys_tprompt, Response),
   Response \== ask_debug.

% sys_debug_help
:- private sys_debug_help/0.
sys_debug_help :-
   get_properties(debug, P),
   get_property(P, 'debug.help', V),
   write(V), nl.

/****************************************************************/
/* Goal Display                                                 */
/****************************************************************/

% sys_goal_show(+Atom, +Frame)
:- private sys_goal_show/2.
sys_goal_show(Port, Frame) :-
   get_properties(debug, P),
   sys_continue_mode(Mode),
   atom_concat('debug.', Mode, Key1),
   get_property(P, Key1, V1),
   sys_goal_depth(Frame, -1, D),
   atom_concat('debug.', Port, Key2),
   get_property(P, Key2, V2),
   frame_property(Frame, sys_call_goal(Goal)),
   rebuild_goal(Goal, Rebuild),
   current_prolog_flag(sys_print_map, N),
   write(V1),
   write(' '),
   write(D),
   write(' '),
   write(V2),
   write(' '),
   write_term(Rebuild, [context(0),quoted(true),variable_names(N)]).

% sys_goal_depth(+Frame, +Integer, -Integer)
:- private sys_goal_depth/3.
sys_goal_depth(null, N, N) :- !.
sys_goal_depth(Frame, N, M) :-
   H is N+1,
   frame_property(Frame, sys_parent_frame(Other)),
   sys_goal_depth(Other, H, M).

/****************************************************************/
/* Continue Debug                                               */
/****************************************************************/

% sys_continue_debug(+Atom, +Frame)
sys_continue_debug(Port, Frame) :-
   sys_continue_mode(Mode),
   sys_continue_setup(Mode, Port, Frame).

% sys_continue_setup(+Atom, +Atom, +Frame)
:- private sys_continue_setup/3.
sys_continue_setup(off, _, _).
sys_continue_setup(step_in, _, _).
sys_continue_setup(step_over, call, Frame) :- !,
   thread_current(Thread),
   set_thread_flag(Thread, sys_tskip_frame, Frame).
sys_continue_setup(step_over, redo, Frame) :- !,
   thread_current(Thread),
   set_thread_flag(Thread, sys_tskip_frame, Frame).
sys_continue_setup(step_over, _, _).
sys_continue_setup(step_out, _, Frame) :-
   frame_property(Frame, sys_parent_frame(Other)),
   thread_current(Thread),
   set_thread_flag(Thread, sys_tskip_frame, Other).
sys_continue_setup(on, _, _).

% sys_continue_mode(-Atom)
:- private sys_continue_mode/1.
sys_continue_mode(M) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_tdebug, M),
   M \== inherit, !.
sys_continue_mode(M) :-
   current_prolog_flag(debug, M).

/******************************************************************/
/* Low-Level Checks                                               */
/******************************************************************/

/**
 * sys_notrace_frame(F):
 * The predicate succeeds if the goal of frame F is sys_notrace.
 */
% sys_notrace_frame(+Frame)
:- public sys_notrace_frame/1.
:- special(sys_notrace_frame/1, 'SpecialDefault', 3).

/**
 * sys_leashed_port(P):
 * The predicate succeeds when the port P is among the leashed ports.
 */
:- public sys_leashed_port/1.
:- special(sys_leashed_port/1, 'SpecialDefault', 4).
