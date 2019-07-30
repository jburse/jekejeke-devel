/**
 * The query answer loop of the Prolog interpreter repeatedly prompts a
 * query and answers it by showing the variable bindings. The query
 * answer loop can be started by the predicate prolog/0 and entered
 * recursively by the predicate break/0. The query answer loop can be
 * terminated by issuing an end of file.
 *
 * Examples:
 * ?- break, p(X).
 * [1] ?- assertz(p(a)).
 * Yes
 * [1] ?-
 * X = a
 *
 * The system predicates abort/1, exit/1 and close/1 throw some well-known
 * system errors. The system predicate exit/1 allows terminating the query
 * answer loop similarly like issuing an end of file. The system predicate
 * abort/1 only terminates the current query but continues the loop. The
 * system predicate close/1 recursively terminates all query answering
 * loops.
 *
 * The system predicate version/0 displays a version banner. Top-level
 * answers are displayed with the operator (=)/2. For custom forms
 * delivered by a printable hook the operator (is)/2 is displayed. For
 * custom constraints delivered by an equation hook the corresponding
 * operator is displayed. For printable and equation hooks see the
 * module residue.
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

:- use_package(foreign(jekpro/reference/runtime)).

:- module(user, []).
:- use_module(library(system/locale)).
:- use_module(library(stream/console)).
:- use_module(library(misc/residue)).
:- use_module(library(system/thread)).
:- sys_load_resource(runtime).

/*************************************************************************/
/* User Session                                                          */
/*************************************************************************/

/**
 * abort:
 * The predicate throws a system error of type user abort.
 */
% abort
:- public abort/0.
abort :-
   throw(error(system_error(user_abort),_)).
:- set_predicate_property(abort/0, sys_notrace).

/**
 * exit:
 * The predicate throws a system error of type user exit.
 */
% exit
:- public exit/0.
exit :-
   throw(error(system_error(user_exit),_)).
:- set_predicate_property(exit/0, sys_notrace).

/**
 * close:
 * The predicate throws a system error of type user close.
 */
% close
:- public close/0.
close :-
   throw(error(system_error(user_close),_)).
:- set_predicate_property(close/0, sys_notrace).

/**
 * prolog:
 * break:
 * The predicate prompts and answers queries until an
 * end of file is encountered.
 */
% prolog
:- public prolog/0.
prolog :-
   call_cleanup(
      sys_toplevel,
      end_all_modules).
:- set_predicate_property(prolog/0, sys_notrace).

% break
:- public break/0.
break :-
   current_prolog_flag(sys_break_level, X),
   Y is X+1,
   setup_call_cleanup(
      set_prolog_flag(sys_break_level, Y),
      sys_toplevel,
      set_prolog_flag(sys_break_level, X)).
:- set_predicate_property(break/0, sys_notrace).

% sys_toplevel
:- private sys_toplevel/0.
sys_toplevel :- repeat,
   sys_trap(sys_toplevel_ask, E,
      (  sys_error_type(E, system_error(user_abort))
      -> sys_error_cause(E), fail
      ;  sys_error_type(E, system_error(user_exit))
      -> sys_error_cause(E)
      ;  sys_error_type(E, system_error(_))
      -> sys_raise(E)
      ;  sys_error_stack(E), fail)), !.
:- set_predicate_property(sys_toplevel/0, sys_notrace).

% sys_toplevel_ask
:- private sys_toplevel_ask/0.
sys_toplevel_ask :- sys_toplevel_level, sys_toplevel_top,
   write('?- '), flush_output,
   read_term(G, [variable_names(N)]),
   (  G == end_of_file -> true
   ;  current_prolog_flag(sys_print_map, M),
      setup_call_cleanup(set_prolog_flag(sys_print_map, N),
         sys_answer(G, N),
         set_prolog_flag(sys_print_map, M)), fail).

% sys_toplevel_level
:- private sys_toplevel_level/0.
sys_toplevel_level :-
   current_prolog_flag(sys_break_level, X),
   X > 0, !,
   write('['),
   write(X),
   write('] ').
sys_toplevel_level.

% sys_toplevel_top
:- private sys_toplevel_top/0.
sys_toplevel_top :-
   top_module(N), !,
   write('('),
   writeq(N),
   write(') ').
sys_toplevel_top.

/****************************************************************/
/* Answer Display                                               */
/****************************************************************/

% sys_answer(+Goal, +Assoc)
:- private sys_answer/2.
sys_answer(G, N) :-
   current_prolog_flag(sys_choices, X),
   expand_goal(G, H),
   call_residue(H, R),
   current_prolog_flag(sys_choices, Y),
   (  X =:= Y -> !,
      sys_filter_show(N, R), nl
   ;  sys_answer_ask(N, R) -> !; true).
sys_answer(_, _) :-
   get_properties(runtime, P),
   get_property(P, 'query.no', V),
   write(V), nl.

% sys_answer_ask(+Assoc, +List)
:- private sys_answer_ask/2.
sys_answer_ask(N, R) :- repeat,
   sys_trap(sys_answer_prompt(N, R, Response), E,
      (  sys_error_type(E, system_error(_))
      -> sys_raise(E)
      ;  sys_error_message(E), fail)), !,
   Response == answer_cut.

% sys_answer_prompt(+Assoc, +List, -Atom)
:- private sys_answer_prompt/3.
sys_answer_prompt(N, R, Response) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_tprompt, Prompt),
   setup_call_cleanup(
      set_thread_flag(Thread, sys_tprompt, on),
      sys_answer_show(N, R, Response),
      set_thread_flag(Thread, sys_tprompt, Prompt)).

% sys_answer_show(+Assoc, +List, -Atom)
:- private sys_answer_show/3.
sys_answer_show(N, R, Response) :-
   sys_filter_show(N, R),
   write(' '), flush_output,
   (  read_line(L) -> true; exit),
   thread_current(Thread),
   (  L == ;
   -> set_thread_flag(Thread, sys_tprompt, off)
   ;  L == ''
   -> set_thread_flag(Thread, sys_tprompt, answer_cut)
   ;  L == ? -> sys_answer_help
   ;  term_atom(G, L, [terminator(period)]),
      once(sys_ignore(G))),
   current_thread_flag(Thread, sys_tprompt, Response),
   Response \== on.

% sys_answer_help
:- private sys_answer_help/0.
sys_answer_help :-
   get_properties(runtime, P),
   get_property(P, 'query.help', V),
   write(V), nl.

/****************************************************************/
/* Error Display                                                */
/****************************************************************/

% sys_error_cause(+Term)
:- private sys_error_cause/1.
sys_error_cause(cause(_,R)) :- !,
   sys_error_stack(R).
sys_error_cause(_).

% sys_error_stack(+Term)
:- private sys_error_stack/1.
sys_error_stack(E) :-
   current_error(T),
   print_stack_trace(T, E).

% sys_error_message(+Term)
:- public sys_error_message/1.
sys_error_message(E) :-
   current_error(T),
   error_make(E, S),
   write(T, S),
   nl(T).

/****************************************************************/
/* Filter & Show Variables                                      */
/****************************************************************/

% sys_filter_show(+Assoc, +List)
:- private sys_filter_show/2.
sys_filter_show(N, R) :-
   sys_filter_assoc(N, N, R, M),
   sys_show_assoc(M, N).

% sys_filter_assoc(+Assoc, +Assoc, +List, -Assoc)
:- private sys_filter_assoc/4.
sys_filter_assoc([X=Y|L], N, K, R) :-
   var(Y),
   sys_get_assoc(Y, N, Z),
   Z == X, !,
   sys_filter_assoc(L, N, K, R).
sys_filter_assoc([X=Y|L], N, K, [X=Y|R]) :-
   sys_filter_assoc(L, N, K, R).
sys_filter_assoc([], _, K, K).

% sys_get_assoc(+Var, +Assoc, -Atom)
:- private sys_get_assoc/3.
sys_get_assoc(Y, [_|L], Z) :-
   sys_get_assoc(Y, L, Z), !.
sys_get_assoc(Y, [Z=T|_], Z) :-
   T == Y.

% sys_show_assoc(+Assoc, +Assoc)
:- private sys_show_assoc/2.
sys_show_assoc([], _) :-
   get_properties(runtime, P),
   get_property(P, 'query.yes', V),
   write(V).
sys_show_assoc([X,Y|Z], N) :- !,
   sys_show_pair(X, N),
   write(','), nl,
   sys_show_assoc([Y|Z], N).
sys_show_assoc([X], N) :-
   sys_show_pair(X, N).

% sys_show_pair(+Pair, +Assoc)
:- private sys_show_pair/2.
sys_show_pair(X=T, N) :-
   sys_printable_value(T, S), !,
   sys_quoted_var(X, Q),
   write(Q),
   write(' is '),
   sys_show_term(S, [priority(699),quoted(true),variable_names(N)]).
sys_show_pair(X=T, N) :- !,
   sys_quoted_var(X, Q),
   write(Q),
   write(' = '),
   sys_show_term(T, [priority(699),quoted(true),variable_names(N)]).
sys_show_pair(G, N) :-
   sys_show_term(G, [context(0),quoted(true),variable_names(N)]).

% sys_show_term(+Term, +List)
:- private sys_show_term/2.
sys_show_term(T, L) :-
   acyclic_term(T), !,
   write_term(T, L).
sys_show_term(_, _) :-
   get_properties(runtime, P),
   get_property(P, 'query.cyclic', V),
   write(V).

/**
 * sys_quoted_var(V, Q):
 * The predicate succeeds in Q with a possibly quoted variable name V.
 */
% sys_quoted_var(+Atom, -Atom)
:- public sys_quoted_var/2.
:- special(sys_quoted_var/2, 'SpecialSession', 0).
