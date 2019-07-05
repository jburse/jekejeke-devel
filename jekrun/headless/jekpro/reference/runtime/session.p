/**
 * The query answer loop of the Prolog interpreter repeatedly prompts
 * a query and answers it by showing the variable bindings. The query
 * answer loop can be entered recursively by the predicate break/0.
 * The query answer loop can be terminated by issuing an end of file.
 * The query answer loop runs in its own input/output stream pair.
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
prolog :- break.
:- set_predicate_property(prolog/0, sys_notrace).

:- public break/0.
:- special(break/0, 'SpecialSession', 1).
:- set_predicate_property(break/0, sys_notrace).

/***********************************************************/
/* Display Variable Instantiations & Constraints           */
/***********************************************************/

/**
 * sys_show_vars:
 * Will show all variable instantiations and constraints related to
 * the current query or directive. Called by the interpreter for
 * each successful query.
 */
% sys_show_vars
:- public sys_show_vars/0.
sys_show_vars :-
   sys_get_variable_names(M),
   sys_get_name_or_eq_list(R, M),
   sys_show_name_or_eq_list(R, M).
:- set_predicate_property(sys_show_vars/0, sys_notrace).

/**
 * sys_get_name_or_eq_list(R, M):
 * Will retrieve all variable instantiations and constraints related
 * to the current query or directive. Called by the interpreter for
 * notebook queries, where M are the variable names.
 */
% sys_get_name_or_eq_list(-List, +List)
:- public sys_get_name_or_eq_list/2.
sys_get_name_or_eq_list(R, M) :-
   sys_get_raw_variables(N),
   sys_eq_list(L),
   sys_filter_variable_names(N, M, L, R).

/**
 * sys_filter_variable_names(L, M, R, S):
 * Succeeds with the list S which contains R and those variable
 * names of L that are not trivial, where M are the variable names.
 */
% sys_filter_variable_names(+List, +List, +List, -List)
:- private sys_filter_variable_names/4.
sys_filter_variable_names([X=Y|L], M, R, S) :-
   var(Y),
   once((  sys_member(Z=T, M),
           T == Y)),
   Z == X, !,
   sys_filter_variable_names(L, M, R, S).
sys_filter_variable_names([X=Y|L], M, R, [X is Z|S]) :-
   sys_printable_value(Y, Z), !,
   sys_filter_variable_names(L, M, R, S).
sys_filter_variable_names([E|L], M, R, [E|S]) :-
   sys_filter_variable_names(L, M, R, S).
sys_filter_variable_names([], _, L, L).

/**
 * sys_show_name_or_eq_list(L, M):
 * Shows the variable assignments and constraints from L on the tty,
 * where M are the variable names.
 */
% sys_show_name_or_eq_list(+List, +List)
:- public sys_show_name_or_eq_list/2.
sys_show_name_or_eq_list([], _) :-
   sys_get_lang(runtime, P),
   get_property(P, 'query.yes', V),
   write(V).
sys_show_name_or_eq_list([X,Y|Z], M) :- !,
   sys_show_name_or_eq(X, M),
   write(','), nl,
   sys_show_name_or_eq_list([Y|Z], M).
sys_show_name_or_eq_list([X], M) :-
   sys_show_name_or_eq(X, M).

/**
 * sys_show_no:
 * Shows a no on the tty.
 */
% sys_show_no
:- public sys_show_no/0.
sys_show_no :-
   sys_get_lang(runtime, P),
   get_property(P, 'query.no', V),
   write(V).

/**
 * sys_show_name_or_eq(E, M):
 * Shows the variable assignment or constraint E on the tty,
 * where M are the variable names.
 */
% sys_show_name_or_eq(+Eq, +List)
:- private sys_show_name_or_eq/2.
:- meta_predicate sys_show_name_or_eq(0,?).
sys_show_name_or_eq(X is T, M) :- !,
   sys_quoted_var(X, Q),
   write(Q),
   write(' is '),
   sys_show_value(T, M).
sys_show_name_or_eq(X = T, M) :- !,
   sys_quoted_var(X, Q),
   write(Q),
   write(' = '),
   sys_show_value(T, M).
sys_show_name_or_eq(T, M) :-
   acyclic_term(T), !,
   write_term(T, [context(0),quoted(true),variable_names(M)]).
sys_show_name_or_eq(_, _) :-
   write('<cyclic term>').

:- private sys_show_value/2.
sys_show_value(T, M) :-
   acyclic_term(T), !,
   write_term(T, [priority(699),quoted(true),variable_names(M)]).
sys_show_value(_, _) :-
   write('<cyclic term>').

/**
 * sys_quoted_var(V, Q):
 * The predicate succeeds in Q with a possibly quoted variable name V.
 */
% sys_quoted_var(+Atom, -Atom)
:- public sys_quoted_var/2.
:- special(sys_quoted_var/2, 'SpecialSession', 2).

/**
 * sys_get_raw_variables(L):
 * The predicate succeeds in L the current variable names
 * from the top-level query including non-variables or duplicates.
 */
% sys_get_raw_variables(-VariableNames)
:- public sys_get_raw_variables/1.
:- special(sys_get_raw_variables/1, 'SpecialSession', 3).
