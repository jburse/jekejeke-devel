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
 * loops. The system predicate version/0 displays a version banner.
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
 * welcome:
 * version:
 * The predicate displays a version banner.
 */
% welcome
:- public welcome/0.
welcome :- version.
:- set_predicate_property(welcome/0, sys_notrace).

:- public version/0.
:- special(version/0, 'SpecialSession', 0).
:- set_predicate_property(version/0, sys_notrace).

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
/* Display Answer Constraints                              */
/***********************************************************/

/**
 * sys_show_vars:
 * Will show all constraints related to the current query.
 * Called by the interpreter for each successful query.
 */
% sys_show_vars
:- public sys_show_vars/0.
sys_show_vars :-
   sys_get_variable_names(N),
   sys_term_eq_list(N, L),
   sys_filter_variable_names(N, L, R),
   sys_show_name_or_eq_list(R).
:- set_predicate_property(sys_show_vars/0, sys_notrace).

/**
 * sys_filter_variable_names(L, R, S):
 * Succeeds with the list S which contains R and those variable
 * names of L that are not trivial.
 */
% sys_filter_variable_names(+List, +List, -List)
:- private sys_filter_variable_names/3.
sys_filter_variable_names([X=Y|L], R, S) :-
   var(Y),
   sys_get_variable_names(N),
   once((  sys_member(Z=T, N),
           T == Y)),
   Z == X, !,
   sys_filter_variable_names(L, R, S).
sys_filter_variable_names([E|L], R, [E|S]) :-
   sys_filter_variable_names(L, R, S).
sys_filter_variable_names([], L, L).

/**
 * sys_show_name_or_eq_list(L):
 * Shows the variable assignments and constraints from L on the tty.
 */
% sys_show_name_or_eq_list(+List)
:- private sys_show_name_or_eq_list/1.
sys_show_name_or_eq_list([]) :-
   sys_get_lang(runtime, P),
   get_property(P, 'query.yes', V),
   ttywrite(V).
sys_show_name_or_eq_list([X,Y|Z]) :- !,
   sys_show_name_or_eq(X),
   ttywrite(','), ttynl,
   sys_show_name_or_eq_list([Y|Z]).
sys_show_name_or_eq_list([X]) :-
   sys_show_name_or_eq(X).

/**
 * sys_show_name_or_eq(E):
 * Shows the variable assignment or constraint E on the tty.
 */
% sys_show_name_or_eq(+Eq)
:- private sys_show_name_or_eq/1.
:- meta_predicate sys_show_name_or_eq(0).
sys_show_name_or_eq(X = T) :- !,
   sys_write_var(X),
   ttywrite(' = '),
   sys_get_variable_names(N),
   ttywrite_term(T, [quoted(true),priority(699),variable_names(N)]).
sys_show_name_or_eq(T) :-
   sys_get_variable_names(N),
   ttywrite_term(T, [quoted(true),context(0),variable_names(N)]).

/**
 * sys_write_var(V):
 * Write the variable name with appropriate quoting.
 */
% sys_write_var(+Atom)
:- private sys_write_var/1.
:- special(sys_write_var/1, 'SpecialSession', 4).