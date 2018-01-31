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
 * The predicates begin_module/1 and end_module/0 can be used from
 * the interpreter top-level loop or inside a consulted file. In both
 * cases the predicates will open respectively close a local module.
 * For a consulted file the predicate begin_module/1 will also do first
 * a clear of the local module, and the predicate end_module/0 will do
 * a style check of the local module.
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
:- use_module(library(misc/text)).
:- use_module(library(bootload/toolkit)).
:- use_module(library(basic/lists)).
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
   sys_get_raw_variables(N),
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
   sys_once((sys_member(Z=T,N),T==Y)),
   Z == X, !,
   sys_filter_variable_names(L, R, S).
sys_filter_variable_names([X=Y|L], R, [X is Z|S]) :-
   sys_printable_value(Y, Z), !,
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
sys_show_name_or_eq(X is T) :- !,
   sys_write_var(X),
   ttywrite(' is '),
   sys_get_variable_names(N),
   ttywrite_term(T, [quoted(true),priority(699),variable_names(N)]).
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
:- special(sys_write_var/1, 'SpecialSession', 2).

/********************************************************/
/* Locale Modules                                       */
/********************************************************/

/**
 * begin_module(N):
 * The predicate begins a new typein module N.
 */
% begin_module(+Atom)
:- public begin_module/1.
begin_module(N) :-
   absolute_file_name(verbatim(N), D),
   sys_module_action(D, [action(begin_module),sys_link(sys_auto_load)]),
   set_prolog_flag(sys_last_pred, null).

/**
 * end_module:
 * The predicate ends the current typein module.
 */
% end_module
:- public end_module/0.
end_module :-
   sys_peek_stack(D),
   sys_module_action(D, [action(end_module),sys_link(sys_auto_load)]),
   set_prolog_flag(sys_last_pred, null).

/**
 * session_module:
 * The predicate creates a session typein module.
 */
/*
:- public session_module/1.
session_module(N) :-
   sys_pop_stack,
   sys_push_stack(N).
*/

:- private sys_module_action/2.
:- special(sys_module_action/2, 'SpecialSession', 3).

:- private sys_peek_stack/1.
:- special(sys_peek_stack/1, 'SpecialSession', 4).

/*
:- private sys_pop_stack/0.
:- special(sys_pop_stack/0, 'SpecialSession', 5).

:- private sys_push_stack/1.
:- special(sys_push_stack/1, 'SpecialSession', 6).
*/

/***********************************************************/
/* Apropos Utility                                         */
/***********************************************************/

/**
 * apropos(P):
 * The predicate succeeds in listing the public predicates on the
 * terminal that are advertised by the loaded capabilities and that
 * contain the given atom P in their name.
 */
% apropos(+Atom)
:- public apropos/1.
apropos(P) :-
   sys_compile_pattern(P, [boundary(part)], H),
   sys_enum_table(N),
   sys_enum_apropos(N, F/A, M),
   sys_match_pattern(H, F),
   ttywriteq(F/A),
   ttywrite('\t'),
   ttywrite(M), ttynl, fail.
apropos(_).
:- set_predicate_property(apropos/1, sys_notrace).

% sys_compile_pattern(+Atom, -Options, -Compiled)
:- private sys_compile_pattern/3.
sys_compile_pattern(P, O, H) :-
   sys_get_iso_compiler(C),
   sys_pattern_options(O, Q),
   sys_make_pattern(C, P, Q, H).

% sys_enum_table(-Spec)
:- private sys_enum_table/1.
sys_enum_table(library(T)) :-
   sys_current_capability(C),
   sys_capability_property(C, apropos_table(L)),
   member(T, L).

% sys_enum_apropos(+Atom, -Indicator, -Module)
:- private sys_enum_apropos/3.
sys_enum_apropos(N, I, M) :-
   setup_call_cleanup(
      sys_open_resource(N, S),
      (  repeat,
         (  read_line(S, L)
         -> sys_split_line(L, H, T),
            term_atom(I, H),
            term_atom(M, T); !, fail)),
      close(S)).

% sys_open_resource(+Atom, -Stream)
:- private sys_open_resource/2.
sys_open_resource(N, S) :-
   absolute_resource_name(N, P),
   sys_open(P, read, [], S).

% sys_split_line(+Atom, -Atom, -Atom)
:- private sys_split_line/3.
sys_split_line(L, H, T) :-
   sub_atom(L, P, 1, '\t'),
   sub_atom(L, 0, P, H),
   atom_length(L, N),
   Q is P+1,
   M is N-Q,
   sub_atom(L, Q, M, T).
