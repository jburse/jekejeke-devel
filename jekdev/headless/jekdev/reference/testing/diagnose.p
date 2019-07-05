/**
 * This module allows the online display of test results. Beforehand
 * the module runner needs to be used to produce the test results. The
 * predicate diagnose_online/0 will then first present a listing of
 * the theories and their summarized test case success and failure
 * count. The end-user can then choose a predicate and the summarized
 * results will be showed there. Finally the end-user can inspect an
 * individual test case.
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

:- package(library(jekdev/reference/testing)).

:- module(diagnose, []).
:- use_module(library(stream/console)).
:- use_module(library(inspection/frame)).
:- use_module(library(inspection/provable)).
:- use_module(runner).

/*************************************************************/
/* List Suites                                               */
/*************************************************************/

% list_result_suite
:- private list_result_suite/0.
list_result_suite :-
   write('Ok\tNok\tSuite'), nl, list_result_suite_data,
   result_summary(Ok-Nok),
   write(Ok),
   write('\t'),
   write(Nok),
   write('\tTotal'), nl.

% list_result_suite_data
:- private list_result_suite_data/0.
list_result_suite_data :-
   result_suite(Suite, Ok-Nok),
   write(Ok),
   write('\t'),
   write(Nok),
   write('\t'),
   write(Suite), nl, fail.
list_result_suite_data.

/*************************************************************/
/* List Predicates                                           */
/*************************************************************/

% list_result_predicate(+Atom)
:- private list_result_predicate/1.
list_result_predicate(Suite) :-
   write('Ok\tNok\tPredicate'), nl,
   list_result_predicate_data(Suite),
   result_suite(Suite, Ok-Nok),
   write(Ok),
   write('\t'),
   write(Nok),
   write('\tTotal'), nl.

% list_result_predicate_data(+Atom)
:- private list_result_predicate_data/1.
list_result_predicate_data(Suite) :-
   result_predicate(Fun, Arity, Suite, Ok-Nok),
   write(Ok),
   write('\t'),
   write(Nok),
   write('\t'),
   writeq(Fun/Arity), nl, fail.
list_result_predicate_data(_).

/*************************************************************/
/* List Cases                                                */
/*************************************************************/

% list_result(+Atom, +Integer, +Atom)
:- private list_result/3.
list_result(Fun, Arity, Suite) :-
   write('Ok\tNok\tCase'), nl,
   write('Body'), nl,
   list_result_data(Fun, Arity, Suite),
   result_predicate(Fun, Arity, Suite, Ok-Nok),
   write(Ok),
   write('\t'),
   write(Nok),
   write('\tTotal'), nl.

% list_result_data(+Atom,+Integer,+Atom)
:- private list_result_data/3.
list_result_data(Fun, Arity, Suite) :-
   result(Fun, Arity, Suite, Case, Ok-Nok),
   write(Ok),
   write('\t'),
   write(Nok),
   write('\t'),
   write(Case), nl,
   write('\t\t'),
   list_test_case_data(Fun, Arity, Suite, Case), nl, fail.
list_result_data(_, _, _).

% list_test_case_data(+Atom,+Integer,+Atom,+Integer)
:- private list_test_case_data/4.
list_test_case_data(Fun, Arity, Suite, Case) :-
   rule_ref(case(Fun, Arity, Suite, Case), Body, _),
   callable_property(Body, sys_variable_names(N)),
   write_term(Body, [quoted(true),context(0),variable_names(N)]),
   write('. '), fail.
list_test_case_data(_, _, _, _).

/**
 * diagnose_online:
 * The predicate starts an online drill down of the test results.
 */
% diagnose_online
:- public diagnose_online/0.
diagnose_online :- list_result_suite,
   write('Suite: '), flush_output,
   read(Suite),
   list_result_predicate(Suite),
   write('Functor/Arity: '), flush_output,
   read(Fun/Arity),
   list_result(Fun, Arity, Suite).
