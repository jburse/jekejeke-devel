/**
 * This module allows executing test cases. The test runner
 * executes the test cases and sum-marizes the results. The
 * test cases and the results are certain facts in the knowledge
 * base. The test runner can be invoked via the predicate
 * runner_batch/0. The coverage depends on how well the test
 * cases are designed in respect of the probed execution paths.
 *
 * The test cases are stored by as facts and rules in the
 * following form:
 *
 *      :- multifile(ref/4).
 *      :- discontiguous(ref/4).
 *      % ref(Fun, Arity, Suite, Ref).
 *
 *      :- multifile(case/4).
 *      :- discontiguous(case/4).
 *      % case(Fun, Arity, Suite, Number) :- Body.
 *
 * Note: Fun/Arity is used to denote predicates, and Fun/-Arity-1
 * is used to denote evaluable functions. The test steps and the
 * test validation points need to be implemented in the body of the
 * predicate case/4. The body is assumed to terminate, the test
 * runner doesn't impose some timeout currently. The body can be used
 * to check a multitude of scenarios:
 *
 * Examples:
 * % Check whether the goal succeeds:
 *      Goal
 * % Check whether the determinitic goal succeeds with result Value:
 *      Goal, Var==Value
 * % Check whether the non-determinitic goal first succeeds with result Value:
 *      Goal, !, Var==Value
 * % Check whether the goal fails:
 *      \+ Goal:
 * % Check whether the goal succeeds on redo:
 *      findall([],Goal,[_,_|_])
 * % Check whether the goal succeeds on redo with result Value:
 *      findall(Var,Goal,[_,Var|_]), Var==Value
 * % Check whether the goal fails on redo:
 *      findall([],Goal,[_])
 * % Check whether the goal throws the exception Value:
 *      catch(Goal,error(Var,_),true), Var==Value
 *
 * The testing is not limited to the above example scenarios. A
 * particular application domain might need additional test helper
 * predicates to express the desired test steps and test validation
 * points. Examples are the CLP(FD) test cases which use the
 * predicate call_residue/2. The test results are ok and not-ok counts,
 * a non-conclusive count is currently not provided.
 *
 * The results are stored by the following facts:
 *
 *      :- public result_summary/1.
 *      :- dynamic result_summary/1.
 *      % result_summary(OkNok),
 *
 *      :- public result_suite/2.
 *      :- dynamic result_suite/2.
 *      % result_suite(Suite, OkNok).
 *
 *      :- public result_predicate/4.
 *      :- dynamic result_predicate/4.
 *      % result_predicate(Fun, Arity, Suite, OkNok).
 *
 *      :- public result/5.
 *      :- dynamic result/5.
 *      % result(Fun, Arity, Suite, Number, OkNok).
 *
 * The results are accessible by the diagnose module or the report
 * module from the Jekejeke Prolog development environment. Or the
 * tester might code its own analysis based on these facts. In the
 * future we might as well provide additional tools, such as a
 * coverage analysis tool or similar.
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

:- module(runner, []).
:- use_module(library(inspection/frame)).
:- use_module(helper).

:- public ref/4.
:- multifile ref/4.
:- static ref/4.

:- public case/4.
:- multifile case/4.
:- static case/4.

/****************************************************************/
/* Summary Update                                               */
/****************************************************************/

% result_summary(-OkNok)
:- public result_summary/1.
:- dynamic result_summary/1.

% sys_remove_summary
:- private sys_remove_summary/0.
sys_remove_summary :-
   retract(result_summary(_)), fail.
sys_remove_summary.

% sys_update_summary(+OkNok)
:- private sys_update_summary/1.
sys_update_summary(L) :-
   retract(result_summary(R)), !,
   sys_add_oknok(L, R, S),
   assertz(result_summary(S)).
sys_update_summary(L) :-
   assertz(result_summary(L)).

/****************************************************************/
/* Suite Update                                                */
/****************************************************************/

% result_suite(-Suite, -OkNok)
:- public result_suite/2.
:- dynamic result_suite/2.

% sys_remove_suite.
:- private sys_remove_suite/0.
sys_remove_suite :-
   retract(result_suite(_, _)), fail.
sys_remove_suite.

% sys_update_suite(+Suite, +OkNok).
:- private sys_update_suite/2.
sys_update_suite(Suite, L) :-
   retract(result_suite(Suite, R)), !,
   sys_add_oknok(L, R, S),
   assertz(result_suite(Suite, S)).
sys_update_suite(Suite, L) :-
   assertz(result_suite(Suite, L)).

/****************************************************************/
/* Predicate Update                                             */
/****************************************************************/

% result_predicate(-Fun, -Arity, -Suite, -OkNok)
:- public result_predicate/4.
:- dynamic result_predicate/4.

% sys_remove_predicate.
:- private sys_remove_predicate/0.
sys_remove_predicate :-
   retract(result_predicate(_, _, _, _)), fail.
sys_remove_predicate.

% sys_update_predicate(+Fun, +Arity, +Suite, +OkNok)
:- private sys_update_predicate/4.
sys_update_predicate(Fun, Arity, Suite, L) :-
   retract(result_predicate(Fun, Arity, Suite, R)), !,
   sys_add_oknok(L, R, S),
   assertz(result_predicate(Fun, Arity, Suite, S)).
sys_update_predicate(Fun, Arity, Suite, L) :-
   assertz(result_predicate(Fun, Arity, Suite, L)).

/****************************************************************/
/* Result Update                                                */
/****************************************************************/

% result(-Fun, -Arity, -Suite, -Case, -OkNok)
:- public result/5.
:- dynamic result/5.

% sys_remove_result.
:- private sys_remove_result/0.
sys_remove_result :-
   retract(result(_, _, _, _, _)), fail.
sys_remove_result.

% sys_update_result(+Fun, +Arity, +Suite, +Case, +OkNok)
:- private sys_update_result/5.
sys_update_result(Fun, Arity, Suite, Case, L) :-
   retract(result(Fun, Arity, Suite, Case, R)), !,
   sys_add_oknok(L, R, S),
   assertz(result(Fun, Arity, Suite, Case, S)).
sys_update_result(Fun, Arity, Suite, Case, L) :-
   assertz(result(Fun, Arity, Suite, Case, L)).

/****************************************************************/
/* Run Bodies                                                   */
/****************************************************************/

% sys_test_body(+Body, -OkNok).
:- private sys_test_body/2.
sys_test_body(Body, 1-0) :-
   catch(Body, _, fail), !.
sys_test_body(_, 0-1).

/**
 * runner_batch:
 * The predicate executes the test cases, collects and summarizes the results.
 */
% runner_batch
:- public runner_batch/0.
runner_batch :- sys_remove_result, sys_remove_predicate, sys_remove_suite, sys_remove_summary,
   rule_frame(case(Fun, Arity, Suite, Case), Body, _),
                                                  %   write('Case='), write(Case), nl,
   sys_test_body(Body, OkNok),
   sys_update_result(Fun, Arity, Suite, Case, OkNok),
   sys_update_predicate(Fun, Arity, Suite, OkNok),
   sys_update_suite(Suite, OkNok),
   sys_update_summary(OkNok), fail.
runner_batch.

% runner_batch2
:- public runner_batch2/0.
runner_batch2 :- sys_remove_result, sys_remove_predicate, sys_remove_suite, sys_remove_summary,
   rule_frame(case(Fun, Arity, Suite, Case), Body, _),
   write('Case='),
   write(Case), nl,
   sys_test_body(Body, OkNok),
   sys_update_result(Fun, Arity, Suite, Case, OkNok),
   sys_update_predicate(Fun, Arity, Suite, OkNok),
   sys_update_suite(Suite, OkNok),
   sys_update_summary(OkNok), fail.
runner_batch2.

