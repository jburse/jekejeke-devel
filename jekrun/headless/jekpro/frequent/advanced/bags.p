/**
 * The following predicates do a grouping of the solutions and may
 * succeed more than once. This grouping is based on the determination
 * of the witnesses of a solution. The witness are the global variables
 * of T^G where T is the template and G is the goal.
 *
 * Examples:
 * p(a,y). p(a,x). p(b,x).
 *
 * ?- bagof(X, p(X, Y), L).
 * Y = y, L = [a] ;
 * Y = x, L = [a, b]
 *
 * ?- bagof(X, Y^p(X, Y), L).
 * L = [a, a, b]
 *
 * The predicate bagof/3 will do a sorting of the witnesses but not
 * of the resulting lists. The varia-tion setof/3 will sort the
 * witnesses and the resulting lists. The variations bagof/4 and
 * setof/4 allow specifying sort options.
 *
 * Examples:
 * ?- bagof(Y, p(X, Y), L).
 * X = a, L = [y, x] ;
 * X = b, L = [x]
 *
 * ?- setof(Y, p(X, Y), L).
 * X = a, L = [x, y] ;
 * X = b, L = [x]
 *
 * The predicate copy_term/2 can be used to copy a term. The
 * predicates findall/[3,4] can be used to collect a resulting
 * list without any grouping. The elements will be copied. The
 * predicates foreach/2 and foreach/4 use findall/3 to compute
 * a dynamic conjunction
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

:- use_package(foreign(jekpro/frequent/advanced)).
:- use_package(foreign(matula/util/data)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/reference/structure)).

:- module(user, []).
:- use_module(library(advanced/variant)).
:- use_module(library(advanced/pivot)).
:- use_module(library(advanced/revolve)).
:- use_module(library(runtime/collector)).

:- public ^ /2.
:- meta_predicate ^(?, 0).
^(_, _) :- throw(error(existence_error(body, ^ /2), _)).

/**********************************************************/
/* Bagof Predicates                                       */
/**********************************************************/

/**
 * bagof(T, X1^…^Xn^G, L): [ISO 8.10.2]
 * bagof(T, X1^…^Xn^G, L, O):
 * The predicate determines all the solutions to the matrix G, whereby
 * collecting copies of the template T sorted by the witnesses in a list.
 * The predicate then repeatedly succeeds by unifying the witnesses
 * and when L unifies with the corresponding list. The quaternary
 * predicate takes additional sort options as argument.
 */
% bagof(+Template, +QuantGoal, -List)
:- public bagof/3.
:- meta_predicate bagof(?, 0, ?).
bagof(J, Goal, L) :-
   sys_goal_globals(J^Goal, W),
   sys_revolve_new(R),
   (sys_revolve_bag(Goal, W, R, J), fail; true),
   sys_revolve_pair(R, W-P),
   findall(W-J, sys_pivot_list(P, W-J), H),
   sys_strip_list(H, W, L).

% bagof(+Template, +QuantGoal, -List, +List)
:- public bagof/4.
:- meta_predicate bagof(?, 0, ?, ?).
bagof(J, Goal, L, O) :-
   sys_variant_comparator(O, C),
   sys_goal_globals(J^Goal, W),
   sys_revolve_new(C, R),
   (sys_revolve_bag(Goal, W, R, J), fail; true),
   sys_revolve_pair(R, C, W-P),
   findall(W-J, sys_pivot_list(P, W-J), H),
   sys_strip_list(H, W, L).

/**
 * setof(T, X1^…^Xn^G, L): [ISO 8.10.3]
 * setof(T, X1^…^Xn^G, L, O):
 * The predicate determines the same lists as the predicate bagof/3. But
 * before returning them the lists are sorted by means of the predicate
 * sort/2. The quaternary predicate takes additional sort options as
 * argument.
 */
% setof(+Template, +QuantGoal, -List)
:- public setof/3.
:- meta_predicate setof(?, 0, ?).
setof(J, Goal, L) :-
   sys_goal_globals(J^Goal, W),
   sys_revolve_new(R),
   (sys_revolve_set(Goal, W, R, J), fail; true),
   sys_revolve_pair(R, W-P),
   findall(W-J, sys_pivot_enum(P, W-J), H),
   sys_strip_list(H, W, L).

% setof(+Template, +QuantGoal, -List, +List)
:- public setof/4.
:- meta_predicate setof(?, 0, ?, ?).
setof(J, Goal, L, O) :-
   sys_variant_comparator(O, C),
   sys_goal_globals(J^Goal, W),
   sys_revolve_new(C, R),
   (sys_revolve_set(Goal, W, R, J), fail; true),
   sys_revolve_pair(R, C, W-P),
   findall(W-J, sys_pivot_enum(P, W-J), H),
   sys_strip_list(H, W, L).

/**********************************************************/
/* Grouping Utilities                                     */
/**********************************************************/

% sys_revolve_bag(+Goal, +List, +Ref, +Term)
:- private sys_revolve_bag/4.
:- meta_predicate sys_revolve_bag(0, ?, ?, ?).
sys_revolve_bag(Goal, W, R, J) :-
   sys_goal_kernel(Goal, B),
   B,
   sys_revolve_lookup(R, W, P),
   sys_pivot_add(P, W-J).

% sys_revolve_set(+Goal, +List, +Ref, +Term)
:- private sys_revolve_set/4.
:- meta_predicate sys_revolve_set(0, ?, ?, ?).
sys_revolve_set(Goal, W, R, J) :-
   sys_goal_kernel(Goal, B),
   B,
   sys_revolve_lookup(R, W, P),
   sys_pivot_put(P, W-J).

% sys_strip_list(+List, +Term, -List)
:- private sys_strip_list/3.
sys_strip_list([], _, []).
sys_strip_list([W-J|L], W, [J|R]) :-
   sys_strip_list(L, W, R).

/**********************************************************/
/* All Solutions II                                       */
/**********************************************************/

/**
 * foreach(F, G):
 * Calls the conjunction of those goal instances of G where F succeeds.
 * Variables occurring in G and not occurring in F are shared.
 */
% foreach(+Generator, +Goal)
:- public foreach/2.
:- meta_predicate foreach(0, 0).
foreach(F, G) :-
   sys_goal_kernel(G, B),
   sys_goal_globals(F^G, W),
   findall(W-B, F, H),
   sys_join_keys(H, W),
   sys_call_values(H).

% sys_call_values(+Pairs)
:- private sys_call_values/1.
sys_call_values([]).
sys_call_values([_-V|L]) :-
   call(V),
   sys_call_values(L).

/**
 * foreach(F, G, I, O):
 * Calls the conjunction of those closure instances of G where F succeeds
 * threading them with input I and output O. Variables occurring in G and
 * not occurring in F are shared.
 */
% foreach(+Generator, +Goal, +Term, -Term)
:- public foreach/4.
:- meta_predicate foreach(0, 2, ?, ?).
foreach(F, G, I, O) :-
   sys_goal_kernel(G, B),
   sys_goal_globals(F^G, W),
   findall(W-B, F, H),
   sys_join_keys(H, W),
   sys_call_values(H, I, O).

% sys_call_values(+Pairs, +Term, -Term)
:- private sys_call_values/3.
sys_call_values([], L, L).
sys_call_values([_-V|L], I, O) :-
   call(V, I, H),
   sys_call_values(L, H, O).

% sys_join_keys(+Pairs, +Key)
:- private sys_join_keys/2.
sys_join_keys([], _).
sys_join_keys([K-_|L], K) :-
   sys_join_keys(L, K).
