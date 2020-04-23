/**
 * The following predicates do a grouping of the solutions and may
 * succeed more than once. This grouping is based on the determination
 * of the witnesses of a solution. The witness are the global variables
 * of T^G where T is the template and G is the goal.
 *
 * Examples:
 * p(a,y). p(a,x). p(b,x).
 *
 * ?- bagof(X,p(X,Y),L).
 * Y = y, L = [a] ;
 * Y = x, L = [a, b]
 * ?- bagof(X,Y^p(X,Y),L).
 * L = [a, a, b]
 *
 * The predicate bagof/3 will do a sorting of the witnesses but not
 * of the resulting lists. The varia-tion setof/3 will sort the
 * witnesses and the resulting lists. The variations bagof/4 and
 * setof/4 allow specifying sort options.
 *
 * Examples:
 * p(a). p(b).
 * q(a). q(b). q(c).
 *
 * ?- forall(p(X), q(X)).
 * Yes
 * ?- forall(q(X), p(X)).
 * No
 *
 * The predicate copy_term/2 can be used to copy a term. The predicate
 * findall/3 can be used to collect a resulting list without any
 * grouping. The elements will be copied. The forall/2 performs generate
 * and test and can be used for a bounded universal quantification.
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

:- use_package(foreign(jekpro/frequent/standard)).
:- use_package(foreign(matula/util/data)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/reference/structure)).

:- module(user, []).

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
   (sys_revolve_run(Goal, W, R, J), fail; true),
   sys_revolve_pair(R, W-P),
   findall(W-J, sys_pivot_enum(P, W-J), H),
   sys_strip_list(H, W, L).

% bagof(+Template, +QuantGoal, -List, +List)
:- public bagof/4.
:- meta_predicate bagof(?, 0, ?, ?).
bagof(J, Goal, L, O) :-
   sys_variant_comparator(O, C),
   sys_goal_globals(J^Goal, W),
   sys_revolve_new(C, R),
   (sys_revolve_run(Goal, W, R, J), fail; true),
   sys_revolve_pair(R, C, W-P),
   findall(W-J, sys_pivot_enum(P, W-J), H),
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
setof(T, G, L) :-
   bagof(T, G, H),
   sort(H, L).

% setof(+Template, +QuantGoal, -List, +List)
:- public setof/4.
:- meta_predicate setof(?, 0, ?, ?).
setof(T, G, L, O) :-
   bagof(T, G, H, O),
   sort(H, L).

/**********************************************************/
/* Grouping Utilities                                     */
/**********************************************************/

% sys_revolve_run(+Goal, +List, +Ref, +Term)
:- private sys_revolve_run/4.
:- meta_predicate sys_revolve_run(0, ?, ?, ?).
sys_revolve_run(Goal, W, R, J) :-
   sys_goal_kernel(Goal, B),
   B,
   sys_revolve_lookup(R, W, P),
   sys_pivot_add(P, W-J).

% sys_strip_list(+List, +Term, -List)
:- private sys_strip_list/3.
sys_strip_list([], _, []).
sys_strip_list([W-J|L], W, [J|R]) :-
   sys_strip_list(L, W, R).

/**********************************************************/
/* All Solutions I                                        */
/**********************************************************/

/**
 * findall(T, G, L): [ISO 8.10.1]
 * findall(T, G, L, R):
 * The predicate first finds all the solutions to the goal G, whereby
 * collecting copies of the template T in a list. The predicate then
 * succeeds when L unifies with the list.
 */
% findall(+Template, +Goal, -List)
:- public findall/3.
:- meta_predicate findall(?, 0, ?).
:- special(findall/3, 'SpecialFind', 0).

% findall(+Template, +Goal, -List, +List)
:- public findall/4.
:- meta_predicate findall(?, 0, ?, ?).
:- special(findall/4, 'SpecialFind', 1).

/**
 * forall(A,B): [N208 8.10.4]
 * The predicate succeeds when there is no success of A
 * such that B fails. Otherwise the predicate fails.
 */
:- public forall/2.
:- meta_predicate forall(0, 0).
forall(A, B) :- \+ (A, \+ B).

/**********************************************************/
/* Copy Term                                              */
/**********************************************************/

/**
 * copy_term(X, Y): [ISO 8.5.4]
 * The predicate creates a copy of X and succeeds when the copy unifies with Y.
 */
% copy_term(+Term, -Term)
:- public copy_term/2.
:- special(copy_term/2, 'SpecialFind', 2).

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

/*************************************************************/
/* Pivot Datatype                                            */
/*************************************************************/

/**
 * sys_variant_comparator(O, C):
 * The predicate succeeds in C with the variant comparator
 * for the sort options O.
 */
% sys_variant_comparator(+List, -Comparator)
:- public sys_variant_comparator/2.
:- foreign(sys_variant_comparator/2, 'ForeignBags',
      sysVariantComparator('Interpreter', 'Object')).

/**
 * sys_revolve_new(R):
 * Thre predicate succeeds in R with a new revolve.
 */
% sys_revolve_new(-Revolve)
:- public sys_revolve_new/1.
:- foreign(sys_revolve_new/1, 'ForeignBags', sysRevolveNew).

/**
 * sys_revolve_new(C, R):
 * The predicate succeeds in R with a new revolve
 * for the variant comparator C.
 */
% sys_revolve_new(+Comparator, -Revolve)
:- public sys_revolve_new/2.
:- foreign(sys_revolve_new/2, 'ForeignBags',
      sysRevolveNew('AbstractLexical')).

/**
 * sys_revolve_lookup(R, K, P):
 * The predicate succeeds in P with the old or new pivot
 * for a copy of the key K in the revolve R.
 */
% sys_revolve_lookup(+Revolve, +Term, -Pivot)
:- public sys_revolve_lookup/3.
:- foreign(sys_revolve_lookup/3, 'ForeignBags',
      sysRevolveLookup('Interpreter', 'AbstractMap', 'Object')).

/**
 * sys_revolve_pair(R, U):
 * The predicate succeeds in U with the key value pairs
 * of the revolve R.
 */
% sys_revolve_pair(+Revolve, -Pair)
:- public sys_revolve_pair/2.
:- foreign(sys_revolve_pair/2, 'ForeignBags',
      sysRevolvePair('CallOut', 'AbstractMap')).

/**
 * sys_revolve_pair(R, C, U):
 * The predicate succeeds in U with the key value pairs
 * of the revolve R for the variant comparator C.
 */
% sys_revolve_pair(+Revolve, +Comparator, -Pair)
:- public sys_revolve_pair/3.
:- foreign(sys_revolve_pair/3, 'ForeignBags',
      sysRevolvePair('CallOut', 'AbstractMap', 'AbstractLexical')).

/**
 * sys_pivot_add(P, O):
 * The predicate succeeds extending the pivot P by O.
 */
% sys_pivot_add(+Pivot, +Term)
:- public sys_pivot_add/2.
:- foreign(sys_pivot_add/2, 'ForeignBags',
      sysPivotAdd('Interpreter', 'SetEntry', 'Object')).

/**
 * sys_pivot_enum(R, U):
 * The predicate succeeds in U with the key value pairs
 * of the revolve R.
 */
% sys_pivot_enum(+Pivot, -Term)
:- public sys_pivot_enum/2.
:- foreign(sys_pivot_enum/2, 'ForeignBags',
      sysPivotEnum('CallOut', 'SetEntry')).

