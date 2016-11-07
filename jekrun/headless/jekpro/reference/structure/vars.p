/**
 * The test predicate ground/1 checks whether the given term is
 * ground. This means that no un-instantiated variable occurs in
 * the term. The predicate term_variables/2 allows collecting the
 * un-instantiated variables that occur in a term. The predicate
 * will thus return an empty list if the term was ground. Finally
 * the predicate sys_term_singletons/2 collects the un-instantiated
 * variables that only occur once. They are a subset of all the
 * variables that occur in the term.
 *
 * Example:
 * ?- sys_goal_kernel(X^p(X,Y),K).
 * K = p(X,Y)
 * ?- sys_goal_globals(X^p(X,Y),L).
 * L = [Y]
 *
 * Further there are predicates to deal with existential quantifiers.
 * The existential quantifier is represented by the (^)/2 operator.
 * In a goal X1^..^Xn^K we call K the kernel of the goal and the
 * variables K subtracted by the variables of X1,..,Xn the global
 * variables of the goal. The predicates sys_goal_kernel/2 and
 * sys_goal_globals/2 cater for the determination of the kernel
 * and the global variables of a goal.
 *
 * An alternative to using the ‘$VAR’(<number>) construct is dynamically
 * creating a variable names map. This has the advantage that the construct
 * itself can be written out. The predicate sys_number_variables/4 helps
 * in creating a variable names map. The resulting variable names map
 * can be used with the predicates write_term/[2,3]. The variable names
 * map from the current top-level query can be retrieved via the
 * predicate sys_get_variable_names/1.
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

:- use_package(foreign(jekpro/reference/structure)).

:- module(user, []).

/**
 * term_variables(X, L): [TC2 8.5.5]
 * term_variables(X, L, R):
 * The predicate succeeds when L unifies with the variables of X.
 * The variant with a third argument produces a difference list.
 */
% term_variables(+Term, -List)
:- public term_variables/2.
:- special(term_variables/2, 'SpecialVars', 0).

% term_variables(+Term, -List, +List)
:- public term_variables/3.
:- special(term_variables/3, 'SpecialVars', 1).

/**
 * sys_term_singletons(X, L):
 * The predicate succeeds when L unifies with the variables of X
 * that occur only once.
 */
% sys_term_singletons(+Term, -List)
:- public sys_term_singletons/2.
:- special(sys_term_singletons/2, 'SpecialVars', 2).

/**
 * sys_goal_kernel(G, K):
 * The predicate succeeds when K unifies with the kernel of the goal G.
 */
% sys_goal_kernel(+GoalQuant, -Goal)
:- public sys_goal_kernel/2.
:- special(sys_goal_kernel/2, 'SpecialVars', 3).

/**
 * sys_goal_globals(G, L):
 * The predicate succeeds when L unifies with the global variables of the goal G.
 */
% sys_goal_globals(+GoalQuant, -List)
:- public sys_goal_globals/2.
:- special(sys_goal_globals/2, 'SpecialVars', 4).

/**
 * numbervars(X, N, M):
 * The predicate instantiates the un-instantiated variables of the term X with
 * compounds of the form ‘$VAR’(<index>). The <index> starts with N. The predicate
 * succeeds when M unifies with the next available <index>.
 */
% numbervars(+Term, +Integer, -Integer)
:- public numbervars/3.
:- special(numbervars/3, 'SpecialVars', 5).

/**
 * sys_number_variables(V, N, S, M):
 * The predicate succeeds with variable names M resulting from giving names
 * to the variables in V, respecting the variable names N and the unnamed
 * singletons S.
 */
% sys_number_variables(+List, +Assoc, +List, -Assoc)
:- public sys_number_variables/4.
:- special(sys_number_variables/4, 'SpecialVars', 6).

/**
 * ground(X): [TC2 8.3.10]
 * The predicate succeeds when X is a ground term, i.e. contains
 * no variables.
 */
% already defined in member

/**
 * sys_get_variable_names(L):
 * The predicate retrieves the current variable names
 * from the top-level query.
 */
% sys_get_variable_names(-VariableNames)
:- public sys_get_variable_names/1.
:- special(sys_get_variable_names/1, 'SpecialVars', 7).

/**
 * acyclic_term(X): [TC2 8.3.11]
 * The predicate succeeds when X is an acyclic term, i.e. contains
 * no cycles.
 */
% acyclic_term(+Term)
:- public acyclic_term/1.
:- special(acyclic_term/1, 'SpecialVars', 8).
