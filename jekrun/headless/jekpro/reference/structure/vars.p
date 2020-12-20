/**
 * The predicate term_variables/2 allows collecting the
 * un-instantiated variables that occur in a term. The predicate
 * will thus return an empty list if the term was ground. Finally
 * the predicate term_singletons/2 collects the un-instantiated
 * variables that only occur once. They are a subset of all the
 * variables that occur in the term.
 *
 * Example:
 * ?- term_variables(f(g(X,Y),X), L).
 * L = [X, Y]
 *
 * ?- term_singletons(f(g(X,Y),X), L).
 * L = [Y]
 *
 * An alternative to using the ‘$VAR’(<number>) construct is dynamically
 * creating a variable names map. This has the advantage that the construct
 * itself can be written out. The predicate sys_number_variables/4 helps
 * in creating a variable names map. The resulting variable names map
 * can be used with the predicates write_term/[2,3].
 *
 * The predicate ground/1 suceeds if the term has no un-instantiated variables.
 * The predicate nonground/2 can be used to pick the first un-instantiated
 * variable of a term without listing all un-instantiated variables.
 * The predicate acyclic_term/1 check whether the given term is non-cyclic.
 * The predicate safe_term_variables/2 lists the un-instantiated variables
 * even for a cyclic term.
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

:- use_package(foreign(jekpro/reference/structure)).

:- module(user, []).

/**
 * term_variables(X, L): [TC2 8.5.5]
 * term_variables(X, L, R):
 * The predicate succeeds when L unifies with the variables of X.
 * The ternary variant produces a difference list.
 */
% term_variables(+Term, -List)
:- public term_variables/2.
term_variables(X, L) :-
   term_variables(X, L, []).

% term_variables(+Term, -List, +List)
:- public term_variables/3.
:- special(term_variables/3, 'SpecialVars', 0).

/**
 * term_singletons(X, L):
 * The predicate succeeds when L unifies with the variables of X
 * that occur only once.
 */
% term_singletons(+Term, -List)
:- public term_singletons/2.
:- special(term_singletons/2, 'SpecialVars', 1).

/**
 * numbervars(X, N, M):
 * The predicate instantiates the un-instantiated variables of the term X with
 * compounds of the form ‘$VAR’(<index>). The <index> starts with N. The predicate
 * succeeds when M unifies with the next available <index>.
 */
% numbervars(+Term, +Integer, -Integer)
:- public numbervars/3.
:- special(numbervars/3, 'SpecialVars', 2).

/**
 * sys_number_variables(V, N, S, M):
 * The predicate succeeds with variable names M resulting from giving names
 * to the variables in V, respecting the variable names N and the unnamed
 * singletons S.
 */
% sys_number_variables(+List, +Assoc, +List, -Assoc)
:- public sys_number_variables/4.
:- special(sys_number_variables/4, 'SpecialVars', 3).

/**
 * ground(X): [TC2 8.3.10]
 * The predicate succeeds when X is a ground term, i.e. contains
 * no variables.
 */
% ground(+Term)
% already defined in member

/**
 * nonground(X, Y):
 * The predicate succeeds when X is not a ground term, i.e.
 * contains a variable, and unifies Y with the first variable.
 */
% nonground(+Term, -Var)
:- public nonground/2.
:- special(nonground/2, 'SpecialVars', 4).

/**
 * acyclic_term(X): [TC2 8.3.11]
 * The predicate succeeds when X is an acyclic term, i.e. contains
 * no cycles.
 */
% acyclic_term(+Term)
:- public acyclic_term/1.
:- special(acyclic_term/1, 'SpecialVars', 5).

/**
 * safe_term_variables(X, L):
 * safe_term_variables(X, L, R):
 * The predicate succeeds when L unifies with the variables of X,
 * even when X is a cyclic term. The ternary variant produces a
 * difference list.
 */
% safe_term_variables(+Term, -List)
:- public safe_term_variables/2.
safe_term_variables(X, L) :-
   safe_term_variables(X, L, []).

% safe_term_variables(+Term, -List, +List)
:- public safe_term_variables/3.
:- special(safe_term_variables/3, 'SpecialVars', 6).
