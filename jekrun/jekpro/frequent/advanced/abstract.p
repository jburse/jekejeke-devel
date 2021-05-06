/**
 * Jekejeke Prolog also provides a simple denotation for lambda abstraction.
 * We use the operator (\)/2 to denote abstracted terms and the operator (^)/2
 * to denote local terms. We can describe the lambda abstraction via the
 * following syntax:
 *
 * abstraction      --> binder "\" body_with_local.
 * body_with_local  --> local "^" body_with_local.
 *                    | body.
 * binder           --> term.
 * local            --> term.
 *
 * It is possible to abstract goals and closures. The result is a new closure
 * with an incremented closure index. The binder can be an arbitrary term,
 * which allows pattern matching. The local can be an arbitrary term as well,
 * which allows combining multiple local variables. The global variables of
 * a lambda abstraction are aliased along invocations.
 *
 * Examples:
 * ?- map(X\Y\(H is X+1, Y is H*H),[1,2,3],R).
 * No                           % Aliasing prevents success.
 * ?- map(X\Y\H^(H is X+1, Y is H*H),[1,2,3],R).
 * R = [4,9,16]                 % Now everything is fine.
 *
 * When a lambda abstraction is invoked the binder is replaced by the argument.
 * In normal lambda calculus the global variables of the argument can clash
 * with further binders in the body. In our implementation it can also happen
 * that binders, local variables and global variables can clash. Local
 * variables can be used to prevent clashes by renaming variables:
 *
 * Examples:
 * ?- K=X\Y\ =(X), call(K,Y,B,R).
 * K = X\Y\ =(X),
 * B = R                       % Clash gives wrong result.
 * ?- K=X\Y^Y\ =(X), call(K,Y,B,R).
 * K = X\Y^Y\ =(X),
 * R = Y                       % Now everything is fine.
 *
 * This module also provides a couple of common higher order list
 * processing predicates. These predicates take a closure and apply it to
 * a number of list arguments. Among the predicates, we find the predicates
 * maplist/[2-5] applying a closure in tandem to lists. The predicates
 * foldl/[4-7] additionally allow an accumulator.
 *
 * Finally, there are predicates sys_goal_kernel/2 and sys_goal_globals/2 to
 * deal with existential quantifiers. The existential quantifier is
 * represented by the (^)/2 operator. In a goal X1^..^Xn^K we call K the
 * kernel of the quantified goal and the variables of K subtracted by the
 * variables of  X1,..,Xn the global variables of the quantified goal.
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

:- package(library(jekpro/frequent/advanced)).
:- package(foreign(jekpro/frequent/advanced)).

:- module(abstract, []).

:- public infix(\).
:- op(200, xfy, \).

% already defined in elem.p
% :- public (^)/3.
% :- meta_predicate ^(?,1,?).
% ^(_,_,_) :- throw(error(existence_error(body, (^)/3), _)).

:- public ^ /4.
:- meta_predicate ^(?, 2, ?, ?).
^(_, _, _, _) :- throw(error(existence_error(body, ^ /4), _)).

:- public ^ /5.
:- meta_predicate ^(?, 3, ?, ?, ?).
^(_, _, _, _, _) :- throw(error(existence_error(body, ^ /5), _)).

:- public ^ /6.
:- meta_predicate ^(?, 4, ?, ?, ?, ?).
^(_, _, _, _, _, _) :- throw(error(existence_error(body, ^ /6), _)).

:- public ^ /7.
:- meta_predicate ^(?, 5, ?, ?, ?, ?, ?).
^(_, _, _, _, _, _, _) :- throw(error(existence_error(body, ^ /7), _)).

:- public ^ /8.
:- meta_predicate ^(?, 6, ?, ?, ?, ?, ?, ?).
^(_, _, _, _, _, _, _, _) :- throw(error(existence_error(body, ^ /8), _)).

:- public ^ /9.
:- meta_predicate ^(?, 7, ?, ?, ?, ?, ?, ?, ?).
^(_, _, _, _, _, _, _, _, _) :- throw(error(existence_error(body, ^ /9), _)).

/**
 * \(X, A, Y1, .., Yn):
 * The predicate is defined for 1 ≤ n ≤ 7. The goal \(X, A, Y1, .., Yn)
 * succeeds whenever the goal call(A[X/Y1], Y2, ..., Yn) succeeds.
 */
% \(+Var, +Goal, +Term, ..)
:- public (\)/3.
:- nonstrict (\)/3.
:- meta_predicate \(?, 0, ?).
\(X, A, Y) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X, B, L), rec(Y, Q, L)),
   call(Q).

:- public (\)/4.
:- meta_predicate \(?, 1, ?, ?).
\(X, A, Y, Z) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X, B, L), rec(Y, Q, L)),
   call(Q, Z).

:- public (\)/5.
:- meta_predicate \(?, 2, ?, ?, ?).
\(X, A, Y, Z, T) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X, B, L), rec(Y, Q, L)),
   call(Q, Z, T).

:- public (\)/6.
:- meta_predicate \(?, 3, ?, ?, ?, ?).
\(X, A, Y, Z, T, U) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X, B, L), rec(Y, Q, L)),
   call(Q, Z, T, U).

:- public (\)/7.
:- meta_predicate \(?, 4, ?, ?, ?, ?, ?).
\(X, A, Y, Z, T, U, V) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X, B, L), rec(Y, Q, L)),
   call(Q, Z, T, U, V).

:- public (\)/8.
:- meta_predicate \(?, 5, ?, ?, ?, ?, ?, ?).
\(X, A, Y, Z, T, U, V, W) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X, B, L), rec(Y, Q, L)),
   call(Q, Z, T, U, V, W).

:- public (\)/9.
:- meta_predicate \(?, 6, ?, ?, ?, ?, ?, ?, ?).
\(X, A, Y, Z, T, U, V, W, R) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X, B, L), rec(Y, Q, L)),
   call(Q, Z, T, U, V, W, R).

/****************************************************************/
/* Higher Order List Processing                                 */
/****************************************************************/

/**
 * maplist(C, L1, ..., Ln):
 * The predicate succeeds in applying the closure C to the
 * elements of L1, ..., Ln. The predicate is currently
 * defined for 1 ≤ n ≤ 4.
 */
:- public maplist/2.
:- meta_predicate maplist(1, ?).
maplist(_, []).
maplist(C, [X|L]) :-
   call(C, X),
   maplist(C, L).

:- public maplist/3.
:- meta_predicate maplist(2, ?, ?).
maplist(_, [], []).
maplist(C, [X|L], [Y|R]) :-
   call(C, X, Y),
   maplist(C, L, R).

:- public maplist/4.
:- meta_predicate maplist(3, ?, ?, ?).
maplist(_, [], [], []).
maplist(C, [X|L], [Y|R], [Z|S]) :-
   call(C, X, Y, Z),
   maplist(C, L, R, S).

:- public maplist/5.
:- meta_predicate maplist(4, ?, ?, ?, ?).
maplist(_, [], [], [], []).
maplist(C, [X|L], [Y|R], [Z|S], [U|T]) :-
   call(C, X, Y, Z, U),
   maplist(C, L, R, S, T).

/**
 * foldl(C, L1, ..., Ln, I, O):
 * The predicate succeeds in applying the closure C to the
 * elements of L1, ..., Ln and accumulating the result among
 * I and O. The predicate is currently defined for 1 ≤ n ≤ 4.
 */
:- public foldl/4.
:- meta_predicate foldl(3, ?, ?, ?).
foldl(_, [], P, P).
foldl(C, [X|L], P, Q) :-
   call(C, X, P, H),
   foldl(C, L, H, Q).

:- public foldl/5.
:- meta_predicate foldl(4, ?, ?, ?, ?).
foldl(_, [], [], P, P).
foldl(C, [X|L], [Y|R], P, Q) :-
   call(C, X, Y, P, H),
   foldl(C, L, R, H, Q).

:- public foldl/6.
:- meta_predicate foldl(5, ?, ?, ?, ?, ?).
foldl(_, [], [], [], P, P).
foldl(C, [X|L], [Y|R], [Z|S], P, Q) :-
   call(C, X, Y, Z, P, H),
   foldl(C, L, R, S, H, Q).

:- public foldl/7.
:- meta_predicate foldl(6, ?, ?, ?, ?, ?, ?).
foldl(_, [], [], [], [], P, P).
foldl(C, [X|L], [Y|R], [Z|S], [U|T], P, Q) :-
   call(C, X, Y, Z, U, P, H),
   foldl(C, L, R, S, T, H, Q).

/****************************************************************/
/* Helpers                                                      */
/****************************************************************/

/**
 * sys_goal_kernel(G, K):
 * The predicate succeeds when K unifies with the kernel of the goal G.
 */
% sys_goal_kernel(+GoalQuant, -Goal)
:- public sys_goal_kernel/2.
:- special(sys_goal_kernel/2, 'SpecialAbstract', 0).

/**
 * sys_goal_globals(G, L):
 * The predicate succeeds when L unifies with the global variables of the goal G.
 */
% sys_goal_globals(+GoalQuant, -List)
:- public sys_goal_globals/2.
:- special(sys_goal_globals/2, 'SpecialAbstract', 1).
