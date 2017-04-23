/**
 * This module provides matrixes of vector rows. A matrix is a
 * compound with varying number of vectors. A vector can be
 * accessed by the predicate []/3. The first vector has the
 * index one. An element can be accessed by the predicate []/4.
 * The first element in each vector has the index one. The arity
 * of the matrix can be queried by the predicate len/2. Vectors
 * can be created by the two special forms [_ | _] and {_ | _}
 * introduced in the module element.
 *
 * Examples:
 * ?- X is [[A,B],[C,D]], Y is X[2][1].
 * X is [[A,B],[C,D]],
 * Y is C
 * ?- X is [[A,B],[C,D]], Y is X[2,1].
 * X is [[A,B],[C,D]],
 * Y is C
 *
 * This module provides arithmetic for matrixes. Besides change sign,
 * addition and subtraction, we also find multiplication, division and
 * power. The multiplication uses the usual multiplication sign (*)/2
 * despite the fact that matrix multiplication is not commutative. Power
 * is defined for an integer exponent. Operations such as transposing
 * are currently not provided.
 *
 * Examples:
 * ?- X is [[1,1/2],[1/2,1/3]], Y is X^(-1).
 * X is [[1,1/2],[1/2,1/3]],
 * Y is [[4,-6],[-6,12]]
 * ?- X is [[1,1/A],[1,1]], Y is X^(-1).
 * X is [[1,1/A],[1,1]],
 * Y is [[-A/(1-A),1/(1-A)],[A/(1-A),-A/(1-A)]]
 *
 * The matrix inversion is implemented by an exchange step method. It
 * works for constant and symbol expression elements. We have not yet
 * implemented pivot search so that the current implementation might
 * not find an inversion even if there exists one. Error handling is
 * rudimentary. Cancellation does not yet generate non-zero
 * side conditions.
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

:- package(library(jekmin/frequent/gauss)).
:- use_package(library(jekmin/frequent/groebner)).
:- use_package(library(jekpro/frequent/misc)).

:- module(matrice, []).

:- use_module('../groebner/generic').
:- use_module(library(advanced/arith)).
:- use_module(library(misc/residue)).
:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).

/***********************************************************/
/* Array Builder & Access                                  */
/***********************************************************/

/**
 * X[Y, Z]:
 * The predicate succeeds in Z with the Y-the vector
 * of the matrix X.
 */
% [](+Matrice, +Integer, -Vector)
:- override []/3.
:- public []/3.
X [Y, Z] :-
   integer(Y),
   arg(Y, X, Z).

/**
 * X[Y, Z, T]:
 * The predicate succeeds in T with the Z-the element
 * of the Y-the vector of X.
 */
% [](+Matrice, +Integer, +Integer, -Element)
:- override []/4.
:- public []/4.
X [Y, Z, T] :-
   integer(Y),
   integer(Z),
   arg(Y, X, H),
   arg(Z, H, T).

/**
 * len(X, Y):
 * The predicate succeeds in Y with the number of vectors
 * in the matrix X.
 */
% len(+Matrice, -Integer)
:- public len/2.
len(X, Y) :-
   functor(X, _, Y).

/***********************************************************/
/* Basic Arithmetic                                        */
/***********************************************************/

/**
 * -(X, Y):
 * The predicate succeeds in Y with the sign changed matrix X.
 */
% -(+Matrice, -Matrice)
:- override (-)/2.
:- public (-)/2.
X - Y :-
   L is len(X),
   Y is {-X[I]|between(1, L, I)}.

/**
 * +(X, Y, Z):
 * The predicate succeeds in Z with the sum of the matrix X and
 * the matrix Y.
 */
% +(+Matrice, +Internal, -Matrice)
:- override (+)/3.
:- public (+)/3.
+(X, Y, Z) :-
   functor(Y, matrice, _),
   L is len(X),
   L =:= len(Y),
   Z is {X[I]+Y[I]|between(1, L, I)}.

/**
 * -(X, Y, Z):
 * The predicate succeeds in Z with the matrix X subtracted
 * by the matrix Y.
 */
% -(+Matrice, +Internal, -Matrice)
:- override (-)/3.
:- public (-)/3.
-(X, Y, Z) :-
   functor(Y, matrice, _),
   L is len(X),
   L =:= len(Y),
   Z is {X[I]-Y[I]|between(1, L, I)}.

/**
 * *(X, Y, Z):
 * The predicate unifies Z with the product of the matrix X followed
 * by the matrix Y.
 */
% *(+Matrice, +Internal, -Matrice)
:- override * /3.
:- public * /3.
*(X, Y, Z) :-
   functor(Y, matrice, _),
   L is len(X[1]),
   L =:= len(Y),
   M is len(X),
   N is len(Y[1]),
   Z is {{sum({X[I,K]*Y[K,J]|between(1, L, K)})|between(1, N, J)}|between(1, M, I)}.

/**
 * /(X, Y, Z):
 * The predicate succeeds in Z with the matrix X divided
 * by the matrix Y.
 */
% /(+Matrice, +Internal, -Matrice)
:- override / /3.
:- public / /3.
/(X, Y, Z) :-
   functor(Y, matrice, _),
   sys_matrice_inv(Y, H),
   Z is X*H.

% sys_matrice_inv(+Matrice, -Matrice)
:- private sys_matrice_inv/2.
sys_matrice_inv(X, R) :-
   L is len(X[1]),
   L =:= len(X),
   sys_matrice_step(1, X, R).

% sys_matrice_step(+Integer, +Matrice, -Matrice)
:- private sys_matrice_step/3.
sys_matrice_step(K, X, R) :-
   N is len(X),
   user:(K =< N), !,
   L = K,
   P is 1/X[K,L],
   Y is {{V|between(1, N, J),
            (  user:(I =:= K)
            -> (  user:(J =:= L)
               -> V = P
               ;  V is -X[I,J]*P)
            ;  user:(J =:= L)
            -> V = Q
            ;  V is X[I,J]-X[K,J]*Q)}|between(1, N, I),
                                      Q is X[I,L]*P},
   M is K+1,
   sys_matrice_step(M, Y, R).
sys_matrice_step(_, X, X).

/**
 * ^(X, Y, Z):
 * The predicate succeeds in Z with the Y-the power of the matrix X.
 */
% ^(+Matrice, +Integer, -Matrice)
:- override ^ /3.
:- public ^ /3.
^(X, Y, R) :-
   user:(Y < 0), !,
   user:Y - Z,
   H is X^Z,
   sys_matrice_inv(H, R).
^(X, 0, R) :- !,
   L is len(X),
   L =:= len(X[1]),
   R is {{V|between(1, L, J),
            (  user:(I =:= J)
            -> V = 1
            ;  V = 0)}|between(1, L, I)}.
^(X, Y, R) :-
   user:mod(Y, 2, 1), !,
   user: -(Y, 1, Z),
   R is X^Z*X.
^(X, Y, R) :-
   user: //(Y, 2, Z),
   H is X^Z,
   R is H*H.

/***********************************************************/
/* CAS Display Hook                                        */
/***********************************************************/

/**
 * sys_printable_value(F, G):
 * The predicate succeeds in G with a custom form of F. The
 * predicate should be extended for custom forms.
 */
% sys_printable_value(+Term, -Term)
:- public residue:sys_printable_value/2.
:- multifile residue:sys_printable_value/2.
residue:sys_printable_value(X, _) :-
   var(X), !, fail.
residue:sys_printable_value(F, G) :-
   functor(F, matrice, _), !,
   F =.. [_|H],
   sys_portray_matrice(H, G).

% sys_portray_matrice(+List, -List)
:- private sys_portray_matrice/2.
sys_portray_matrice([X|L], [Y|R]) :-
   printable(X, Y),
   sys_portray_matrice(L, R).
sys_portray_matrice([], []).

/*********************************************************************/
/* Generic Hook                                                      */
/*********************************************************************/

/**
 * X is E:
 * The predicate succeeds in evaluating E by using polymorphism.
 */
% is(-Internal, +Expr)
:- override generic:is/2.
:- multifile generic:is/2.
:- public generic:is/2.
:- meta_predicate generic:(?is#(1)).
generic:(X is E) :-
   var(E), !,
   sys_ensure_serno(E),
   sys_freeze_var(E, X).
generic:(X is E) :-
   functor(E, matrice, _), !,
   X = E.

:- multifile generic:is_abnormal/1.
:- public generic:is_abnormal/1.
generic:is_abnormal(E) :-
   functor(E, matrice, _).
