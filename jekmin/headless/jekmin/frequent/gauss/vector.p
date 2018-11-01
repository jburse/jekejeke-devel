/**
 * This module provides vectors of element columns. A vector is a
 * compound with varying number of elements which can be constants
 * or symbolic expressions. An element can be accessed by the
 * predicate []/3. The first element has the index one. The arity
 * of the vector can be queried by the predicate len/2. Vectors can
 * be created by the two special forms [_ | _] and {_ | _} introduced
 * in the module element.
 *
 * Examples:
 * ?- X is [A,B,C], Y is len(X).
 * X is [A,B,C],
 * Y = 3
 * ?- X is [A,B,C], Z is X[2].
 * X is [A,B,C],
 * Z is B
 *
 * The predicate sum/2, min/2 and max/2 can be used to compute the sum,
 * minimum or maximum of the elements of a vector. Further aggregate
 * functions are currently not provided. This module further provides
 * some rudimentary arithmetic such as sign change, addition and subtraction.
 * Other operations such as scalar product or transposing are currently
 * not provided. Error handling is rudimentary.
 *
 * The vector based consing returns a matrix. The consing does currently
 * not check that the second argument is a proper list and that all
 * elements of the list are vectors. The intention here is to use the
 * consing to only create homogenous matrixes of vectors. The reader
 * interested in the methods of the matrix should browse into the
 * module matrix.
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

:- package(library(jekmin/frequent/gauss)).
:- use_package(library(jekmin/frequent/groebner)).
:- use_package(library(jekpro/frequent/misc)).

:- module(vector, []).

:- use_module(../groebner/generic).
:- use_module(library(advanced/arith)).
:- use_module(library(misc/residue)).
:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).

/***********************************************************/
/* Array Builder & Access                                  */
/***********************************************************/

/**
 * .(X, Y, Z):
 * The predicate succeeds in Z with the consing of X and Y.
 */
% .(+Vector, +List, -Matrice)
:- override '.'/3.
:- public '.'/3.
'.'(X, L, Z) :-
   Z =.. [matrice,X|L].

/**
 * X[Y, Z]:
 * The predicate succeeds in Z with the Y-the element
 * of the vector X.
 */
% +Vector [+Integer, -Element]
:- override sys_index/3.
:- public sys_index/3.
X [Y, Z] :-
   integer(Y),
   arg(Y, X, Z).

/**
 * len(X, Y):
 * The predicate succeeds in Y with the number of elements
 * in the vector X.
 */
% len(+Vector, -Integer)
:- public len/2.
len(X, Y) :-
   functor(X, _, Y).

/**
 * sum(X, Y):
 * The predicate succeeds in Y with the sum of elements
 * in the vector X.
 */
% sum(+Vector, -Internal)
:- public sum/2.
sum(X, Y) :-
   X =.. [_|L],
   (  L = [A|B]
   -> sys_sum_vector(B, A, Y)
   ;  Y = 0).

% sys_sum_vector(+List, +Internal, -Internal)
:- private sys_sum_vector/3.
sys_sum_vector([X|L], A, R) :-
   H is A+X,
   sys_sum_vector(L, H, R).
sys_sum_vector([], R, R).

/**
 * min(X, Y):
 * The predicate succeeds in Y with the minimum of the
 * elements in the vector X.
 */
% min(+Vector, -Internal)
:- public min/2.
min(X, Y) :-
   X =.. [_,A|L],
   sys_min_vector(L, A, Y).

% sys_min_vector(+List, +Internal, -Internal)
:- private sys_min_vector/3.
sys_min_vector([X|L], A, R) :-
   H is min(A,X),
   sys_min_vector(L, H, R).
sys_min_vector([], R, R).

/**
 * max(X, Y):
 * The predicate succeeds in Y with the maximum of the
 * elements in the vector X.
 */
% max(+Vector, -Internal)
:- public max/2.
max(X, Y) :-
   X =.. [_,A|L],
   sys_max_vector(L, A, Y).

% sys_max_vector(+List, +Internal, -Internal)
:- private sys_max_vector/3.
sys_max_vector([X|L], A, R) :-
   H is max(A,X),
   sys_max_vector(L, H, R).
sys_max_vector([], R, R).

/***********************************************************/
/* Basic Arithmetic                                        */
/***********************************************************/

/**
 * -(X, Y):
 * The predicate succeeds in Y with the sign changed vector X.
 */
% -(+Vector, -Vector)
:- override (-)/2.
:- public (-)/2.
X - Y :-
   L is len(X),
   Y is {-X[I]|between(1, L, I)}.

/**
 * +(X, Y, Z):
 * The predicate succeeds in Z with the sum of the vector X and
 * the vector Y.
 */
% +(+Vector, +Internal, -Vector)
:- override (+)/3.
:- public (+)/3.
+(X, Y, Z) :-
   functor(Y, vector, _),
   L is len(X),
   L =:= len(Y),
   Z is {X[I]+Y[I]|between(1, L, I)}.

/**
 * -(X, Y, Z):
 * The predicate succeeds in Z with the vector X subtracted
 * by the vector Y.
 */
% -(+Vector, +Internal, -Vector)
:- override (-)/3.
:- public (-)/3.
-(X, Y, Z) :-
   functor(Y, vector, _),
   L is len(X),
   L =:= len(Y),
   Z is {X[I]-Y[I]|between(1, L, I)}.

/***********************************************************/
/* CAS BindCount[] Hook                                        */
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
   functor(F, vector, _), !,
   F =.. [_|H],
   sys_portray_vector(H, G).

% sys_portray_vector(+List, -List)
:- private sys_portray_vector/2.
sys_portray_vector([X|L], [Y|R]) :-
   printable(X, Y),
   sys_portray_vector(L, R).
sys_portray_vector([], []).

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
   functor(E, vector, _), !,
   X = E.

:- multifile generic:is_abnormal/1.
:- public generic:is_abnormal/1.
generic:is_abnormal(E) :-
   functor(E, vector, _).
