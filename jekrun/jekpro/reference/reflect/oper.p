/**
 * A knowledge base keeps a list of zero, one or more syntax operators. The
 * syntax operators define how Prolog terms are read and written. The
 * interpreter provides the classical access to operators by the predicate
 * op/3 and current_op/3. These predicates take respectively deliver an
 * operator level, an operator mode and an operator name. If an operator
 * has associativity, it is possible to use the operator multiple times
 * in the same expression without parenthesis. The following operator icons
 * are supported. See the API documentation for a table with the operator
 * modes.
 *
 * Example:
 * ?- [user].
 * :- op(200, xfy, ++).
 * append(nil, X, X).
 * append(X++Y, Z, X++T) :- append(Y, Z, T).
 * ^D
 *
 * ?- append(X, Y, a++b++c++nil).
 * X = nil,
 * Y = a++b++c++nil ;
 * X = a++nil,
 * Y = b++c++il
 *
 * In the example above we have defined an infix operator (++)/2 with right
 * associativity. Jekejeke Prolog provides further properties of
 * individual operators. The access of the properties is based on an
 * operator indicator which is one of the terms prefix(O), postfix(O)
 * or infix(O) where O is the operator name. The user operator indicators
 * can be enumerated via the predicate current_oper/1. The operator
 * properties can be accessed and modified via the predicates oper_property/2,
 * set_oper_property/2 and reset_oper_property/2.
 *
 * oper    --> "prefix(" name ")"
 *           | "postfix(" name ")"
 *           | "infix(" name ")".
 *
 * name    --> module ":" name
 *           | atom.
 *
 * A first set of operator properties deals with the visibility of the
 * operator. These are the properties system/0, full_name/1 and private/0.
 * Pretty printing is done by controlling the indentation of operators
 * and the spaces around an operator. Pretty printing is only in effect
 * for terms aka clauses and goals. Arguments are printed in minimizing
 * the number of spaces. Pretty printing is inferred from the meta-predicate
 * declaration and the operator level.
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

:- callable_property(here, sys_context(C)),
   set_source_property(C, use_package(foreign(jekpro/reference/reflect))).
:- callable_property(here, sys_context(C)),
   reset_source_property(C, sys_source_visible(public)).

/**
 * op(L, M, N): [ISO 8.14.3]
 * op(L, M, [N1, …, Nn]): [ISO 8.14.3]
 * For L <> 0 the predicate (re-)defines the operator N with mode M
 * and level L. For L = 0 the predicate undefines the operator N
 * with mode M.
 */
% op(+Integer, +Atom, +List)
op(_, _, Z) :- var(Z), throw(error(instantiation_error, _)).
op(_, _, []) :- !.
op(L, M, [Z|T]) :- !, sys_oper(L, M, Z), op(L, M, T).
op(L, M, Z) :- sys_oper(L, M, Z).
:- set_predicate_property(op/3, visible(public)).

% sys_oper(+Integer, +Atom, +Atom) :-
sys_oper(_, M, _) :- var(M), !,
   throw(error(instantiation_error, _)).
sys_oper(_, _, Z) :- var(Z), !,
   throw(error(instantiation_error, _)).
sys_oper(L, M, Z) :-
   sys_make_oper(M, Z, I), !,
   sys_oper2(I, L, M).
sys_oper(_, M, _) :-
   throw(error(domain_error(operator_specifier, M), _)).
:- set_predicate_property(sys_oper/3, visible(private)).

% sys_oper2(+Indicator, +Integer, +Atom)
sys_oper2(I, 0, _) :- !,
   abolish(I).
sys_oper2(infix(X), _, _) :-
   current_oper(postfix(X)),
   throw(error(permission_error(create, operator, infix(X)), _)).
sys_oper2(postfix(X), _, _) :-
   current_oper(infix(X)),
   throw(error(permission_error(create, operator, postfix(X)), _)).
sys_oper2(I, L, M) :-
   sys_neutral_oper(I),
   set_oper_property(I, op(L, M)),
   sys_oper3(I, L),
   sys_oper4(I, L),
   sys_oper5(I, L).
:- set_predicate_property(sys_oper2/3, visible(private)).

% sys_oper3(+Indicator, +Integer)
sys_oper3(I, L) :- >(L, 699), !,
   reset_oper_property(I, sys_nspl),
   reset_oper_property(I, sys_nspr).
sys_oper3(prefix(X), _) :-
   reset_oper_property(prefix(X), sys_nspl),
   set_oper_property(prefix(X), sys_nspr).
sys_oper3(infix(X), _) :-
   set_oper_property(infix(X), sys_nspl),
   set_oper_property(infix(X), sys_nspr).
sys_oper3(postfix(X), _) :-
   set_oper_property(postfix(X), sys_nspl),
   reset_oper_property(postfix(X), sys_nspr).
:- set_predicate_property(sys_oper3/2, visible(private)).

% sys_oper4(+Indicator, +Integer)
sys_oper4(infix(X), L) :- >(L, 1149), !,
   set_oper_property(infix(X), sys_newr).
sys_oper4(infix(X), L) :- >(L, 1049), !,
   reset_oper_property(infix(X), sys_newr).
sys_oper4(infix(X), L) :- >(L, 949), !,
   set_oper_property(infix(X), sys_newr).
sys_oper4(I, _) :-
   reset_oper_property(I, sys_newr).
:- set_predicate_property(sys_oper4/2, visible(private)).

% sys_oper5(+Indicator, +Integer)
sys_oper5(infix(X), L) :- >(L, 1049), !,
   set_oper_property(infix(X), sys_tabr).
sys_oper5(I, _) :-
   reset_oper_property(I, sys_tabr).
:- set_predicate_property(sys_oper5/2, visible(private)).

/**
 * sys_neutral_oper(I):
 * If no syntax operator has yet been defined for the syntax operator
 * indicator I, defines a corresponding neutral syntax operator.
 */
% sys_neutral_oper(+Indicator)
% already defined in Branch
% :- special(sys_neutral_oper/1, 'SpecialOper', 0).
% :- set_predicate_property(sys_neutral_oper/1, visible(public)).

/**
 * current_op(L, M, O): [ISO 8.14.4]
 * The predicate succeeds for every  operator O with mode M and level L.
 */
% current_op(+Level, +Mode, -Pattern)
current_op(L, M, C) :- var(C), !,
   current_oper(I),
   oper_property(I, op(L, M)),
   sys_make_oper(M, C, I).
current_op(L, M, C) :- var(M), !,
   current_oper(I),
   oper_property(I, op(L, M)),
   sys_make_oper(M, C, I).
current_op(L, M, C) :-
   sys_make_oper(M, C, I),
   oper_property(I, op(L, M)).
:- set_predicate_property(current_op/3, visible(public)).

% sys_make_oper(+Atom, +Atom, -Indicator)
sys_make_oper(xf, N, postfix(N)).
sys_make_oper(yf, N, postfix(N)).
sys_make_oper(fx, N, prefix(N)).
sys_make_oper(fy, N, prefix(N)).
sys_make_oper(xfx, N, infix(N)).
sys_make_oper(xfy, N, infix(N)).
sys_make_oper(yfx, N, infix(N)).
:- set_predicate_property(sys_make_oper/3, visible(public)).

/**
 * current_oper(I):
 * The predicate succeeds for each user operator I.
 */
% current_oper(-Indicator)
current_oper(I) :- ground(I), !,
   sys_current_oper_chk(I).
current_oper(I) :-
   sys_current_oper(L),
   sys_member(I, L).
:- set_predicate_property(current_oper/1, visible(public)).

:- special(sys_current_oper/1, 'SpecialOper', 2).
:- set_predicate_property(sys_current_oper/1, visible(private)).

:- special(sys_current_oper_chk/1, 'SpecialOper', 3).
:- set_predicate_property(sys_current_oper_chk/1, visible(private)).

/**
 * oper_property(I, P):
 * The predicate succeeds for each property P of each user operator I. The
 * following operator properties are supported. For a list of properties
 * see the API documentation.
 */
% oper_property(+-Indicator, -+Property)
oper_property(I, R) :- ground(I), !,
   sys_oper_property2(I, R).
oper_property(I, R) :- var(R), !,
   sys_current_oper(L),
   sys_member(I, L),
   sys_oper_property(I, P),
   sys_member(R, P).
oper_property(I, R) :-
   sys_oper_property_idx(R, P),
   sys_member(I, P).
:- set_predicate_property(oper_property/2, visible(public)).

% sys_oper_property2(+Indicator, -Property)
sys_oper_property2(I, R) :- var(R), !,
   sys_oper_property(I, P),
   sys_member(R, P).
sys_oper_property2(I, R) :-
   functor(R, F, A),
   sys_oper_property_chk(I, F/A, P),
   sys_member(R, P).
:- set_predicate_property(sys_oper_property2/2, visible(private)).

:- special(sys_oper_property/2, 'SpecialOper', 4).
:- set_predicate_property(sys_oper_property/2, visible(private)).

:- special(sys_oper_property_chk/3, 'SpecialOper', 5).
:- set_predicate_property(sys_oper_property_chk/3, visible(private)).

:- special(sys_oper_property_idx/2, 'SpecialOper', 6).
:- set_predicate_property(sys_oper_property_idx/2, visible(private)).

/**
 * set_oper_property(I, P):
 * The predicate assigns the property P to the operator I.
 */
% set_oper_property(+Indicator, +Property)
% already defined in special.p
% :- special(set_oper_property/2, 'SpecialOper', 7).
% :- set_predicate_property(set_oper_property/2, visible(public)).

/**
 * reset_oper_property(I, P):
 * The predicate de-assigns the property P from the operator I.
 */
% reset_oper_property(+Indicator, +Property)
:- special(reset_oper_property/2, 'SpecialOper', 8).
:- set_predicate_property(reset_oper_property/2, visible(public)).

% first defined in special.p
% sys_declaration_indicator(+Declaration, -Indicator).
:- sys_neutral_predicate(sys_declaration_indicator/2).
:- set_predicate_property(sys_declaration_indicator/2, visible(public)).
:- callable_property(here, sys_context(C)),
   set_predicate_property(sys_declaration_indicator/2, sys_public(C)).
:- set_predicate_property(sys_declaration_indicator/2, multifile).
:- callable_property(here, sys_context(C)),
   set_predicate_property(sys_declaration_indicator/2, sys_multifile(C)).
sys_declaration_indicator(op(_, M, _), _) :-
   var(M), throw(error(instantiation_error, _)).
sys_declaration_indicator(op(_, _, Z), _) :-
   var(Z), throw(error(instantiation_error, _)).
sys_declaration_indicator(op(_, M, Z), I) :-
   sys_make_oper(M, Z, I).

/**********************************************************/
/* Moved From Debugger                                    */
/**********************************************************/

% moved from syntax.p in debugger
:- special(sys_syntax_property_idx/2, 'SpecialOper', 9).
:- set_predicate_property(sys_syntax_property_idx/2, visible(public)).

% moved from syntax.p in debugger
:- special(sys_syntax_property_chk/3, 'SpecialOper', 10).
:- set_predicate_property(sys_syntax_property_chk/3, visible(public)).

/****************************************************************/
/* Operators Listing                                            */
/****************************************************************/

% sys_show_operators(+Operator, +Source)
:- special(sys_show_operators/2, 'SpecialOper', 11).
:- set_predicate_property(sys_show_operators/2, visible(public)).
