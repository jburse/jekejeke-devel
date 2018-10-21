/**
 * The interface of special built-ins is currently not published.
 * They are needed when low-level behaviour is desired coupled
 * with high performance, which could not be gained by using a
 * foreign Java predicate or Prolog clauses. It is possible to
 * register predicates and evaluable functions as special built-ins
 * via the predicates special/3 and special_eval/3. A predicate
 * indicator, the service class and the service number has
 * to be specified.
 *
 * directive --> "special(" indicator "," module "," integer ")".
 *
 * static_flag --> "true" | "false".
 *
 * Example:
 * :- special(foo/1, 'FooAPI', 7). % is a special predicate directive.
 *
 * An evaluable function f/n is identified by a predicate indicator f/n+1,
 * meaning the arity has to be increased by one. To ease the end-user
 * the Jekejeke Prolog system automatically implements for each predicate
 * bridging to an evaluable function, and for each evaluable function
 * tunnelling to a predicate.
 *
 * Bridging, predicate as evaluable function:
 * X is p(Y1,..,Yn) :-
 *    Z1 is Y1, .., Zn is Yn,
 *    p(Z1, .., Zn, X), !, value(X).
 * _ is p(_,..,_):-
 *    throw(error(evaluation_error(partial_function),_)).
 *
 * Tunnelling, evaluable function as predicate:
 * f(Y1, .., Yn, X) :-
 *    value(Y1), .., value(Yn),
 *    X is f(Y1,..,Yn).
 *
 * During bridging the arguments are evaluated and then the corresponding
 * predicate is called with an additional last argument for the result.
 * If the corresponding predicate succeeds its choice points are removed.
 * If the corresponding predicate fails or if the result is not a value
 * an error is issued. During tunnelling the arguments are first checked.
 * If an argument is not a value an error is issued. Otherwise the
 * corresponding evaluable function is called.
 *
 * The bridging and tunnelling is further controlled by the virtual property
 * of a predicate. If a predicate has this property the first argument Y1
 * gets special treatment. During bridging this argument will not be
 * evaluated and during tunnelling this argument will not be checked.
 * This is useful for predicates that pass as the first argument the
 * receiver object. The directive virtual/1 can be used to set the virtual
 * property of a predicate.
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

:- sys_context_property(here, C),
   set_source_property(C, use_package(foreign(jekpro/model/builtin))).
:- sys_context_property(here, C),
   reset_source_property(C, sys_source_visible(public)).

:- sys_op(1200, fx, :-).
:- set_oper_property(prefix(:-), visible(public)).
:- sys_op(1200, xfx, :-).
:- set_oper_property(infix(:-), visible(public)).
:- sys_op(1000, xfy, ',').
:- set_oper_property(infix(','), visible(public)).
:- set_oper_property(infix(','), nspl).

:- sys_op(400, yfx, /).
:- set_oper_property(infix(/), visible(public)).

:- sys_op(1150, fy, virtual).
:- set_oper_property(prefix(virtual), visible(public)).

/**
 * special(I, C, K):
 * Succeeds with registering the predicate indicator I as a special
 * builtin that calls an instance of the service class C with
 * function index K.
 */
% special(+IndicatorColon, +Class, +Index)
special(I, C, K) :-
   sys_special(I, C, K),
   sys_check_style_predicate(I).

:- special(set_predicate_property/2, 'SpecialSpecial', 0).
:- set_predicate_property(set_predicate_property/2, visible(public)).

:- set_predicate_property(special/3, visible(public)).

:- special(reset_predicate_property/2, 'SpecialSpecial', 1).
:- set_predicate_property(reset_predicate_property/2, visible(public)).

:- reset_predicate_property(sys_special/3, visible(public)).
:- set_predicate_property(sys_special/3, visible(private)).

/**
 * virtual P, ..:
 * The predicate sets the predicate P to virtual.
 */
% virtual +Indicators
virtual [P|Q] :- !,
   sys_virtual(P),
   (virtual Q).
virtual P,Q :- !,
   sys_virtual(P),
   (virtual Q).
virtual [] :- !.
virtual P :-
   sys_virtual(P).
:- set_predicate_property((virtual)/1, visible(public)).

sys_virtual(X) :-
   var(X),
   throw(error(instantiation_error,_)).
sys_virtual(D) :-
   sys_declaration_indicator(D, I), !,
   sys_virtual(I),
   call(D).
sys_virtual(I) :-
   sys_neutral_predicate(I),
   set_predicate_property(I, virtual).
:- set_predicate_property(sys_virtual/1, visible(private)).

/**
 * sys_neutral_predicate(I):
 * If no predicate has yet been defined for the predicate indicator I,
 * defines a corresponding neutral predicate.
 */
% sys_neutral_predicate(+Indicator)
:- special(sys_neutral_predicate/1, 'SpecialSpecial', 5).
:- set_predicate_property(sys_neutral_predicate/1, visible(public)).

/**
 * sys_declaration_indicator(D, I):
 * The predicate succeeds with the indicator I for the declaration D.
 * The predicate is multifile and can be extended by further clauses.
 */
% sys_declaration_indicator(+Declaration, -Indicator).
:- sys_neutral_predicate(sys_declaration_indicator/2).
:- set_predicate_property(sys_declaration_indicator/2, visible(public)).
:- sys_context_property(here, C),
   set_predicate_property(sys_declaration_indicator/2, sys_public(C)).
:- set_predicate_property(sys_declaration_indicator/2, multifile).
:- sys_context_property(here, C),
   set_predicate_property(sys_declaration_indicator/2, sys_multifile(C)).

sys_declaration_indicator(special(I,_,_), I).
sys_declaration_indicator(set_predicate_property(I,_), I).
sys_declaration_indicator(reset_predicate_property(I,_), I).
sys_declaration_indicator((virtual D), I) :-
   sys_declaration_indicator(D, I).
