/**
 * Foreign predicates can be defined by Java methods, constructors
 * and fields. Foreign predicates will have retrieved the actual
 * goal arguments by the interpreter and automatically passed to
 * the associated Java method, constructor or field. More details
 * about foreign predicates can be found in the Jekejeke Prolog
 * Programming Interface documentation.
 *
 * Foreign predicates can be registered by one of the directives
 * foreign/3, foreign_constructor/3, foreign_setter/3 and foreign_getter/3.
 * Foreign evaluable functions can be registered by one of the directives
 * foreign_fun/3 or foreign_const/3. The directives take as
 * arguments a predicate specification, a declaring class specification
 * and a method, constructor or field specification. We can describe
 * the arguments via the following syntax:
 *
 * directive --> "foreign(" indicator "," module "," signature ")"
 *            | "foreign_constructor(" indicator "," module "," signature ")"
 *            | "foreign_setter(" indicator "," module "," atom ")"
 *            | "foreign_getter(" indicator "," module "," atom ")"
 *            | "foreign_fun(" indicator "," module "," signature ")"
 *            | "foreign_const(" indicator "," module "," atom ")".
 *
 * signature --> atom [ "(" module { "," module } ")" ].
 *
 * Example:
 * :- foreign(hello/0, 'OutHello', hello('Interpreter')).
 *                       % is a foreign predicate directive.
 *
 * Not all declared classes or parameter types have to be fully
 * qualified. The below class names can be directly used without
 * specifying the package name. All formal parameters not of class
 * Term cause a range check and/or conversion of the actual argument.
 * A formal parameter of type BigDecimal or BigInteger causes a
 * widening, whereas a formal parameter of type Integer or Long
 * causes a range check.
 *
 * The supported primitive datatypes are handled analogously. The
 * Java method, constructor or field might also have one of the
 + above classes as a return type. By returning a non-null object the
 * Java method, constructor or field can indicate success and the
 * interpreter will unify the object with the last argument of the
 * corresponding predicate. By returning a null the Java method or
 * field can indicate a failure. For a table of the parameter type
 * and return type mapping see the API documentation.
 *
 * A formal parameter of type Interpreter is needed for foreign predicate
 * that change variable bindings. A formal parameter of type CallOut is
 * needed for non-deterministic foreign predicates. The API of the CallOut
 * allows fine control of the creation of choice points, of the choice
 * point data, of clean-up handling and barrier handling. For more
 * information one should consult the programming interface
 * documentation.
 *
 * A Java method might also have a boolean or a void return type. The
 * return type boolean can indicate success or failure without returning
 * an object. The return type void always indicates success without
 * returning an object. For non-static methods or fields an additional
 * argument for the receiving object is added to the front of the
 * foreign predicate.
 *
 * The interpreter will catch InterpreterMessage exceptions thrown by
 * a foreign predicate, determine the Prolog stack trace and throw a
 * corresponding InterpreterException. The interpreter will further also
 * allow InterpreterException exceptions directly thrown by the foreign
 * predicate itself. As a convenience the interpreter also recognizes a
 * couple of Java exceptions and wraps them into Prolog errors before
 * throwing. The family of interrupted exceptions is mapped to the signal
 * currently stored in the interpreter and the signal is cleared. For a
 * table of the exception type mapping see the API documentation.
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

:- set_source_property(system, use_package(foreign(java/lang))).

:- sys_context_property(here, C),
   set_source_property(C, use_package(foreign(jekpro/reference/reflect))).
:- sys_context_property(here, C),
   reset_source_property(C, sys_source_visible(public)).

/***************************************************************/
/* Foreign Predicates                                          */
/***************************************************************/

/**
 * foreign(I, C, M):
 * Succeeds with registering the predicate indicator I as a foreign
 * predicate that calls the method M of the class C.
 */
% foreign(+IndicatorColon, +Class, +Signature)
foreign(I, C, M) :-
   sys_foreign(I, C, M),
   sys_check_style_predicate(I).
:- set_predicate_property(foreign/3, visible(public)).

:- special(sys_foreign/3, 'SpecialForeign', 0).
:- set_predicate_property(sys_foreign/3, visible(private)).

/**
 * foreign_constructor(I, C, M):
 * Succeeds with registering the predicate indicator I as a foreign
 * predicate that calls the constructor M of the class C.
 */
% foreign_constructor(+IndicatorColon, +Class, +Signature)
foreign_constructor(I, C, M) :-
   sys_foreign_constructor(I, C, M),
   sys_check_style_predicate(I).
:- set_predicate_property(foreign_constructor/3, visible(public)).

:- special(sys_foreign_constructor/3, 'SpecialForeign', 1).
:- set_predicate_property(sys_foreign_constructor/3, visible(private)).

/**
 * foreign_getter(I, C, M):
 * Succeeds with registering the predicate indicator I as a foreign
 * predicate that gets the field M of the class C.
 */
% foreign_getter(+IndicatorColon, +Class, +Name)
foreign_getter(I, C, M) :-
   sys_foreign_getter(I, C, M),
   sys_check_style_predicate(I).
:- set_predicate_property(foreign_getter/3, visible(public)).

:- special(sys_foreign_getter/3, 'SpecialForeign', 2).
:- set_predicate_property(sys_foreign_getter/3, visible(private)).

/**
 * foreign_setter(I, C, M):
 * Succeeds with registering the predicate indicator I as a foreign
 * predicate that sets the field M of the class C.
 */
% foreign_setter(+IndicatorColon, +Class, +Name)
foreign_setter(I, C, M) :-
   sys_foreign_setter(I, C, M),
   sys_check_style_predicate(I).
:- set_predicate_property(foreign_setter/3, visible(public)).

:- special(sys_foreign_setter/3, 'SpecialForeign', 3).
:- set_predicate_property(sys_foreign_setter/3, visible(private)).

/***************************************************************/
/* Foreign Evaluable Functions                                 */
/***************************************************************/

/**
 * foreign_fun(I, C, M):
 * Succeeds with registering the evaluable indicator I as a foreign
 * evaluable function that calls the method M of the class C.
 */
% foreign_fun(+IndicatorColon, +Class, +Signature)
foreign_fun(I, C, M) :-
   sys_foreign_fun(I, C, M),
   sys_check_style_predicate(I).
:- set_predicate_property(foreign_fun/3, visible(public)).

:- special(sys_foreign_fun/3, 'SpecialForeign', 4).
:- set_predicate_property(sys_foreign_fun/3, visible(private)).

/**
 * foreign_const(I, C, M):
 * Succeeds with registering the evaluable indicator I as a foreign
 * evaluable function that gets the field M of the class C.
 */
% foreign_const(+IndicatorColon, +Class, +Name)
foreign_const(I, C, M) :-
   sys_foreign_const(I, C, M),
   sys_check_style_predicate(I).
:- set_predicate_property(foreign_const/3, visible(public)).

:- special(sys_foreign_const/3, 'SpecialForeign', 5).
:- set_predicate_property(sys_foreign_const/3, visible(private)).

% first defined in special.p
% sys_declaration_indicator(+Declaration, -Indicator).
:- sys_neutral_predicate(sys_declaration_indicator/2).
:- set_predicate_property(sys_declaration_indicator/2, visible(public)).
:- sys_context_property(here, C),
   set_predicate_property(sys_declaration_indicator/2, sys_public(C)).
:- set_predicate_property(sys_declaration_indicator/2, multifile).
:- sys_context_property(here, C),
   set_predicate_property(sys_declaration_indicator/2, sys_multifile(C)).
sys_declaration_indicator(foreign(I,_,_), I).
sys_declaration_indicator(foreign_constructor(I,_,_), I).
sys_declaration_indicator(foreign_getter(I,_,_), I).
sys_declaration_indicator(foreign_setter(I,_,_), I).
sys_declaration_indicator(foreign_fun(I,_,_), I).
sys_declaration_indicator(foreign_const(I,_,_), I).
