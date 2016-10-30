/**
 * Predicates, evaluable functions and syntax operators can be
 * protected from external access. A Prolog text member can be
 * either public, package local or private. Private members can
 * only be seen from within the same Prolog text. Package local
 * members will be seen from Prolog texts that share the same
 * package name. And public members are visible everywhere. The
 * default visibility for a member is package local inside
 * modules and public inside Prolog texts.
 *
 * Example:
 * ?- member(X,[1,2,3]).
 * X = 1 ;
 * X = 2 ;
 * X = 3
 * ?- member2([2,3],X,1).
 * Error: Undefined, private or package local predicate member2/3.
 *
 * Normal Prolog texts can be turned into modules via the predicate
 * module/2. For a nameless module the name user can be used. A
 * further convenience is the predicate use_module/1 which does
 * an ensure loaded of the given file specification. Instead of
 * the predicate use_module/1 also the predicate reexport/1 can
 * be used. The later predicate will make the corresponding imported
 * members visible to qualified invocations and client imports.
 *
 * Example:
 * ?- source_property(system, use_package(X)).
 * X = foreign(java/lang) ;
 * Etc..
 *
 * ?- source_property(system, use_file_extension(X)).
 * X = binary('.class') ;
 * X = text('.p') ;
 * Etc..
 *
 * The path resolution uses prefixes and suffixes from the current
 * source and the system source. The prefixes for the current source
 * can be set via the predicate use_package/1 and the suffixes for
 * the current source can be set via the predicate use_file_extension/1.
 * The prefixes and suffixes can be queried via the usual source_property/2
 * predicate.
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

:- set_source_property(system, use_file_extension(text('.px'))).
:- set_source_property(system, use_file_extension(text('.p'))).
:- set_source_property(system, use_file_extension(text('.pl'))).
:- set_source_property(system, use_file_extension(text('.pro'))).
:- set_source_property(system, use_file_extension(resource('.propertiesx'))).
:- set_source_property(system, use_file_extension(resource('.properties'))).

:- set_source_property(system, use_package(library(jekpro/frequent))).
:- set_source_property(system, use_package(library(jekpro/reference))).
:- set_source_property(system, use_package(library(jekpro/platform))).

:- sys_get_context(here, C),
   reset_source_property(C, sys_source_visible(public)).

:- op(1150, fy, private).
:- set_oper_property(prefix(private), visible(public)).

:- op(1150, fy, public).
:- set_oper_property(prefix(public), visible(public)).

/*************************************************************/
/* Source Properties                                         */
/*************************************************************/

/**
 * package(P):
 * The predicate adds the prefix P to the list of prefixes of the
 * current source. Currently library/1 and foreign/1 prefixes are
 * supported. The prefix is also used as a prefix to the module/2
 * directive.
 */
% package(+Callable)
package(P) :-
   var(P),
   throw(error(instantiation_error,_)).
package(library(P)) :- !,
   sys_get_context(P, C),
   set_source_property(C, package(library(P))).
package(foreign(P)) :- !,
   sys_get_context(P, C),
   set_source_property(C, package(foreign(P))).
package(P) :-
   throw(error(domain_error(fix_option,P),_)).
:- set_predicate_property(package/1, visible(public)).

/**
 * use_package(P):
 * The predicate adds the prefix P to the list of prefixes of the
 * current source. Currently library/1 and foreign/1 prefixes
 * are supported. The prefix is not used as a prefix to the module/2
 * directive.
 */
% use_package(+Callable)
use_package(P) :-
   var(P),
   throw(error(instantiation_error,_)).
use_package(library(P)) :- !,
   sys_get_context(P, C),
   set_source_property(C, use_package(library(P))).
use_package(foreign(P)) :- !,
   sys_get_context(P, C),
   set_source_property(C, use_package(foreign(P))).
use_package(P) :-
   throw(error(domain_error(fix_option,P),_)).
:- set_predicate_property(use_package/1, visible(public)).

/**
 * use_file_extension(E):
 * The predicate adds the suffix E to the list of suffixes of
 * the current source. Currently text/1, binary/1 and resources/1
 * suffixes are supported.
 */
% use_file_extension(+Callable)
use_file_extension(E) :-
   var(E),
   throw(error(instantiation_error,_)).
use_file_extension(text(E)) :- !,
   sys_get_context(E, C),
   set_source_property(C, use_file_extension(text(E))).
use_file_extension(binary(E)) :- !,
   sys_get_context(E, C),
   set_source_property(C, use_file_extension(binary(E))).
use_file_extension(resource(E)) :- !,
   sys_get_context(E, C),
   set_source_property(C, use_file_extension(resource(E))).
use_file_extension(E) :-
   throw(error(domain_error(fix_option,E),_)).
:- set_predicate_property(use_file_extension/1, visible(public)).

/**
 * module(N, L):
 * The predicate is a convenience for a combination of setting
 * the module name to N, setting the source to private and
 * setting the syntax operators and predicates L to public.
 */
% module(+Atom, +Indicators)
module(N, _) :-
   var(N),
   throw(error(instantiation_error,_)).
module(N, L) :-
   sys_eq(N, user), !,
   sys_get_context(N, C),
   reset_source_property(C, sys_source_visible(public)),
   (public L).
module(N, L) :-
   sys_get_context(N, C),
   reset_source_property(C, sys_source_visible(public)),
   set_source_property(C, sys_source_name(N)),
   (public L).
:- set_predicate_property(module/2, visible(public)).

/*************************************************************/
/* Import Properties                                         */
/*************************************************************/

/**
 * use_module(R):
 * The predicate imports the read path R with making its predicates,
 * evaluable functions and syntax operators visible and without exporting them.
 */
% use_module(+Atom)
use_module(Path) :-
   absolute_file_name(Path, Pin),
   sys_load_file(Pin, [condition(on),verbose(summary),sys_link(use_module)]).
:- set_predicate_property(use_module/1, visible(public)).

/**
 * reexport(R):
 * The predicate imports the read path R with making its predicates,
 * evaluable functions and syntax operators visible and with exporting them.
 */
% reexport(+Atom)
reexport(Path) :-
   absolute_file_name(Path, Pin),
   sys_load_file(Pin, [condition(on),verbose(summary),sys_link(reexport)]).
:- set_predicate_property(reexport/1, visible(public)).

/**
 * sys_auto_load(R):
 * The predicate imports the read path R with neither making its predicates,
 * evaluable functions and syntax operators visible and nor exporting them.
 */
% sys_auto_load(+Atom)
sys_auto_load(Path) :-
   absolute_file_name(Path, Pin),
   sys_load_file(Pin, [condition(on),verbose(off),sys_link(sys_auto_load)]).
:- set_predicate_property(sys_auto_load/1, visible(public)).

/**
 * sys_load_resource(R):
 * The predicate imports the read path R trying resolve it to a resource
 * bundle instead of a Prolog text.
 */
% sys_load_resource(+Atom)
sys_load_resource(Path) :-
   absolute_resource_name(Path, Pin),
   sys_load_file(Pin, [condition(on),verbose(summary),sys_link(sys_load_resource)]).
:- set_predicate_property(sys_load_resource/1, visible(public)).

/**
 * sys_add_resource(R):
 * Add the read path R to the list of error resources.
 */
% sys_add_resource(+Atom)
sys_add_resource(Path) :-
   absolute_resource_name(Path, Pin),
   sys_register_file(Pin).
:- set_predicate_property(sys_add_resource/1, visible(public)).

/*************************************************************/
/* Predicate Properties                                      */
/*************************************************************/

/**
 * private P, ..:
 * The predicate sets the operator, evaluable or predicate P to private.
 */
% private +Indicators
private [P|Q] :- !,
   sys_private(P),
   (private Q).
private P,Q :- !,
   sys_private(P),
   (private Q).
private [] :- !.
private P :-
   sys_private(P).
:- set_predicate_property((private)/1, visible(public)).

% sys_private(+IndicatorOrOperator)
sys_private(X) :-
   var(X),
   throw(error(instantiation_error,_)).
sys_private(D) :-
   sys_declaration_indicator(D, I), !,
   sys_private(I),
   call(D).
sys_private(prefix(X)) :- !,
   sys_neutral_oper(prefix(X)),
   set_oper_property(prefix(X), visible(private)).
sys_private(infix(X)) :- !,
   sys_neutral_oper(infix(X)),
   set_oper_property(infix(X), visible(private)).
sys_private(postfix(X)) :- !,
   sys_neutral_oper(postfix(X)),
   set_oper_property(postfix(X), visible(private)).
sys_private(I) :-
   sys_neutral_predicate(I),
   set_predicate_property(I, visible(private)).
:- set_predicate_property(sys_private/1, visible(private)).

/**
 * public P, ..:
 * The predicate sets the operator, evaluable or predicate P to public.
 */
% public +Indicators
public [P|Q] :- !,
   sys_public(P),
   (public Q).
public P,Q :- !,
   sys_public(P),
   (public Q).
public [] :- !.
public P :-
   sys_public(P).
:- set_predicate_property((public)/1, visible(public)).

% sys_public(+IndicatorOrOperator)
sys_public(X) :-
   var(X),
   throw(error(instantiation_error,_)).
sys_public(D) :-
   sys_declaration_indicator(D, I), !,
   sys_public(I),
   call(D).
sys_public(prefix(X)) :- !,
   sys_neutral_oper(prefix(X)),
   set_oper_property(prefix(X), visible(public)).
sys_public(infix(X)) :- !,
   sys_neutral_oper(infix(X)),
   set_oper_property(infix(X), visible(public)).
sys_public(postfix(X)) :- !,
   sys_neutral_oper(postfix(X)),
   set_oper_property(postfix(X), visible(public)).
sys_public(I) :-
   sys_make_indicator(F, _, I),
   sys_get_context(F, C),
   sys_once(sys_and(predicate_property(I,sys_usage(D)),
               sys_not(sys_eq(C,D)))),
   sys_not(predicate_property(I,sys_public(D))),
   throw(error(permission_error(promote,public,I),_)).
sys_public(I) :-
   sys_neutral_predicate(I),
   set_predicate_property(I, visible(public)),
   sys_make_indicator(F, _, I),
   sys_get_context(F, C),
   set_predicate_property(I, sys_accessible_public(C)).
:- set_predicate_property(sys_public/1, visible(private)).

/**
 * override I, ...:
 * The predicate sets the predicate indicator I to override.
 */
% override(+Indicators)
override [P|Q] :- !,
   sys_override(P),
   (override Q).
override P,Q :- !,
   sys_override(P),
   (override Q).
override [] :- !.
override P :-
   sys_override(P).
:- set_predicate_property((override)/1, visible(public)).

% sys_override(+Indicator)
sys_override(V) :-
   var(V),
   throw(error(instantiation_error,_)).
sys_override(D) :-
   sys_declaration_indicator(D, I), !,
   sys_override(I),
   call(D).
sys_override(prefix(X)) :- !,
   sys_neutral_oper(prefix(X)),
   set_oper_property(prefix(X), override).
sys_override(infix(X)) :- !,
   sys_neutral_oper(infix(X)),
   set_oper_property(infix(X), override).
sys_override(postfix(X)) :- !,
   sys_neutral_oper(postfix(X)),
   set_oper_property(postfix(X), override).
sys_override(I) :-
   sys_neutral_predicate(I),
   sys_make_indicator(J, _, I),
   sys_get_context(J, C),
   set_predicate_property(I, (override C)).
:- set_predicate_property(sys_override/1, visible(private)).

% first defined in special.p
% sys_declaration_indicator(+Declaration, -Indicator).
:- sys_neutral_predicate(sys_declaration_indicator/2).
:- set_predicate_property(sys_declaration_indicator/2, visible(public)).
:- sys_get_context(here, C),
   set_predicate_property(sys_declaration_indicator/2, sys_accessible_public(C)).
:- set_predicate_property(sys_declaration_indicator/2, multifile).
:- sys_get_context(here, C),
   set_predicate_property(sys_declaration_indicator/2, sys_accessible_multifile(C)).
sys_declaration_indicator((public D), I) :-
   sys_declaration_indicator(D, I).
sys_declaration_indicator((private D), I) :-
   sys_declaration_indicator(D, I).
sys_declaration_indicator((override D), I) :-
   sys_declaration_indicator(D, I).

/********************************************************/
/* Load Rest                                            */
/********************************************************/

:- ensure_loaded(library(bootload/engine)).
:- ensure_loaded(library(bootload/toolkit)).

:- ensure_loaded(library(arithmetic/eval)).
:- ensure_loaded(library(arithmetic/elem)).
:- ensure_loaded(library(arithmetic/round)).
:- ensure_loaded(library(arithmetic/bits)).
:- ensure_loaded(library(arithmetic/trigo)).
:- ensure_loaded(library(arithmetic/compare)).

:- ensure_loaded(library(structure/type)).
:- ensure_loaded(library(structure/vars)).
:- ensure_loaded(library(structure/lexical)).
:- ensure_loaded(library(structure/univ)).
:- ensure_loaded(library(structure/atom)).
:- ensure_loaded(library(structure/sort)).

:- ensure_loaded(library(runtime/meta)).
:- ensure_loaded(library(runtime/quali)).
:- ensure_loaded(library(runtime/logic)).
:- ensure_loaded(library(runtime/dynamic)).
:- ensure_loaded(library(runtime/session)).

:- ensure_loaded(library(standard/apply)).
:- ensure_loaded(library(standard/bags)).
:- ensure_loaded(library(standard/expand)).
:- ensure_loaded(library(standard/dcg)).
:- ensure_loaded(library(standard/signal)).

:- ensure_loaded(library(stream/stream)).
:- ensure_loaded(library(stream/byte)).
:- ensure_loaded(library(stream/char)).
:- ensure_loaded(library(stream/term)).

:- ensure_loaded(library(basic/proxy)).
:- ensure_loaded(library(basic/array)).

:- set_prolog_flag(sys_clause_expand, on).