/**
 * A properties bundle consists of multiple properties files that vary
 * in the file name by an injection of a locale code before the
 * file extension. Our convention is that each bundle must contain
 * a properties file without injection. This file acts as a root for
 * the bundle and as a fall-back for locales that are not found:
 *
 * Examples:
 * code.properties:    The root and fall-back.
 * code_de.properties: The German member of the bundle.
 *
 * The predicates get_properties/[2,3] allow retrieving a locale
 * properties file of a resource bun-dle. These predicates make use
 * of the predicate absolute_resource_name/2 to resolve the root so
 * that the same base name without an extension can be used for both
 * Prolog text and resource bundles. The predicates get_property/[3,4]
 * allow retrieving a property value.
 *
 * Examples:
 * test(Y) :- get_properties('code',X), get_property(X,'foo',Y).
 * ?- test(X).
 * X = bar
 *
 * The resource bundle of a properties file has to be loaded in advanced
 * via the predicate sys_load_resource/1 and will then be cached for
 * faster subsequent access. The predicates get_error_properties/[1,2]
 * allow retrieving a union of those properties files, which were
 * registered via sys_register_file/1 in advance.
 *
 * The predicate atom_format/[3,4] allows formatting a list of arguments
 * based on a template and a locale. The predicates message_make/[3,4]
 * allows formatting a term based on properties file and a locale. The
 * predicates get_descr_model/[2,3] and get_descr_platform/[2,3]
 * respective capability defined properties files.
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

:- package(library(jekpro/frequent/system)).
:- use_package(foreign(jekpro/frequent/system)).
:- use_package(foreign(java/util)).
:- use_package(foreign(jekpro/tools/call)).

:- module(locale, []).

/**
 * get_properties(S, P):
 * get_properties(S, L, P):
 * The predicate unifies P with the properties from the bundle S
 * for the current default locale. The ternary version of the
 * predicate allows specifying the locale L. The bundle S has
 * to be loaded in advance via sys_load_resource/1.
 */
% get_properties(+Slash, -Props)
:- public get_properties/2.
get_properties(Slash, Props) :-
   absolute_resource_name(Slash, Pin),
   current_prolog_flag(sys_locale, Locale),
   sys_get_properties(Pin, Locale, Props).

% get_properties(+Slash, +Atom, -Props)
:- public get_properties/3.
get_properties(Slash, Locale, Props) :-
   absolute_resource_name(Slash, Pin),
   sys_get_properties(Pin, Locale, Props).

:- private sys_get_properties/3.
:- foreign(sys_get_properties/3, 'ForeignLocale',
   sysGetLang('Interpreter', 'String', 'String')).

/**
 * get_error_properties(P):
 * get_error_properties(L, P):
 * The predicate unifies P with the error properties of the current
 * knowledge base. The binary predicate allows specifying a locale L.
 * The error resource bundles have to be loaded in advance via
 * sys_load_resource/1 and registered via sys_register_file/1.
 */
% get_error_properties(-Props)
:- public get_error_properties/1.
get_error_properties(Props) :-
   current_prolog_flag(sys_locale, Locale),
   get_error_properties(Locale, Props).

% get_error_properties(+Locale, -Props)
:- public get_error_properties/2.
:- foreign(get_error_properties/2, 'ForeignLocale',
   sysGetErrorProperties('Interpreter', 'String')).

/**
 * get_property(P, K, V):
 * get_property(P, K, D, V):
 * The predicate unifies V with the value for the key K from
 * the properties P. The quaternary version of the predicate
 * allows specifying a default value D.
 */
% get_property(+Props, +Atom, -Atom)
:- public get_property/3.
:- foreign(get_property/3, 'ForeignLocale',
   sysGetProperty('Properties', 'String')).

% get_property(+Props, +Atom, +Atom, -Atom)
:- public get_property/4.
:- foreign(get_property/4, 'ForeignLocale',
   sysGetProperty('Properties', 'String', 'String')).

/**
 * format_atom(F, A, S):
 * format_atom(L, F, A, S):
 * The predicate formats the arguments A from the format F and unifies
 * the result with S. The quaternary predicate allows specifying
 * a locale L.
 */
% format_atom(+Format, +Arguments, -Atom)
:- public format_atom/3.
format_atom(Format, Arguments, Atom) :-
   current_prolog_flag(sys_locale, Locale),
   format_atom(Locale, Format, Arguments, Atom).

% format_atom(+Locale, +Format, +List, -Atom)
:- public format_atom/4.
:- foreign(format_atom/4, 'ForeignLocale',
   sysFormatToString('Interpreter', 'String', 'String', 'Object')).

/**
 * message_make(P, M, S):
 * message_make(L, P, M, S):
 * The predicate formats the messsage term M from the properties P
 * and unifies the result with S. The quaternary predicate allows
 * specifying a locale L.
 */
% message_make(+Props, +Term, -Atom)
:- public message_make/3.
message_make(Props, Term, Atom) :-
   current_prolog_flag(sys_locale, Locale),
   message_make(Locale, Props, Term, Atom).

% message_make(+Locale, +Props, +Term, -Atom)
:- public message_make/4.
:- foreign(message_make/4, 'ForeignLocale',
   sysMessageMake('Interpreter', 'String', 'Properties', 'Object')).

/**
 * get_descr_model(C, P):
 * get_descr_model(L, C, P):
 * The predicate unifies P with the model description properties
 * of the given capability C. The ternary predicate allows
 * specifying a locale L.
 */
% get_descr_model(+Capability, -Props)
:- public get_descr_model/2.
get_descr_model(Capability, Props) :-
   current_prolog_flag(sys_locale, Locale),
   get_descr_model(Locale, Capability, Props).

% get_descr_model(+Locale, +Capability, -Props)
:- public get_descr_model/3.
:- foreign(get_descr_model/3, 'ForeignLocale',
   sysGetDescrModel('Interpreter', 'String', 'String')).

/**
 * get_descr_platform(C, P):
 * get_descr_platform(L, C, P):
 * The predicate unifies P with the platform description properties
 * of the given capability C. The ternary predicate allows
 * specifying a locale L.
 */
% get_descr_platform(+Capability, -Props)
:- public get_descr_platform/2.
get_descr_platform(Capability, Props) :-
   current_prolog_flag(sys_locale, Locale),
   get_descr_platform(Locale, Capability, Props).

% get_descr_platform(+Locale, +Capability, -Props)
:- public get_descr_platform/3.
:- foreign(get_descr_platform/3, 'ForeignLocale',
   sysGetDescrPlatform('Interpreter', 'String', 'String')).
