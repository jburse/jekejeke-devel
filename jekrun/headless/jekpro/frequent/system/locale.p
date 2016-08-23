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
 * The predicates get_lang/2 and get_lang/3 allow retrieving a
 * member of a bundle. These predicates make use of the predicate
 * absolute_resource_name/2 to resolve the root so that the same
 * base name without an extension can be used for Prolog text and
 * for properties files.
 *
 * Examples:
 * test(Y) :- get_lang('code',X), get_property(X,'foo',Y).
 * ?- test(X).
 * X = bar
 *
 * The lookup can be combined with sys_capture/1 to allow addressing
 * a properties file relative to the current Prolog text. The
 * predicates get_property/3 and get_property/4 allow retrieving a
 * property value from a properties file. The lookup and retrieval
 * is fast, since we memory load and cache properties files.
 *
 * The predicate atom_format/[3,4] allows formatting a list of arguments
 * based on a template and a locale. The predicates message_make/[3,4]
 * and error_make/[3,4] allow formatting a term based on properties
 * file and a locale. The predicates get_error_properties/[1,2] and
 * get_description_properties/[2,3] allow retrieving knowledgebase
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekpro/frequent/system)).
:- use_package(foreign(jekpro/frequent/system)).
:- use_package(foreign(java/util)).
:- use_package(foreign(jekpro/tools/call)).

:- module(locale, []).

/**
 * sys_get_lang(S, P):
 * sys_get_lang(S, L, P):
 * The predicate unifies P with the properties from the bundle S
 * for the current default locale. The ternary version of the
 * predicate allows specifying the locale L. The bundle S has
 * to be loaded in advance via sys_load_resource/1.
 */
% sys_get_lang(+Slash, -Props)
:- public sys_get_lang/2.
sys_get_lang(Slash, Props) :-
   absolute_resource_name(Slash, Pin),
   current_prolog_flag(sys_locale, Locale),
   sys_sys_get_lang(Pin, Locale, Props).

% sys_get_lang(+Slash, +Atom, -Props)
:- public sys_get_lang/3.
sys_get_lang(Slash, Locale, Props) :-
   absolute_resource_name(Slash, Pin),
   sys_sys_get_lang(Pin, Locale, Props).

:- private sys_sys_get_lang/3.
:- foreign(sys_sys_get_lang/3, 'ForeignLocale',
      sysGetLang('Interpreter','String','String')).

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
      sysGetProperty('Properties','String')).

% get_property(+Props, +Atom, +Atom, -Atom)
:- public get_property/4.
:- foreign(get_property/4, 'ForeignLocale',
      sysGetProperty('Properties','String','String')).

/**
 * atom_format(F, A, S):
 * atom_format(L, F, A, S):
 * The predicate formats the arguments A from the format F and unifies
 * the result with S. The quaternary predicate allows specifying
 * a locale L.
 */
% atom_format(+Format, +Arguments, -Atom)
:- public atom_format/3.
atom_format(Format, Arguments, Atom) :-
   current_prolog_flag(sys_locale, Locale),
   atom_format(Locale, Format, Arguments, Atom).

% atom_format(+Locale, +Format, +List, -Atom)
:- public atom_format/4.
:- foreign(atom_format/4, 'ForeignLocale',
      sysAtomFormat('Interpreter','String','String','Object')).

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
      sysMessageMake('Interpreter','String','Properties','Object')).

/**
 * error_make(P, E, S):
 * error_make(L, P, E, S):
 * The predicate formats the error term E without its context from
 * the properties P and unifies the result with S. The quaternary
 * predicate allows specifying a locale L.
 */
% error_make(+Props, +Term, -Atom)
:- public error_make/3.
error_make(Props, Term, Atom) :-
   current_prolog_flag(sys_locale, Locale),
   error_make(Locale, Props, Term, Atom).

% error_make(+Locale, +Props, +Term, -Atom)
:- public error_make/4.
:- foreign(error_make/4, 'ForeignLocale',
      sysErrorMake('Interpreter','String','Properties','Object')).

/**
 * get_error_properties(P):
 * get_error_properties(L, P):
 * The predicate unifies P with the error properties of the
 * current knowledge base. The binary predicate allows specifying
 * a locale L. The error resource bundles have to be loaded in
 * advance via sys_load_resource/1.
 */
% get_error_properties(-Props)
:- public get_error_properties/1.
get_error_properties(Props) :-
   current_prolog_flag(sys_locale, Locale),
   get_error_properties(Locale, Props).

% get_error_properties(+Locale, -Props)
:- public get_error_properties/2.
:- foreign(get_error_properties/2, 'ForeignLocale',
      sysGetErrorProperties('Interpreter','String')).

/**
 * get_description_properties(C, P):
 * get_description_properties(L, C, P):
 * The predicate unifies P with the description properties of the
 * given capability C. The ternary predicate allows specifying
 * a locale L.
 */
% get_decription_properties(+Capability, -Props)
:- public get_description_properties/2.
get_description_properties(Capability, Props) :-
   current_prolog_flag(sys_locale, Locale),
   get_description_properties(Locale, Capability, Props).

% get_description_properties(+Locale, +Capability, -Props)
:- public get_description_properties/3.
:- foreign(get_description_properties/3, 'ForeignLocale',
      sysGetDescriptionProperties('Interpreter','String','String')).
