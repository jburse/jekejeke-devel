/**
 * The module provides access to the shell that started the interpreter.
 * The predicate getenv/2 allows accessing and enumerating shell
 * environment variables. The access is always case insensitive.
 *
 * Examples:
 * ?- get_time(D), format('%tc\n', [D]).
 * Mon Aug 22 17:07:24 CEST 2016
 * ?- statistics(wall, T), get_time(T, D), format('%tc\n', [D]).
 * Mon Aug 22 17:07:39 CEST 2016
 *
 * The predicates get_time/1 and get_time/2 can be used to retrieve a
 * time object that is suitable for format/2 and friends. The predicate
 * get_time/2 has an integer time stamp constructor parameter.
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

:- module(shell, []).

/*****************************************************************/
/* Environment Variables                                         */
/*****************************************************************/

/**
 * getenv(N, V):
 * The predicate succeeds for the value V of the environment
 * variable named N.
 */
% getenv(-Atom, -Atom)
:- public getenv/2.
getenv(Name, Value) :-
   ground(Name), !,
   sys_get_env(Name, Value).
getenv(Name, Value) :-
   sys_list_env(List),
   sys_member(Name, List),
   sys_get_env(Name, Value).

:- private sys_get_env/2.
:- foreign(sys_get_env/2, 'System', getenv('String')).

:- private sys_list_env/1.
:- foreign(sys_list_env/1, 'ForeignShell', sysListEnv('Interpreter')).

/*****************************************************************/
/* Retrieve Date & Calendar                                      */
/*****************************************************************/

/**
 * get_time(S):
 * The predicate succeeds with a current time object S.
 */
% get_time(-DateTime)
:- public get_time/1.
:- foreign_constructor(get_time/1, 'Date', new).

/**
 * get_time(T, D):
 * The predicate succeeds with a date object D for the time
 * T in milliseconds since January 1, 1970, 00:00:00 GMT.
 */
% get_time(+Integer, -DateTime)
:- public get_time/2.
get_time(T, D) :-
   var(D), !,
   sys_long_to_date(T, D).
get_time(T, D) :-
   sys_date_to_long(D, T).

:- private sys_long_to_date/2.
:- foreign_constructor(sys_long_to_date/2, 'Date', new(long)).

:- private sys_date_to_long/2.
:- virtual sys_date_to_long/2.
:- foreign(sys_date_to_long/2, 'Date', getTime).

/**
 * get_time(T, Z, C):
 * The predicate succeeds with a calendar object C for the time
 * T in milliseconds since January 1, 1970, 00:00:00 GMT,
 * in the desired time zone Z.
 */
% get_time(+Integer, +Atom, -DateTime)
:- public get_time/3.
:- foreign(get_time/3, 'ForeignShell', sysGetTime(long,'String')).

/*****************************************************************/
/* Format Date & Calendar                                        */
/*****************************************************************/

/**
 * date_atom(F, T, S):
 * date_atom(L, F, T, S):
 * The predicate succeeds in S with the date or calendar T
 * formatted to the format F. The quaternary predicate allows
 * specifying a locale L.
 */
% date_atom(+Format, +DateTime, -Atom)
:- public date_atom/3.
date_atom(Format, Argument, Atom) :-
   current_prolog_flag(sys_locale, Locale),
   date_atom(Locale, Format, Argument, Atom).

% date_atom(+Locale, +Format, +DateTime, -Atom)
:- public date_atom/4.
date_atom(Locale, Format, Argument, Atom) :-
   var(Atom), !,
   sys_date_to_string(Locale, Format, Argument, Atom).
date_atom(Locale, Format, Argument, Atom) :-
   sys_string_to_date(Locale, Format, Atom, Argument).

:- private sys_date_to_string/4.
:- foreign(sys_date_to_string/4, 'ForeignShell',
      sysDateToString('String','String','Object')).

:- private sys_string_to_date/4.
:- foreign(sys_string_to_date/4, 'ForeignShell',
      sysStringToDate('String','String','String')).
