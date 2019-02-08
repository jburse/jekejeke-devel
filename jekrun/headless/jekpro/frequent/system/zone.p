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

:- module(zone, []).

/*****************************************************************/
/* Retrieve Date & Calendar                                      */
/*****************************************************************/

/**
 * get_time(D):
 * The predicate succeeds with a date object S for the current time.
 */
% get_time(-DateTime)
:- public get_time/1.
:- foreign_constructor(get_time/1, 'Date', new).

/**
 * get_time(T, D):
 * The predicate succeeds with a date object D for the time
 * T in milliseconds since January 1, 1970, 00:00:00 GMT and
 * vice versa.
 */
% get_time(+-Integer, -+DateTime)
:- public get_time/2.
get_time(Millis, DateTime) :-
   var(DateTime), !,
   sys_long_to_date(Millis, DateTime).
get_time(Millis, DateTime) :-
   sys_date_to_long(DateTime, Millis).

:- private sys_long_to_date/2.
:- foreign_constructor(sys_long_to_date/2, 'Date', new(long)).

:- private sys_date_to_long/2.
:- virtual sys_date_to_long/2.
:- foreign(sys_date_to_long/2, 'Date', getTime).

/**
 * get_time(T, Z, C):
 * get_time(L, T, Z, C):
 * The predicate succeeds with a calendar object C for the time
 * T in milliseconds since January 1, 1970, 00:00:00 GMT,
 * in the desired time zone Z. The quaternary predicate allows
 * specifying a locale L.
 */
% get_time(+Integer, +Atom, -DateTime)
:- public get_time/3.
get_time(Millis, Zone, DateTime) :-
   current_prolog_flag(sys_locale, Locale),
   get_time(Locale, Millis, Zone, DateTime).

% get_time(+Atom, +Integer, +Atom, -DateTime)
:- public get_time/4.
:- foreign(get_time/4, 'ForeignZone', sysGetTime('String',long,'String')).

/*****************************************************************/
/* Format Date & Calendar                                        */
/*****************************************************************/

/**
 * date_atom(F, T, S):
 * date_atom(L, F, T, S):
 * The predicate succeeds in S with the date or calendar T formatted
 * to the format F and vice versa. The quaternary predicate allows
 * specifying a locale L.
 */
% date_atom(+Format, +-DateTime, -+Atom)
:- public date_atom/3.
date_atom(Format, DateTime, Formatted) :-
   current_prolog_flag(sys_locale, Locale),
   date_atom(Locale, Format, DateTime, Formatted).

% date_atom(+Locale, +Format, +-DateTime, -+Atom)
:- public date_atom/4.
date_atom(Locale, Format, DateTime, Formatted) :-
   var(Formatted), !,
   sys_date_to_string(Locale, Format, DateTime, Formatted).
date_atom(Locale, Format, DateTime, Formatted) :-
   sys_string_to_date(Locale, Format, Formatted, DateTime).

:- private sys_date_to_string/4.
:- foreign(sys_date_to_string/4, 'ForeignZone',
      sysDateToString('String','String','Object')).

:- private sys_string_to_date/4.
:- foreign(sys_string_to_date/4, 'ForeignZone',
      sysStringToDate('String','String','String')).

/**
 * rfc1123_atom(T, A):
 * The predicate succeeds in A with the RFC1123 formatted time
 * T in milliseconds since January 1, 1970, 00:00:00 GMT
 * and vice versa.
 */
% rfc1123_atom(+-Integer, -+Atom)
:- public rfc1123_atom/2.
rfc1123_atom(Millis, Formatted) :-
   var(Formatted), !,
   get_time(en_GB, Millis, 'GMT', Calendar),
   date_atom(en_GB, 'EEE, dd MMM yyyy HH:mm:ss ''GMT''', Calendar, Formatted).
rfc1123_atom(Millis, Formatted) :-
   date_atom(en_GB, 'EEE, dd MMM yyyy HH:mm:ss zzz', Calendar, Formatted),
   get_time(Millis, Calendar).
