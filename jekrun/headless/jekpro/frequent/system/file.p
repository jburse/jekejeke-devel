/**
 * This module provides some basic file operations. The implementation
 * is backed by some Java code that delivers some primitives to access
 * files. The Prolog text invokes absolute_file_name/[2,3] before
 * accessing these primitives. The result is that file name resolution
 * and encoding is applied for these primitives and that they
 * accept URIs.
 *
 * Example:
 * ?- directory_file('file:/C:/Program+Files/Java/', X).
 * X = 'jdk1.6.0_45' ;
 * X = 'jdk1.7.0_67' ;
 * X = 'jdk1.8.0_20' ;
 * Etc..
 *
 * Currently only URIs of protocol “file:” are supported. But future
 * implementations might support further protocols such as “jar:” or
 * service based protocols such as Google Drive, DropBox or others.
 * The realization might depend on the platform, so that the Swing
 * variant and Android might support different protocols in
 * the future.
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
:- use_package(foreign(matula/util/system)).

:- module(file, []).
:- use_module(library(basic/lists)).

/************************************************************/
/* Name & Path Assembly                                     */
/************************************************************/

/**
 * make_name(B, E, N):
 * If B or E is a variable then the predicate succeeds when
 * B and E unify with the base name and the extension
 * respectively of the file name N. Otherwise the predicates
 * succeeds when N unifies with the constructed name.
 */
% make_name(+-Atom, +-Atom, -+Atom)
:- public make_name/3.
make_name(B, E, N) :-
   (  var(B)
   ;  var(E)), !,
   sys_name_base(N, B),
   sys_name_ext(N, E).
make_name(B, E, N) :-
   sys_name_make(B, E, N).

:- private sys_name_base/2.
:- foreign(sys_name_base/2, 'ForeignFile',
      sysNameBase('String')).

:- private sys_name_ext/2.
:- foreign(sys_name_ext/2, 'ForeignFile',
      sysNameExt('String')).

:- private sys_name_make/3.
:- foreign(sys_name_make/3, 'ForeignFile',
      sysNameMake('String','String')).

/**
 * make_path(D, N, P):
 * If D or N is a variable then the predicate succeeds when D and N
 * unify with the directory and the name respectively of the path P.
 * Otherwise the predicates succeeds when P unifies with the
 * constructed path.
 */
% make_path(+-Atom, +-Atom, -+Atom)
% should also handle //
:- public make_path/3.
make_path(D, N, P) :-
   (  var(D)
   ;  var(N)), !,
   sys_path_directory(P, D),
   sys_path_name(P, N).
make_path(D, N, P) :-
   sys_path_make(D, N, P).

:- private sys_path_directory/2.
:- foreign(sys_path_directory/2, 'ForeignFile',
      sysPathDirectory('String')).

:- private sys_path_name/2.
:- foreign(sys_path_name/2, 'ForeignFile',
      sysPathName('String')).

:- private sys_path_make/3.
:- foreign(sys_path_make/3, 'ForeignFile',
      sysPathMake('String','String')).

/************************************************************/
/* File Ops                                                 */
/************************************************************/

/**
 * delete_file(F):
 * Succeeds when the file F could be deleted.
 */
% delete_file(+Atom)
:- public delete_file/1.
delete_file(Name) :-
   absolute_file_name(Name, Pin),
   sys_delete_file(Pin).

:- private sys_delete_file/1.
:- foreign(sys_delete_file/1, 'ForeignDirectory',
      sysDeleteFile('String')).

/**
 * rename_file(F, G):
 * Succeeds when the file F could be renamed to the file G.
 */
% rename_file(+Atom, +Atom)
:- public rename_file/2.
rename_file(FromName, ToName) :-
   absolute_file_name(FromName, FromPin),
   absolute_file_name(ToName, ToPin, [access(write)]),
   sys_rename_file(FromPin, ToPin).

:- private sys_rename_file/2.
:- foreign(sys_rename_file/2, 'ForeignDirectory',
      sysRenameFile('String','String')).

/**
 * exists_file(F):
 * Succeeds when the file F exists and when it isn't a directory.
 */
% exists_file(+Atom)
:- public exists_file/1.
exists_file(Name) :-
   absolute_file_name(Name, Pin),
   \+ sys_is_directory(Pin).

/************************************************************/
/* Directory Ops                                            */
/************************************************************/

/**
 * exists_directory(F):
 * Succeeds when the file F exists and when it is a directory.
 */
% exists_directory(+Atom)
:- public exists_directory/1.
exists_directory(Name) :-
   absolute_file_name(Name, Pin),
   sys_is_directory(Pin).

:- private sys_is_directory/1.
:- foreign(sys_is_directory/1, 'ForeignDirectory',
      sysIsDirectory('String')).

/**
 * make_directory(F):
 * The predicate succeeds when the directory F could be created.
 */
% make_directory(+Atom)
:- public make_directory/1.
make_directory(Name) :-
   absolute_file_name(Name, Pin, [access(write)]),
   sys_make_directory(Pin).

:- private sys_make_directory/1.
:- foreign(sys_make_directory/1, 'ForeignDirectory',
      sysMakeDirectory('String')).

/**
 * directory_file(F, N):
 * Succeeds whenever N unifies with an entry if the directory F.
 */
% directory_file(+Atom, -Atom)
:- public directory_file/2.
directory_file(Name, Elem) :-
   absolute_file_name(Name, Pin),
   sys_directory_files(Pin, List),
   member(Elem, List).

:- private sys_directory_files/2.
:- foreign(sys_directory_files/2, 'ForeignDirectory',
      sysDirectoryFiles('String')).

/************************************************************/
/* Time Stamp                                               */
/************************************************************/

/**
 * get_time_file(F, T):
 * Succeeds when T unifies with the last modified date of
 * the file F. T is measured in milliseconds since
 * the epoch.
 */
% get_time_file(+Atom, -Integer)
:- public get_time_file/2.
get_time_file(Name, Date) :-
   absolute_file_name(Name, Pin),
   sys_get_time_file(Pin, Date).

:- private sys_get_time_file/2.
:- foreign(sys_get_time_file/2, 'ForeignDirectory',
      sysGetTimeFile('String')).

/**
 * set_time_file(F, T):
 * Succeeds when the last modified date of the file F could
 * be set to T. T is measured in milliseconds since the
 * epoch but might be rounded by the system.
 */
% set_time_file(+Atom, +Integer)
:- public set_time_file/2.
set_time_file(Name, Date) :-
   absolute_file_name(Name, Pin, [access(write)]),
   sys_set_time_file(Pin, Date).

:- private sys_set_time_file/2.
:- foreign(sys_set_time_file/2, 'ForeignDirectory',
      sysSetTimeFile('String',long)).

/************************************************************/
/* Path Following                                           */
/************************************************************/

/**
 * is_relative_path(P):
 * The predicate succeeds when the path P is a relative path.
 */
% is_relative_path(+Atom)
:- public is_relative_path/1.
:- foreign(is_relative_path/1, 'ForeignFile',
      sysPathIsRelative('String')).

/**
 * follow_path(B, R, A):
 * If R is a variable then the predicate succeeds when R unifies
 * with the relative or absolute path that leads from B to A. Otherwise
 * the predicate succeeds when A unifies with the path that results
 * from B by following R.
 */
% follow_path(+-Atom, +-Atom, -+Atom)
% should also handle // with /
:- public follow_path/3.
follow_path(B, R, A) :-
   var(R), !,
   sys_path_relative(B, A, R).
follow_path(B, R, A) :-
   sys_path_absolute(B, R, A).

:- private sys_path_absolute/3.
:- foreign(sys_path_absolute/3, 'ForeignFile',
      sysPathAbsolute('String','String')).

:- private sys_path_relative/3.
:- foreign(sys_path_relative/3, 'ForeignFile',
      sysPathRelative('String','String')).

/**
 * canonical_path(P, C):
 * The predicate succeeds when C unifies with the canonical path of P.
 */
% canonical_path(+Atom, -Atom)
:- public canonical_path/2.
:- foreign(canonical_path/2, 'ForeignFile',
      sysCanonicalPath('String')).