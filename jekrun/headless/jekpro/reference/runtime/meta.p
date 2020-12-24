/**
 * The meta-predicate declaration takes as an argument a predicate
 * name and a number of meta-argument specifiers. Analogously the
 * meta-function directive takes as an argument a function name
 * and a number of meta-argument specifiers. We can describe
 * the arguments via the following grammar rules:
 *
 * meta_directive    --> "meta_predicate" meta_signature.
 *
 * meta_signature    --> module ":" meta_signature
 *                     | name [ "(" meta_specifier
 *                               { "," meta_specifier } ")" ].
 *
 * meta_specifier    --> integer
 *                     | "?"
 *                     | "::(" meta_specifier2 ")"
 *
 * meta_specifier2   --> integer
 *                     | "::(" meta_specifier2 ")".
 *
 * Example:
 * :- meta_predicate count(0,?).
 *
 * A positive integer n indicates a goal and a negative integer n
 * indicates a clause. If the integer n, respectively –n-1 if n<0, is
 * different from zero then the argument is a goal respectively clause
 * closure. The question mark (?) indicates that the argument is neither
 * a goal nor a clause. The (::)/1 wrapper indicates that the argument
 * is also an object message.
 *
 * Example:
 * ?- [user].
 * goal_expansion(writeln(X), (write(X), nl)).
 * ^D
 * ?- call(writeln, 'Hello World!').
 * Hello World!
 *
 * It should be noted that Jekejeke Prolog executes meta-predicates even
 * when meta-predicate directives are not present. The meta-predicate
 * directives are needed for pretty printing and term expansion. Closures
 * are expended by uncurrying, expanding and currying again. Term and
 * goal expansion can be switched on-off by the sys_clause_expand
 * flag, and it is on by default.
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

:- module(user, []).
:- use_module(library(runtime/quali)).

:- public prefix(meta_predicate).
:- op(1150, fx, meta_predicate).

/*******************************************************/
/* The Directives                                      */
/*******************************************************/

/**
 * meta_predicate M, …:
 * The predicate sets the corresponding functor to the meta-predicate
 * declaration M.
 */
% meta_predicate +Callables
:- public (meta_predicate)/1.
meta_predicate [P|Q] :- !, sys_meta_predicate(P), meta_predicate(Q).
meta_predicate P, Q :- !, sys_meta_predicate(P), meta_predicate(Q).
meta_predicate [] :- !.
meta_predicate P :- sys_meta_predicate(P).

:- private sys_meta_predicate/1.
sys_meta_predicate(P) :-
   functor(P, F, A),
   sys_make_indicator(F, A, I),
   callable_property(F, sys_context(C)),
   once((predicate_property(I, sys_usage(D)),
      \+ C = D)),
   \+ predicate_property(I, sys_meta_predicate(D)),
   throw(error(permission_error(promote, meta_predicate, I), _)).
sys_meta_predicate(P) :-
   functor(P, F, A),
   sys_make_indicator(F, A, I),
   callable_property(F, sys_context(C)),
   sys_neutral_predicate(I),
   predicate_property(I, full_name(N)),
   P =.. [_|L],
   R =.. [N|L],
   set_predicate_property(I, meta_predicate(R)),
   set_predicate_property(I, sys_meta_predicate(C)),
   sys_check_style_head(I).

% first defined in special.p
% sys_declaration_indicator(+Declaration, -Indicator).
:- public sys_declaration_indicator/2.
:- multifile sys_declaration_indicator/2.
sys_declaration_indicator(meta_predicate(P), I) :-
   functor(P, F, A),
   sys_make_indicator(F, A, I).
