/**
 * This module provides a Prolog implementation of the actor model. The
 * realization leans towards Erlang with the same actor and message
 * semantics. Currently remote access is two way so that a client is
 * notified that a primitive was performed.
 *
 * Additionally we have bootstrapped a Prolog remote procedure call
 * rpc/2. This predicate can deliver multiple solutions via backtracking.
 * The predicate does react to events in the continua-tion and will
 * tear down the remote call.
 *
 * Example:
 * ?- rpc('localhost:3010', father(tom, X)).
 * X = sally ;
 * X = erica
 * ?- rpc('localhost:3010', father(tom, X)).
 * X = sally
 *
 * Currently we only support unreliable UDP transport with up
 * to 4096 bytes. We might add further protocols in the future.
 * Especially a combination with our module "http" and its web sockets
 * is planned which would even allow communication with web browser.
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

:- package(library(jekpro/frequent/experiment)).

:- module(broker, []).

:- use_module(library(misc/socket)).
:- use_module(library(misc/pipe)).
:- use_module(library(misc/lock)).
:- use_module(library(system/thread)).
:- use_module(library(system/domain)).
:- use_module(library(experiment/ref)).
:- use_module(library(structure/bytes)).

:- public infix(when).
:- op(1105, xfy, when).

:- public when/2.
:- meta_function when(?, 0).
when(_, _) :- throw(error(existence_error(body, when/2), _)).

% server_endpoint(+Atom, +Endpoint)
:- private server_endpoint/2.
:- group_local server_endpoint/2.

% actor_queue(+Atom, +Pipe, +Thread)
:- private actor_queue/3.
:- group_local actor_queue/3.

% actor_deferred(+Atom, +Term)
:- private actor_deferred/2.
:- group_local actor_deferred/2.

/***********************************************************/
/* Message Broker                                          */
/***********************************************************/

/**
 * broker_start(A):
 * The predicate succeeds in starting a broker identified by the authority A.
 */
% broker_start(+Atom)
:- public broker_start/1.
broker_start(Authority) :-
   unslotted_new(Lock),
   lock_acquire(Lock),
   thread_new(broker_run(Lock, Authority), Thread),
   thread_start(Thread),
   lock_acquire(Lock),
   spawn_setup.

/**
 * broker_stop:
 * The predicate succeeds in stopping the current broker.
 */
% broker_stop
:- public broker_stop/0.
broker_stop :-
   server_endpoint(Authority, Endpoint),
   term_block(end, Block),
   make_authority(_, Host, Port, Authority),
   sys_atomic(endpoint_send(Endpoint, Block, Host, Port)),
   spawn_cleanup.

% broker_run(+Lock, +Atom)
:- private broker_run/2.
broker_run(Lock, Authority) :-
   setup_call_cleanup(
      (broker_setup(Authority), lock_release(Lock)),
      broker_server(Authority),
      broker_cleanup(Authority)).

% broker_setup(+Atom)
:- private broker_setup/1.
broker_setup(Authority) :-
   make_authority(_, Host, Port, Authority),
   endpoint_new(Host, Port, Endpoint),
   assertz(server_endpoint(Authority, Endpoint)).

% broker_cleanup(+Atom)
:- private broker_cleanup/1.
broker_cleanup(Authority) :-
   retract(server_endpoint(Authority, Endpoint)),
   close(Endpoint).

% broker_server(+Atom)
:- private broker_server/1.
broker_server(Authority) :-
   server_endpoint(Authority, Endpoint),
   repeat,
   endpoint_receive(Endpoint, Block),
   term_block(Term, Block),
   (  Term = end -> !
   ;  Term = control(Name, Message)
   -> actor_queue(Name, Queue, _),
      pipe_put(Queue, Message),
      fail
   ;  Term = send(Name, Message, Pid)
   -> actor_queue(Name, Queue, _),
      pipe_put(Queue, Message),
      control(Pid, sent),
      fail
   ;  Term = spawn(Goal, Pid)
   -> spawn_run(Goal, Name),
      control(Pid, spawned(Name)),
      fail
   ;  Term = exit(Name, Message, Pid)
   -> exit_actor(Name, Message),
      control(Pid, exited),
      fail; fail).

/* control(P, M):
 * The predicate succeeds in sending the message M to the
 * actor identified by the actor path P. This sending is non-blocking
 * and succeeds immediately.
 */
% control(+Pid, +Term)
:- private control/2.
control(pid(Authority, Name), Message) :-               /* local control */
   server_endpoint(Authority, _), !,
   actor_queue(Name, Queue, _),
   pipe_put(Queue, Message).
control(pid(Authority, Name), Message) :-               /* remote control */
   server_endpoint(_, Endpoint),
   term_block(control(Name, Message), Block),
   make_authority(_, Host, Port, Authority),
   sys_atomic(endpoint_send(Endpoint, Block, Host, Port)).

/***********************************************************/
/* Actor Primitives                                        */
/***********************************************************/

/**
 * spawn(A, G, P):
 * The predicate succeeds in P with the actor path of a new actor on
 * the authority A running the goal G. Spawn is blocking until
 * acknowledge is received.
 */
% spawn(+Atom, +Goal, -Pid)
:- public spawn/3.
:- meta_predicate spawn(?, 0, ?).
spawn(Authority, Goal, pid(Authority, Name)) :-         /* local run */
   server_endpoint(Authority, _), !,
   spawn_run(Goal, Name).
spawn(Authority, Goal, pid(Authority, Name)) :-         /* remote run */
   server_endpoint(Authority2, Endpoint),
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name2),
   term_block(spawn(Goal, pid(Authority2, Name2)), Block),
   make_authority(_, Host, Port, Authority),
   sys_atomic(endpoint_send(Endpoint, Block, Host, Port)),
   receive(spawned(Name), _).

% spawn_run(+Goal, -Atom)
:- private spawn_run/2.
:- meta_predicate spawn_run(0, ?).
spawn_run(Goal, Name) :-
   unslotted_new(Lock),
   lock_acquire(Lock),
   thread_new(spawn_safe(Lock, Goal), Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   thread_start(Thread),
   lock_acquire(Lock).

% spawn_safe(+Lock, +Goal)
:- private spawn_safe/2.
:- meta_predicate spawn_safe(?, 0).
spawn_safe(Lock, Goal) :-
   setup_call_cleanup(
      (spawn_setup, lock_release(Lock)),
      Goal,
      spawn_cleanup).

% spawn_setup
:- private spawn_setup/0.
spawn_setup :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   pipe_new(Queue),
   assertz(actor_queue(Name, Queue, Thread)).

% spawn_cleanup
:- private spawn_cleanup/0.
spawn_cleanup :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   retract(actor_queue(Name, _, _)),
   retractall(actor_deferred(Name, _)).

/**
 * self(P):
 * The predicate succeeds in P with the actor path of the current actor.
 */
% self(-Pid)
:- public self/1.
self(pid(Authority, Name)) :-
   server_endpoint(Authority, _),
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name).

/**
 * exit(P, M):
 * The predicate succeeds in interrupting the actor P by an error M.
 * Exit is blocking until acknowledge is received.
 */
% exit(+Pid, +Term)
:- public exit/2.
exit(pid(Authority, Name), Message) :-                  /* local exit */
   server_endpoint(Authority, _), !,
   exit_actor(Name, Message).
exit(pid(Authority, Name), Message) :-                  /* remote exit */
   server_endpoint(Authority2, Endpoint),
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name2),
   term_block(exit(Name, Message, pid(Authority2, Name2)), Block),
   make_authority(_, Host, Port, Authority),
   sys_atomic(endpoint_send(Endpoint, Block, Host, Port)),
   receive(exited, _).

% exit_actor(+Atom, +Term)
:- private exit_actor/2.
exit_actor(Name, Message) :-
   actor_queue(Name, _, Thread), !,
   thread_abort(Thread, Message),
   thread_join(Thread).
exit_actor(_, _).

/***********************************************************/
/* Message Primitives                                      */
/***********************************************************/

/* send(P, M):
 * The predicate succeeds in sending the message M to the
 * actor identified by the actor path P. This sending is blocking
 * until acknowledge is received.
 */
% send(+Pid, +Term)
:- public send/2.
send(pid(Authority, Name), Message) :-                  /* local send */
   server_endpoint(Authority, _), !,
   actor_queue(Name, Queue, _),
   pipe_put(Queue, Message).
send(pid(Authority, Name), Message) :-                  /* remote send */
   server_endpoint(Authority2, Endpoint),
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name2),
   term_block(send(Name, Message, pid(Authority2, Name2)), Block),
   make_authority(_, Host, Port, Authority),
   sys_atomic(endpoint_send(Endpoint, Block, Host, Port)),
   receive(sent, _).

/**
 * receive(L, M):
 * receive(L, M, T):
 * The predicate succeeds in M with the message that the current
 * actor received and that matches the pattern and guard disjunction
 * L. Receiving is blocking and succeeds only when a matching
 * message has arrived. The ternary predicate takes an additional
 * timeout parameter.
 */
% receive(+Assoc, -Term)
:- public receive/2.
receive(PatternGuards, Selected) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   clause_ref(actor_deferred(Name, Selected), true, Ref),
   receive_member(SelectedGuard, PatternGuards),
   receive_check(SelectedGuard, Selected), !,
   erase_ref(Ref).
receive(PatternGuards, Selected) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   actor_queue(Name, Queue, _),
   repeat,
   pipe_take(Queue, Selected),
   (  receive_member(SelectedGuard, PatternGuards),
      receive_check(SelectedGuard, Selected), !
   ;  assertz(actor_deferred(Name, Selected)),
      fail).

% receive(+Assoc, -Term, +Integer)
:- public receive/3.
receive(PatternGuards, Selected, _) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   clause_ref(actor_deferred(Name, Selected), true, Ref),
   receive_member(SelectedGuard, PatternGuards),
   receive_check(SelectedGuard, Selected), !,
   erase_ref(Ref).
receive(PatternGuards, Selected, Timeout) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   actor_queue(Name, Queue, _),
   repeat,
   (pipe_poll(Queue, Timeout, Selected)
-> (  receive_member(SelectedGuard, PatternGuards),
      receive_check(SelectedGuard, Selected), !
   ;  assertz(actor_deferred(Name, Selected)),
      fail); !, fail).

% receive_member(-Term, +Assoc)
:- private receive_member/2.
receive_member(Term, Assoc) :-
   nonvar(Assoc),
   Assoc = (Term; _).
receive_member(Term, Assoc) :-
   nonvar(Assoc),
   Assoc = (_; Assoc2), !,
   receive_member(Term, Assoc2).
receive_member(Term, Term).

% receive_check(+Pair, -Term)
:- private receive_check/2.
receive_check(SelectedGuard, Selected) :-
   nonvar(SelectedGuard),
   SelectedGuard = (Selected when Guard), !,
   Guard.
receive_check(Selected, Selected).

/***********************************************************/
/* Prolog RPC                                              */
/***********************************************************/

/**
 * rpc(A, G):
 * The predicate succeeds whenever the authority A running
 * the goal G succeeds.
 */
% rpc(+Atom, +Goal)
:- public rpc/2.
:- meta_predicate rpc(?, 0).
rpc(A, G) :-
   term_variables(G, L),
   self(P),
   setup_call_cleanup(
      spawn(A, rpc_wrap(L, G, P), Q),
      rpc_fetch(L, Q),
      exit(Q, system_error(user_close))).

% rpc_fetch(+List, +Pid)
:- private rpc_fetch/2.
rpc_fetch(L, Q) :-
   repeat,
   receive((last(_); the(_); ball(_); no), S),
   (  S = last(H)
   -> !, H = L
   ;  S = the(H)
   -> (H = L; send(Q, next), fail)
   ;  S = ball(H)
   -> throw(H, error(resource_error(remote_error), _))
   ;  !, fail).

% rpc_wrap(+List, +Goal, +Pid)
:- public rpc_wrap/3.
:- sys_notrace rpc_wrap/3.
:- meta_predicate rpc_wrap(?, 0, ?).
rpc_wrap(L, G, P) :-
   sys_trap(rpc_query(L, G, P),
      E,
      (  sys_error_type(E, system_error(_)) -> sys_raise(E)
      ;  send(P, ball(E)))).

% rpc_query(+List, +Goal, +Pid)
:- private rpc_query/3.
:- meta_predicate rpc_query(?, 0, ?).
rpc_query(L, G, P) :-
   current_prolog_flag(sys_choices, X),
   G,
   current_prolog_flag(sys_choices, Y),
   (  X == Y, !, send(P, last(L))
   ;  send(P, the(L)),
      receive(next, _),
      fail).
rpc_query(_, _, P) :-
   send(P, no).
