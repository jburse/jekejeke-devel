/**
 * UDP based message broker.
 * With Guards
 * With Authority syntax
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

:- public infix(when).
:- op(1105, xfy, when).

:- public when/2.
:- meta_function when(?, 0).
when(_, _) :- throw(error(existence_error(body, when/2), _)).

% server_endpoint(+Atom, +Endpoint)
:- private server_endpoint/2.
:- group_local server_endpoint/2.

% actor_queue(+Atom, +Pipe)
:- private actor_queue/2.
:- group_local actor_queue/2.

% actor_deferred(+Atom, +Term)
:- private actor_deferred/2.
:- group_local actor_deferred/2.

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
   term_atom(end, Atom),
   atom_block(Atom, Block, [encoding('utf-8')]),
   make_authority(_, Host, Port, Authority),
   endpoint_send(Endpoint, Block, Host, Port),
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
   atom_block(Atom, Block, [encoding('utf-8')]),
   term_atom(Term, Atom),
   (  Term = end -> !
   ;  Term = send(Name, Message)
   -> actor_queue(Name, Queue),
      pipe_put(Queue, Message),
      fail
   ;  Term = run(Pid, Goal)
   -> spawn_actor(Goal, Name),
      send(Pid, started(Name)),
      fail; fail).

/**
 * spawn(A, G, P):
 * The predicate succeeds in P with a new actor on the
 * authority A running the goal G.
 */
% spawn(+Atom, +Goal, -Pid)
:- public spawn/3.
:- meta_predicate spawn(?, 0, ?).
spawn(Authority, Goal, pid(Authority, Name)) :-         /* local spawn */
   server_endpoint(Authority, _), !,
   spawn_actor(Goal, Name).
spawn(Authority2, Goal, pid(Authority2, Name)) :-       /* remote spawn */
   server_endpoint(Authority, Endpoint),
   term_atom(run(pid(Authority, Name), Goal), Atom),
   atom_block(Atom, Block, [encoding('utf-8')]),
   make_authority(_, Host2, Port2, Authority2),
   endpoint_send(Endpoint, Block, Host2, Port2),
   receive(started(Name), _).

% spawn_actor(+Goal, -Atom)
:- private spawn_actor/2.
:- meta_predicate spawn_actor(0, ?).
spawn_actor(Goal, Name) :-
   unslotted_new(Lock),
   lock_acquire(Lock),
   thread_new(spawn_run(Lock, Goal), Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   thread_start(Thread),
   lock_acquire(Lock).

% spawn_setup
:- private spawn_setup/0.
spawn_setup :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   pipe_new(Queue),
   assertz(actor_queue(Name, Queue)).

% spawn_cleanup
:- private spawn_cleanup/0.
spawn_cleanup :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   retract(actor_queue(Name, _)),
   retractall(actor_deferred(Name, _)).

% spawn_run(+Lock, +Goal)
:- private spawn_run/2.
:- meta_predicate spawn_run(?, 0).
spawn_run(Lock, Goal) :-
   setup_call_cleanup(
      (spawn_setup, lock_release(Lock)),
      Goal, spawn_cleanup).

/**
 * self(A):
 * The predicate succeeds in A with the current actor.
 */
% self(-Pid)
:- public self/1.
self(pid(Authority, Name)) :-
   server_endpoint(Authority, _),
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name).

/* send(A, M):
 * The predicate succeeds in sending the message M to the actor A.
 */
% send(+Pid, +Term)
:- public send/2.
send(pid(Authority, Name), Message) :-                  /* local send */
   server_endpoint(Authority, _), !,
   actor_queue(Name, Queue),
   pipe_put(Queue, Message).
send(pid(Authority, Name), Message) :-                  /* remote send */
   server_endpoint(_, Endpoint),
   term_atom(send(Name, Message), Atom),
   atom_block(Atom, Block, [encoding('utf-8')]),
   make_authority(_, Host, Port, Authority),
   endpoint_send(Endpoint, Block, Host, Port).

/**
 * receive(P, S):
 * The predicate blocks until a message arrives that matches
 * the pattern list P. It then succeeds in S with the selected
 * message.
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
   actor_queue(Name, Queue),
   repeat,
   pipe_take(Queue, Selected),
   (  receive_member(SelectedGuard, PatternGuards),
      receive_check(SelectedGuard, Selected), !
   ;  assertz(actor_deferred(Name, Selected)),
      fail).

/**
 * receive(P, T, S):
 * The predicate blocks until a message arrives that matches
 * the pattern list P. It then succeeds in S with the selected
 * message. The predicate fails after T milliseconds.
 */
% receive(+Assoc, +Integer, -Term)
:- public receive/3.
receive(PatternGuards, _, Selected) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   clause_ref(actor_deferred(Name, Selected), true, Ref),
   receive_member(SelectedGuard, PatternGuards),
   receive_check(SelectedGuard, Selected), !,
   erase_ref(Ref).
receive(PatternGuards, Timeout, Selected) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   actor_queue(Name, Queue),
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

% receive_check(+Assoc, -Term)
:- private receive_check/2.
receive_check(SelectedGuard, Selected) :-
   nonvar(SelectedGuard),
   SelectedGuard = (Selected when Guard), !,
   Guard.
receive_check(Selected, Selected).
