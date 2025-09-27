% Thread Pool Implementation in SWI-Prolog
% This creates a pool of worker threads that execute goals from a message queue
:- module(threadpool,
          [ create_thread_pool/1,
            pool_status/0,
            shutdown_thread_pool/0,
            submit_task/1
          ]).

:- use_module(library(thread)).
:- use_module(library(debug)).

% Global variables to store thread pool state
:- dynamic thread_pool_queue/1.
:- dynamic thread_pool_workers/1.
:- dynamic thread_pool_size/1.

% Create a thread pool with N worker threads
create_thread_pool(Size) :-
    % Create message queue for tasks
    message_queue_create(TaskQueue),
    assert(thread_pool_queue(TaskQueue)),
    assert(thread_pool_size(Size)),

    % Create worker threads
    findall(WorkerId,
            (between(1, Size, I),
             atom_concat(worker_, I, WorkerId),
             thread_create(worker_loop(TaskQueue), _, [alias(WorkerId), detached(true)])),
            Workers),
    assert(thread_pool_workers(Workers)),

    format('Thread pool created with ~w workers~n', [Size]),
    format('Workers: ~w~n', [Workers]).

% Worker thread loop - continuously processes tasks from the queue
worker_loop(TaskQueue) :-
    thread_self(ThreadId),
    format('[~w] Worker started~n', [ThreadId]),
    worker_process_loop(TaskQueue, ThreadId).

worker_process_loop(TaskQueue, ThreadId) :-
    % Wait for a task from the queue
    thread_get_message(TaskQueue, Task),

    % Process the task
    (   Task = stop
    ->  format('[~w] Worker stopping~n', [ThreadId])
    ;   process_task(Task, ThreadId),
        worker_process_loop(TaskQueue, ThreadId)
    ).

% Process individual tasks
process_task(task(Goal, TaskId), WorkerId) :-
    %%    format('[~w] Starting task ~w: ~q~n', [WorkerId, TaskId, Goal]),
%%    get_time(StartTime),

    % Execute the goal safely
    (
        catch(call(Goal), Error,
              format('[~w] Task ~w failed with error: ~q~n', [WorkerId, TaskId, Error]))
        % ->  Status = completed
%    ;   Status = failed
    ).
%%   get_time(EndTime),
%%   Duration is EndTime - StartTime,
%%   format('[~w] Task ~w ~w in ~2f seconds~n', [WorkerId, TaskId, Status, Duration]).

% Submit a task to the thread pool
submit_task(Goal) :-
    submit_task(Goal, _).

submit_task(Goal, TaskId) :-
    thread_pool_queue(TaskQueue),
    !,
    gensym(task_, TaskId),
    Task = task(Goal, TaskId),
    thread_send_message(TaskQueue, Task).
%    format('Submitted task ~w: ~q~n', [TaskId, Goal]).

submit_task(_, _) :-
    format('Error: Thread pool not created. Call create_thread_pool/1 first.~n').

% Shutdown the thread pool
shutdown_thread_pool :-
    thread_pool_queue(TaskQueue),
    thread_pool_workers(Workers),
    thread_pool_size(Size),
    !,

    % Send stop messages to all workers
    forall(between(1, Size, _),
           thread_send_message(TaskQueue, stop)),

    % Wait a moment for workers to finish
    sleep(20),

    % Clean up
    message_queue_destroy(TaskQueue),
    retract(thread_pool_queue(TaskQueue)),
    retract(thread_pool_workers(Workers)),
    retract(thread_pool_size(Size)),

    format('Thread pool shutdown complete~n').

shutdown_thread_pool :-
    format('No thread pool to shutdown~n').

% Utility predicate for testing
test_sleep_task(Seconds) :-
    format('  [TASK] Sleeping for ~w seconds...~n', [Seconds]),
    sleep(Seconds),
    format('  [TASK] Woke up after ~w seconds!~n', [Seconds]).

% Example usage and test cases
demo :-
    format('=== Thread Pool Demo ===~n'),

    % Create a thread pool with 3 workers
    create_thread_pool(3),

    % Submit various tasks
    submit_task(test_sleep_task(2)),
    submit_task(test_sleep_task(1)),
    submit_task(test_sleep_task(3)),
    submit_task((format('  [TASK] Hello from custom task!~n'), sleep(1))),
    submit_task(test_sleep_task(2)),
    submit_task((writeln('  [TASK] Quick task'), sleep(0.5))),

    % Let tasks run
    format('~nWaiting for tasks to complete...~n'),
    sleep(8),

    % Shutdown
    format('~nShutting down thread pool...~n'),
    shutdown_thread_pool.

% Alternative demo with task monitoring
demo_with_monitoring :-
    format('=== Thread Pool Demo with Task IDs ===~n'),

    create_thread_pool(2),

    % Submit tasks and collect their IDs
    findall(TaskId,
            (member(Duration, [3, 1, 4, 2]),
             submit_task(test_sleep_task(Duration), TaskId)),
            TaskIds),

    format('Submitted tasks: ~w~n', [TaskIds]),

    % Wait and shutdown
    sleep(10),
    shutdown_thread_pool.

% Helper to check thread pool status
pool_status :-
    (   thread_pool_queue(Queue)
    ->  thread_pool_size(Size),
        thread_pool_workers(Workers),
        message_queue_property(Queue, size(QueueSize)),
        format('Thread pool status:~n'),
        format('  Queue: ~w (size: ~w)~n', [Queue, QueueSize]),
        format('  Workers: ~w~n', [Workers]),
        format('  Pool size: ~w~n', [Size])
    ;   format('No active thread pool~n')
    ).
