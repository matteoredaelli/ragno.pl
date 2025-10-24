:- module(thread_pool, [
              start_pool/2,
              stop_pool/1,
              submit_task/2,
              pool_status/1,
              wait_pool_and_close/1
                       ]).

:- use_module(library(thread)).

% Dynamic predicates to track pool state
:- dynamic pool_config/3.            % pool_config(PoolName, WorkerCount, TaskQueue)
:- dynamic worker_thread/3.          % worker_thread(PoolName, ThreadId, WorkerNum)

%! start_pool(+PoolName, +WorkerCount) is det.
%  Initialize a thread pool with the specified number of workers.
%  Each worker will automatically restart if it fails.
start_pool(PoolName, WorkerCount) :-
    % Create a message queue for tasks
    message_queue_create(TaskQueue),

    % Store pool configuration
    assertz(pool_config(PoolName, WorkerCount, TaskQueue)),

    % Start worker threads
    forall(
        between(1, WorkerCount, N),
        start_worker(PoolName, N, TaskQueue)
    ),

    format('Thread pool "~w" started with ~d workers~n', [PoolName, WorkerCount]).

%! start_worker(+PoolName, +WorkerNum, +TaskQueue) is det.
%  Start a single worker thread that processes tasks from the queue.
start_worker(PoolName, WorkerNum, TaskQueue) :-
    atom_concat(PoolName, '_worker_', Prefix),
    atom_concat(Prefix, WorkerNum, ThreadName),

    thread_create(
        worker_loop(PoolName, WorkerNum, TaskQueue),
        ThreadId,
        [alias(ThreadName), detached(false)]
    ),

    assertz(worker_thread(PoolName, ThreadId, WorkerNum)),
    format('Worker ~w started (Thread ID: ~w)~n', [ThreadName, ThreadId]).

%! worker_loop(+PoolName, +WorkerNum, +TaskQueue) is det.
%  Main loop for worker threads. Continuously processes tasks from queue.
%  Implements automatic restart on failure.
worker_loop(PoolName, WorkerNum, TaskQueue) :-
    catch(
        worker_loop_safe(PoolName, WorkerNum, TaskQueue),
        Error,
        handle_worker_error(PoolName, WorkerNum, TaskQueue, Error)
    ).

%! worker_loop_safe(+PoolName, +WorkerNum, +TaskQueue) is det.
%  Safe inner loop that processes tasks.
worker_loop_safe(PoolName, WorkerNum, TaskQueue) :-
    repeat,
    thread_get_message(TaskQueue, Task, [timeout(1)]),
    (   Task = stop
    ->  format('Worker ~w-~d received stop signal~n', [PoolName, WorkerNum]),
        !
    ;   Task = task(Goal, TaskId)
    ->  %format('Worker ~w-~d processing task ~w~n', [PoolName, WorkerNum, TaskId]),
        execute_task(Goal, TaskId, PoolName, WorkerNum),
        fail  % Continue loop
    ;   fail  % Timeout, continue loop
    ).

%! execute_task(+Goal, +TaskId, +PoolName, +WorkerNum) is det.
%  Execute a task with error handling.
execute_task(Goal, TaskId, PoolName, WorkerNum) :-
    catch(
        %        (   
        call(Goal),
        %            format('Worker ~w-~d completed task ~w successfully~n', 
%                   [PoolName, WorkerNum, TaskId])
%        ),
        TaskError,
        (   format('Worker ~w-~d: Task ~w failed with error: ~w~n',
                   [PoolName, WorkerNum, TaskId, TaskError]),
            format('Worker continues running...~n', [])
        )
    ).

%! handle_worker_error(+PoolName, +WorkerNum, +TaskQueue, +Error) is det.
%  Handle worker thread errors and restart the worker.
handle_worker_error(PoolName, WorkerNum, TaskQueue, Error) :-
    format('ERROR: Worker ~w-~d crashed with error: ~w~n',
           [PoolName, WorkerNum, Error]),
    format('Restarting worker ~w-~d...~n', [PoolName, WorkerNum]),

    % Clean up old thread reference
    thread_self(OldThreadId),
    retractall(worker_thread(PoolName, OldThreadId, WorkerNum)),

    % Restart the worker
    sleep(0.1),  % Brief pause before restart
    start_worker(PoolName, WorkerNum, TaskQueue).

%! submit_task(+PoolName, +Goal) is det.
%  Submit a task to the thread pool for execution.
submit_task(PoolName, Goal) :-
    pool_config(PoolName, _, TaskQueue),
    !,
    get_time(Time),
    TaskId = Time,
    thread_send_message(TaskQueue, task(Goal, TaskId)).
%format('Task ~w submitted to pool "~w"~n', [TaskId, PoolName]).

submit_task(PoolName, _) :-
    format('ERROR: Pool "~w" does not exist~n', [PoolName]),
    fail.

%! stop_pool(+PoolName) is det.
%  Gracefully stop all workers in the pool and clean up resources.
stop_pool(PoolName) :-
    pool_config(PoolName, WorkerCount, TaskQueue),
    !,
    format('Stopping pool "~w"...~n', [PoolName]),

    % Send stop signals to all workers
    forall(
        between(1, WorkerCount, _),
        thread_send_message(TaskQueue, stop)
    ),

    % Wait for all workers to finish
    findall(ThreadId, worker_thread(PoolName, ThreadId, _), ThreadIds),
    forall(
        member(ThreadId, ThreadIds),
        (   catch(thread_join(ThreadId), _, true),
            retractall(worker_thread(PoolName, ThreadId, _))
        )
    ),

    % Clean up queue and config
    message_queue_destroy(TaskQueue),
    retractall(pool_config(PoolName, _, _)),

    format('Pool "~w" stopped~n', [PoolName]).

stop_pool(PoolName) :-
    format('Pool "~w" does not exist~n', [PoolName]).

%! pool_status(+PoolName) is det.
%  Display the current status of a thread pool.

pool_status(PoolName) :-
    pool_config(PoolName, WorkerCount, TaskQueue),
    !,
    format('~n=== Pool Status: ~w ===~n', [PoolName]),
    format('Worker count: ~d~n', [WorkerCount]),
    format('Task queue: ~w~n', [TaskQueue]),

    findall(ThreadId-WorkerNum, worker_thread(PoolName, ThreadId, WorkerNum), Workers),
    length(Workers, WorkerLen),
    format('Active workers: ~d~n', [WorkerLen]),
    forall(
        member(ThreadId-WorkerNum, Workers),
        (   thread_property(ThreadId, status(Status)),
            format('  Worker ~d (Thread ~w): ~w~n', [WorkerNum, ThreadId, Status])
        )
    ),
    format('========================~n~n').

pool_status(PoolName) :-
    format('Pool "~w" does not exist~n', [PoolName]).

wait_pool_and_close(PoolName) :-
    sleep(1),
    pool_config(PoolName, _WorkerCount, TaskQueue),
    message_queue_property(TaskQueue, Property),
    format('~n=== Queue count: ~w ===~n', Property),
    member(size(Count), Property),
    format('~n=== Queue count: ~w ===~n', Count),
    Count > 0 -> wait_pool_and_close(PoolName) ; true.
% ============================================================================
% EXAMPLE USAGE
% ============================================================================

% Example task that succeeds
example_task(N) :-
    format('Executing task with parameter: ~w~n', [N]),
    sleep(0.5),
    Result is N * 2,
    format('Task result: ~w~n', [Result]).

% Example task that fails (to demonstrate auto-restart)
failing_task(N) :-
    format('Executing failing task: ~w~n', [N]),
    (   N mod 3 =:= 0
    ->  throw(error(intentional_failure, 'Task designed to fail'))
    ;   format('Task ~w succeeded~n', [N])
    ).

% Demo: Start pool, submit tasks, and stop
demo :-
    % Start a pool with 3 workers
    start_pool(my_pool, 3),

    % Submit some tasks
    forall(
        between(1, 10, N),
        submit_task(my_pool, example_task(N))
    ),

    % Wait for tasks to complete
    sleep(6),

    % Check status
    pool_status(my_pool),

    % Stop the pool
    stop_pool(my_pool).

% Demo with failing tasks to show auto-restart
demo_with_failures :-
    start_pool(resilient_pool, 2),

    % Submit tasks that will occasionally fail
    forall(
        between(1, 12, N),
        submit_task(resilient_pool, failing_task(N))
    ),

    sleep(8),
    pool_status(resilient_pool),
    stop_pool(resilient_pool).
