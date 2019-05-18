%%%-------------------------------------------------------------------
%%% @author antonio.carrasco.valero
%%% @copyright (C) 2019, Antonio Carrasco Valero Licensed under European Public License EUPL
%%% Licensed under the EUPL, Version 1.1 only (the "Licence");
%%%
%%% You may not use this work except in compliance with the
%%% Licence.
%%%
%%% You may obtain a copy of the Licence at:
%%% https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
%%%
%%% Unless required by applicable law or agreed to in
%%% writing, software distributed under the Licence is
%%% distributed on an "AS IS" basis,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
%%% express or implied.
%%%
%%% See the Licence for the specific language governing
%%% permissions and limitations under the Licence.
%%% @doc
%%%  run as
%%%    c(blinkowaster).
%%%
%%%    blinkowaster:waste( 7, 20000, 5). % 7 threads 20000 max prime, 5 seconds
%%%
%%%    blinkowaster:start( 2, 2000, 7, 20000, 1). % 2 runs with 2000 millis delays in between of 7 threads 20000 max prime, 5 seconds
%%%    blinkowaster:start( [[ 2, 2000, 7, 20000, 1]]). % ONE launch 2 runs with 2000 millis delays in between of 7 threads 20000 max prime, 5 seconds
%%%
%%%    blinkowaster:start([[300,2000,4,20000,1],[200,3000,4,20000,1],[400,1500,3,20000,1]]).
%%% @end
%%% Created : 201905121555
%%%-------------------------------------------------------------------
-module(blinkowaster).
-author("antonio.carrasco.valero").

%% API
-export([start/1, start/5, periodicWaste/5, waste/3]).

-import(timer, [sleep/1]).
-import(os, [cmd/2]).
-import(string, [replace/3]).

-define(WASTECOMMANDTEMPLATE, "sysbench --test=cpu --threads={T_Threads} --cpu-max-prime={T_CpuMaxPrime} --time={T_Time} run").

% Uncomment to log all the CPU waste sysbench commands
%-define( LOGWASTEOUTPUT, true).
-ifdef(LOGWASTEOUTPUT).
-define(WASTEOUTPUTLOGGER, io:fwrite("blinkowaster:waste DONE A_WasteCommandOutput=~p\n", [A_WasteCommandOutput])).
-else.
-define(WASTEOUTPUTLOGGER, ok).
-endif.


start(T_AllWasters) ->
    process_flag(trap_exit, true),
    launchAllWasters(T_AllWasters, []).

launchAllWasters([], T_LaunchedWasterProcs) ->
    io:fwrite("blinkowaster:launchAllWasters launched ALL Wasters T_LaunchedWasterProcs=~w\n", [T_LaunchedWasterProcs]),
    waitForAllWastersDone(T_LaunchedWasterProcs, 0);
launchAllWasters([T_FirstWaster | T_RemWasters], T_LaunchedWasterProcs) ->
    io:fwrite("blinkowaster:launchAllWasters launching ONE Waster T_FirstWaster=~w, T_RemWasters=~w, T_LaunchedWasterProcs=~w\n", [T_FirstWaster, T_RemWasters, T_LaunchedWasterProcs]),
    A_WasterProc = launchOneWaster(T_FirstWaster),
    launchAllWasters(T_RemWasters, [A_WasterProc | T_LaunchedWasterProcs]).

launchOneWaster([T_NumRuns, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time]) ->
    io:fwrite("blinkowaster:launchOneWaster about to launch ONE Waster T_NumRuns=~B, T_DelayBetweenRuns=~B, T_Threads=~B, T_CpuMaxPrime=~B, T_Time=~B\n", [T_NumRuns, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time]),
    spawn_link(blinkowaster, periodicWaste, [T_NumRuns, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time]).


launchOneWaster_WithStart([T_NumRuns, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time]) ->
    io:fwrite("blinkowaster:launchOneWaster about to launch ONE Waster T_NumRuns=~B, T_DelayBetweenRuns=~B, T_Threads=~B, T_CpuMaxPrime=~B, T_Time=~B\n", [T_NumRuns, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time]),
    spawn_link(blinkowaster, start, [T_NumRuns, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time]).


waitForAllWastersDone([], T_NumWaits) ->
    io:fwrite("blinkowaster:waitForAllWastersDone NO MORE T_LaunchedWasterProcs, T_NumWaits=~B\n", [T_NumWaits]),
    io:fwrite("blinkowaster:waitForAllWastersDone EXITING\n"),
    timer:sleep(1000),
    exit(normal);
waitForAllWastersDone(T_LaunchedWasterProcs, T_NumWaits) ->
    io:fwrite("blinkowaster:waitForAllWastersDone T_LaunchedWasterProcs=~w, T_NumWaits=~B\n", [T_LaunchedWasterProcs, T_NumWaits]),
    receive
        finished ->
            io:fwrite("blinkowaster:waitForAllWastersDone received finished T_NumWaits=~B Stopping Wasters ~w\n", [T_NumWaits, T_LaunchedWasterProcs]),
            stopAllWasters(T_LaunchedWasterProcs);
        {'EXIT', M_Exiter, M_Reason} ->
            io:fwrite("blinkowaster:waitForAllWastersDone receive EXIT M_Exiter~w, M_Reason= ~w\n", [M_Exiter, M_Reason]),
            A_WasterProcsNotExited = lists:subtract(T_LaunchedWasterProcs, [M_Exiter]),
            if
                length(A_WasterProcsNotExited) == length(T_LaunchedWasterProcs) ->
                    io:fwrite("blinkowaster:waitForAllWastersDone receive EXIT FROM UNKNOWN process M_Exiter~w, M_Reason= ~w\n", [M_Exiter, M_Reason]),
                    ok;
                true ->
                    io:fwrite("blinkowaster:waitForAllWastersDone receive EXIT FROM KNOWN Launched Wasterprocess M_Exiter~w, M_Reason= ~w\n", [M_Exiter, M_Reason]),
                    waitForAllWastersDone(A_WasterProcsNotExited, T_NumWaits + 1)
            end;
        {_, _, _} ->
            io:fwrite("blinkowaster:waitForAllWastersDone receive _\n"),
            waitForAllWastersDone(T_LaunchedWasterProcs, T_NumWaits + 1)
    end.

stopAllWasters([]) ->
    io:fwrite("blinkowaster:stopAllWasters EXITING\n"),
    exit(normal);
stopAllWasters([T_FirstLaunchedWasterProcs | T_RemLaunchedWasterProcs]) ->
    io:fwrite("blinkowaster:stopAllWasters requesting EXIT from ~w\n", [T_FirstLaunchedWasterProcs]),
    try
        exit(T_FirstLaunchedWasterProcs, kill)
    catch
        {_, _, _} -> ok
    end,
    stopAllWasters(T_RemLaunchedWasterProcs).



start(T_NumRuns, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time) ->
    process_flag(trap_exit, true),
    launchPeriodicWaste(T_NumRuns, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time).

launchPeriodicWaste(T_NumRuns, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time) ->
    A_PeriodicWasteProc = spawn_link(blinkowaster, periodicWaste, [T_NumRuns, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time]),
    io:fwrite("blinkowaster:launchPeriodicWaste launched blinkowaster periodicWaste A_ShowProc=~w\n", [A_PeriodicWasteProc]),
    waitForPeriodicWasteDone(A_PeriodicWasteProc, 0).


waitForPeriodicWasteDone(T_PeriodicWasteProc, T_NumWaits) ->
    io:fwrite("blinkowaster:waitForPeriodicWasteDone T_NumWaits=~B\n", [T_NumWaits]),
    receive
        {'EXIT', M_Exiter, M_Reason} ->
            case M_Exiter of
                T_PeriodicWasteProc ->
                    io:fwrite("blinkowaster:waitForPeriodicWasteDone receive from A_PeriodicWasteProc EXIT M_Exiter~w, M_Reason= ~w\n", [M_Exiter, M_Reason]);
                {_, _, _} ->
                    io:fwrite("blinkowaster:waitForPeriodicWasteDone receive from UNKNOWN EXIT M_Exiter~w, M_Reason= ~w\n", [M_Exiter, M_Reason])
            end,
            shutdownPeriodicWaste(T_PeriodicWasteProc),
            io:fwrite("blinkowaster:waitForPeriodicWasteDone EXITING\n"),
            timer:sleep(1000),
            exit(normal);
        {_, _, _} ->
            io:fwrite("blinkowaster:waitForPeriodicWasteDone receive _\n"),
            waitForPeriodicWasteDone(T_PeriodicWasteProc, T_NumWaits + 1)
    end.


shutdownPeriodicWaste(T_PeriodicWasteProc) ->
    io:fwrite("blinkowaster:shutdownAll requesting exit from PeriodicWaste\n"),
    try
        exit(T_PeriodicWasteProc, kill)
    catch
        {_, _, _} -> ok
    end.




periodicWaste(0, _, _, _, _) ->
    io:fwrite("blinkowaster:periodicWaste 0 EXITING\n"),
    exit(normal);

periodicWaste(-1, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time) ->
    io:fwrite("blinkowaster:periodicWaste FOREVER\n"),
    periodicWasteNextNumRunsLeft(-1, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time);

periodicWaste(T_NumRunsLeft, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time) ->
    io:fwrite("blinkowaster:periodicWaste T_NumRunsLeft=~B\n", [T_NumRunsLeft]),
    periodicWasteNextNumRunsLeft(T_NumRunsLeft - 1, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time).


periodicWasteNextNumRunsLeft(T_NextNumRunsLeft, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time) ->
    waste(T_Threads, T_CpuMaxPrime, T_Time),
    timer:sleep(T_DelayBetweenRuns),
    periodicWaste(T_NextNumRunsLeft, T_DelayBetweenRuns, T_Threads, T_CpuMaxPrime, T_Time).





waste(T_Threads, T_CpuMaxPrime, T_Time) ->
    io:fwrite("blinkowaster:waste T_Threads=~B, T_CpuMaxPrime=~B, T_Time=~B\n", [T_Threads, T_CpuMaxPrime, T_Time]),
    
    A_WasteCommand = "sysbench --test=cpu --num-threads=" ++ integer_to_list(T_Threads) ++ " " ++
        "--cpu-max-prime=" ++ integer_to_list(T_CpuMaxPrime) ++ " " ++
        "--max-time=" ++ integer_to_list(T_Time) ++ "  run",
    io:fwrite("blinkowaster:waste ABOUT TO A_WasteCommand=~p\n", [A_WasteCommand]),
    
    A_WasteCommandOutput = os:cmd(A_WasteCommand),
    ?WASTEOUTPUTLOGGER.

