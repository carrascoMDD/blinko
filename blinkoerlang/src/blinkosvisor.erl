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
%%%
%%% @end
%%% Created : 201905121555
%%%-------------------------------------------------------------------
-module(blinkosvisor).
-author("antonio.carrasco.valero").

%% A supervisor launching a sensor and a show. Shutdowns all if any exits.
%% Pass arguments with #iteration or #interations left for sensor to produce an arithmethic error or show to stop
%% Trial exits
%% Launch a few wasters
%% Launch on specific nodes on same host
%% Show can be configured by a message while falling back to inlined constant defaults
%% Show displays telemetries on multiple segments of the LEDs strip
%% TODO Launch multiple sensors

%% API
-export([start/6]).

-import(timer, [sleep/1]).

a() ->
    "
    # in acvp08u18 192.168.69.82
    cd ~/Works/MDD/Erlang/blinko01/commands/development
    ./cperlporttohost3789.sh
    ./cptohost3789.sh
    
    # in each asset node sensor host
    # in mddhost8 192.168.69.14
    sudo apt-get install python-pip
    UNESCAPEQUOTES: export LC_ALL=\"en_US.UTF-8\"
    UNESCAPEQUOTES: export LC_CTYPE=\"en_US.UTF-8\"
    sudo dpkg-reconfigure locales
    
    pip install psutil
    
    # RUN LOCAL IN DEVELOPER's workstation acvp08u18 192.168.69.82
    # in  acvp08u18 192.168.69.82
    
    # Three consoles
    cd ~/Works/MDD/Erlang/blinko01/erlang/blinkoerlang01
    erl -sname asset -setcookie blinkocookie
    
    cd ~/Works/MDD/Erlang/blinko01/erlang/blinkoerlang01
    erl -sname dash -setcookie blinkocookie
    
    cd ~/Works/MDD/Erlang/blinko01/erlang/blinkoerlang01
    erl -sname svisor -setcookie blinkocookie
    blinkosvisor:start(local, local, 4,1000000, 1000000, [[300,2000,1,20000,1],[200,3000,1,20000,1],[120,4500,1,20000,1],[100,5000,1,20000,1],[600,1000,1,20000,1],[450,2500,1,20000,1],[410,1700,1,20000,1],[310,3300,1,20000,1],[210,2300,1,20000,1],[410,2200,1,20000,1]]).
    
    
    
    # From IntelliJ
    local local 4 1000000  1000000  [[6,2000,1,20000,1]]
    
    
    
    
    # RUN DISTRIBUTED
    
    # in mddhost7 192.168.69.13
    ssh occam01@mddhost7
    cd ~/Works/MDD/Erlang/blinko01/erlang/blinkoerlang01
    erl -sname boot -setcookie blinkocookie
    c(blinkosvisor).
    c(blinkosensor02).
    c(blinkoshow02).
    c(blinkowaster).
    % serve implementations to hosts mddhost3, mddhost7, mddhost8, mddhost9, acvp08u18
    erl_boot_server:start(['192.168.69.3','192.168.69.13','192.168.69.14','192.168.69.15','192.168.69.82']).
    
    
    # Better use acvp08u18 as asset than mddhost8 192.168.69.14
    
    #   # in mddhost8 192.168.69.14
    #   ssh occam01@mddhost8
    #   cd ~/Works/MDD/Erlang/blinko01/erlang/blinkoerlang01
    #   erl -sname asset -setcookie blinkocookie -loader inet -hosts 192.168.69.13
    
    # in acvp08u18 192.168.69.82
    ssh acv@acvp08u18
    cd ~/Works/MDD/Erlang/blinko01/erlang/blinkoerlang01
    
    # Can not load from boot node because of different erlang version
    #   {erl_prim_loader,'no server found'}
    # erl -sname asset -setcookie blinkocookie -loader inet -hosts 192.168.69.13
    cd ~/Works/MDD/Erlang/blinko01/erlang/blinkoerlang01
    erl -sname asset -setcookie blinkocookie
    
    
    
    # in mddhost3 192.168.69.3
    ssh occam01@mddhost3
    cd ~/Works/MDD/Erlang/blinko01/erlang/blinkoerlang01
    erl -sname dash -setcookie blinkocookie -loader inet -hosts 192.168.69.13
    
    
    blinkotele02:start([0]).
    blinkotele02:start([0]).
    
    blinkotele02:start([100]).
    blinkotele02:start([100]).
    
    
    
    # in mddhost9 192.168.69.15
    ssh occam01@mddhost9
    cd ~/Works/MDD/Erlang/blinko01/erlang/blinkoerlang01
    erl -sname svisor -setcookie blinkocookie -loader inet -hosts 192.168.69.13
    # not asset on mddhost8 but acvp08u18 blinkosvisor:start(dash@mddhost3, asset@mddhost8, 600,1000000, 1000000, [[300,2000,4,20000,1],[200,3000,4,20000,1],[120,4500,3,20000,1],[100,5000,4,20000,1],[600,1000,4,20000,1],[450,2500,4,20000,1]]).
    blinkosvisor:start(dash@mddhost3, asset@acvp08u18, 600,1000000, 1000000, [[300,2000,4,20000,1],[200,3000,4,20000,1],[120,4500,3,20000,1],[100,5000,4,20000,1],[600,1000,4,20000,1],[450,2500,4,20000,1]]).
    blinkosvisor:start(dash@mddhost3, asset@acvp08u18, 600,1000000, 1000000, [[300,2000,1,20000,1],[200,3000,1,20000,1],[120,4500,1,20000,1],[100,5000,1,20000,1],[600,1000,1,20000,1],[450,2500,1,20000,1],[410,1700,1,20000,1],[310,3300,1,20000,1],[210,2300,1,20000,1],[410,2200,1,20000,1]]).
    blinkosvisor:start(dash@mddhost3, asset@acvp08u18, 10,1000000, 1000000, [[300,2000,1,20000,1],[200,3000,1,20000,1],[120,4500,1,20000,1],[100,5000,1,20000,1],[600,1000,1,20000,1],[450,2500,1,20000,1],[410,1700,1,20000,1],[310,3300,1,20000,1],[210,2300,1,20000,1],[410,2200,1,20000,1]]).
    
    
    
    ".

% local local 100 1000000  1000000  [[100,1000,1,20000,1]]
% local local 600 1000000  1000000 [[300,2000,1,20000,1],[200,3000,1,20000,1],[120,4500,1,20000,1],[100,5000,1,20000,1],[600,1000,1,20000,1],[450,2500,1,20000,1],[410,1700,1,20000,1],[310,3300,1,20000,1],[210,2300,1,20000,1],[410,2200,1,20000,1]]

start(T_DisplayNode, T_MonitoredNode, T_NumIterations, T_ShowStopAtCount, T_SensorBadarithWhenLeft, T_AllWasters) ->
    io:fwrite("blinkosvisor:start T_NumIterations=~B, T_ShowStopAtCount=~B, T_SensorBadarithWhenLeft=~B, T_AllWasters=~w\n", [T_NumIterations, T_ShowStopAtCount, T_SensorBadarithWhenLeft, T_AllWasters]),
    process_flag(trap_exit, true),
    launch(T_DisplayNode, T_MonitoredNode, T_NumIterations, T_ShowStopAtCount, T_SensorBadarithWhenLeft, T_AllWasters).


launch(T_DisplayNode, T_MonitoredNode, T_NumIterations, T_ShowStopAtCount, T_SensorBadarithWhenLeft, T_AllWasters) ->
    A_ShowProc = launchShowProcess(T_DisplayNode, T_ShowStopAtCount),
    A_SensorProc = launchSensorProcess(A_ShowProc, T_MonitoredNode, T_NumIterations, T_SensorBadarithWhenLeft),
    A_WasterProc = launchWasterProcess(T_MonitoredNode, T_AllWasters),
    timer:sleep(300),
    configure(A_ShowProc, A_SensorProc, A_WasterProc),
    timer:sleep(300),
    handling(A_ShowProc, A_SensorProc, A_WasterProc).


launchShowProcess(T_DisplayNode, T_ShowStopAtCount) ->
    case T_DisplayNode of
        local ->
            A_ShowProcLocal = spawn_link(blinkoshow, start, [T_ShowStopAtCount]),
            io:fwrite("blinkosvisor:launchShowProcess blinkoshow LOCAL spawn_link done T_DisplayNode=~w, A_ShowProc=~w\n", [T_DisplayNode, A_ShowProcLocal]),
            A_ShowProcLocal;
        _ ->
            A_ShowProcRemote = spawn_link(T_DisplayNode, blinkoshow, start, [T_ShowStopAtCount]),
            io:fwrite("blinkosvisor:launchShowProcess blinkoshow REMOTE spawn_link done T_DisplayNode=~w, A_ShowProc=~w\n", [T_DisplayNode, A_ShowProcRemote]),
            A_ShowProcRemote
    end.

launchSensorProcess(T_ShowProc, T_MonitoredNode, T_NumIterations, T_SensorBadarithWhenLeft) ->
    case T_MonitoredNode of
        local ->
            A_SensorProcLocal = spawn_link(blinkosensor, start, [T_ShowProc, <<"SegmentONE">>, true, T_NumIterations, T_SensorBadarithWhenLeft]),
            io:fwrite("blinkosvisor:launchSensorProcess blinkosensor LOCAL spawn_link done T_MonitoredNode=~w, A_SensorProc=~w\n", [T_MonitoredNode, A_SensorProcLocal]),
            A_SensorProcLocal;
        _ ->
            A_SensorProcRemote = spawn_link(T_MonitoredNode, blinkosensor, start, [T_ShowProc, <<"SegmentONE">>, true, T_NumIterations, T_SensorBadarithWhenLeft]),
            io:fwrite("blinkosvisor:launchSensorProcess blinkosensor REMOTE spawn_link done T_MonitoredNode=~w, A_SensorProc=~w\n", [T_MonitoredNode, A_SensorProcRemote]),
            A_SensorProcRemote
    end.

launchWasterProcess(T_MonitoredNode, T_AllWasters) ->
    case T_MonitoredNode of
        local ->
            A_WasterProcLocal = spawn_link(blinkowaster, start, [T_AllWasters]),
            io:fwrite("blinkosvisor:launch launchWasterProcess LOCAL spawn_link done T_MonitoredNode=~w, A_WasterProc=~w, T_AllWasters=~w\n", [T_MonitoredNode, A_WasterProcLocal, T_AllWasters]),
            A_WasterProcLocal;
        _ ->
            A_WasterProcRemote = spawn_link(T_MonitoredNode, blinkowaster, start, [T_AllWasters]),
            io:fwrite("blinkosvisor:launch launchWasterProcess REMOTE spawn_link done T_MonitoredNode=~w, A_WasterProc=~w, T_AllWasters=~w\n", [T_MonitoredNode, A_WasterProcRemote, T_AllWasters]),
            A_WasterProcRemote
    end.



configure(T_ShowProc, T_SensorProc, T_WasterProc) ->
    configureShowProcess(T_ShowProc).


configureShowProcess(T_ShowProc) ->
    A_Configuration = defaultShowConfiguration(),
    io:fwrite("blinkosvisor:configShowProcess A_Configuration=~w\n", [A_Configuration]),
    T_ShowProc ! {configuration, A_Configuration},
    
    A_Segments = defaultShowSegments(),
    io:fwrite("blinkosvisor:configShowProcess A_Segments=~w\n", [A_Segments]),
    configureShowSegments(T_ShowProc, A_Segments).

defaultShowConfiguration() ->
    % [ M_NumLEDs, M_LEDsCommSpeed, M_LEDsMasterBrigtness, M_TelemetryToBrigtnessFactor, M_SortTelemetry, M_Transitions]
    % [ 144, 10, 64, 0.25, true, true]
    [144, 10, 64, 0.25, false, true].

defaultShowSegments() ->
    %  [ M_SegmentName, M_NumLEDs] ->
    [[<<"SegmentONE">>, 144], [<<"SegmentTWO">>, 72]].


configureShowSegments(T_ShowProc, []) ->
    ok;
configureShowSegments(T_ShowProc, [T_Segment | T_RemainingSegments]) ->
    io:fwrite("blinkosvisor:configureShowSegment T_Segment=~w\n", [T_Segment]),
    T_ShowProc ! {segment, T_Segment},
    configureShowSegments(T_ShowProc, T_RemainingSegments).



handling(T_ShowProc, T_SensorProc, T_WasterProc) ->
    receive
        {'EXIT', M_Exiter, M_Reason = {badarith, _}} ->
            case M_Exiter of
                T_ShowProc ->
                    io:fwrite("blinkosvisor:handling receive BADARITH from T_ShowProc EXIT M_Exiter~w, M_Reason=~w\n", [M_Exiter, M_Reason]);
                T_SensorProc ->
                    io:fwrite("blinkosvisor:handling receive UNEXPECTED BADARITH from T_SensorProc EXIT M_Exiter~w, M_Reason=~w\n", [M_Exiter, M_Reason]);
                {_, _, _} ->
                    io:fwrite("blinkosvisor:handling receive BADARITH from UNKNOWN EXIT M_Exiter~w, M_Reason=~w\n", [M_Exiter, M_Reason])
            end,
            shutdownAll(T_ShowProc, T_SensorProc, T_WasterProc),
            io:fwrite("blinkosvisor:handling BADARITH EXITING\n"),
            timer:sleep(1000),
            exit(normal);
        {'EXIT', M_Exiter, M_Reason} ->
            case M_Exiter of
                T_ShowProc ->
                    io:fwrite("blinkosvisor:handling receive from T_ShowProc EXIT M_Exiter~w, M_Reason= ~w\n", [M_Exiter, M_Reason]);
                T_SensorProc ->
                    io:fwrite("blinkosvisor:handling receive from T_SensorProc EXIT M_Exiter~w, M_Reason= ~w\n", [M_Exiter, M_Reason]);
                T_WasterProc ->
                    io:fwrite("blinkosvisor:handling receive from T_WasterProc EXIT M_Exiter~w, M_Reason= ~w\n", [M_Exiter, M_Reason]);
                {_, _, _} ->
                    io:fwrite("blinkosvisor:handling receive from UNKNOWN EXIT M_Exiter~w, M_Reason= ~w\n", [M_Exiter, M_Reason])
            end,
            shutdownAll(T_ShowProc, T_SensorProc, T_WasterProc),
            io:fwrite("blinkosvisor:handling EXITING\n"),
            timer:sleep(1000),
            exit(normal);
        {_, _, _} ->
            io:fwrite("blinkosvisor:handling receive _\n"),
            handling(T_ShowProc, T_SensorProc, T_WasterProc)
    end.

shutdownAll(T_ShowProc, T_SensorProc, T_WasterProc) ->
    io:fwrite("blinkosvisor:shutdownAll sending finished to WasterProc\n"),
    try
        T_WasterProc ! finished
    catch
        {_, _, _} -> ok
    end,
    io:fwrite("blinkosvisor:shutdownAll sending finished to ShowProc\n"),
    try
        T_ShowProc ! finished
    catch
        {_, _, _} -> ok
    end,
    io:fwrite("blinkosvisor:shutdownAll requesting exit from SensorProc\n"),
    try
        exit(T_SensorProc, kill)
    catch
        {_, _, _} -> ok
    end.