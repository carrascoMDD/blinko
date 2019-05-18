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
-module(blinkoshow).
-author("antonio.carrasco.valero").

% Interprets received configuration commands for the underlying python TelemetryLED
% Interprets received Segment commands for the underlying python TelemetryLED


%% API
-export([start/1]).

-define(BLINKOPYTHONPATH, "../blinkopython/blinkopython-show").

start(T_StopAtCount) ->
    io:fwrite("blinkoshow:start T_StopAtCount=~B\n", [T_StopAtCount]),
    PyProc = start_python(),
    showLoop(PyProc, T_StopAtCount, 0).

start_python() ->
    io:fwrite("blinkoshow:start_python\n"),
    {ok, PyProc} = python:start([{python_path, filename:absname(?BLINKOPYTHONPATH)}]),
    PyProc.

stop_python(ThePyProc) ->
    io:fwrite("blinkoshow:stop_python\n"),
    python:stop(ThePyProc).

showLoop(ThePyProc, T_StopAtCount, T_Count) ->
    io:fwrite("blinkoshow:showLoop T_Count=~B, T_StopAtCount=~B\n", [T_Count, T_StopAtCount]),
    if
        T_Count == T_StopAtCount ->
            io:fwrite("blinkoshow:showLoop 2 ; closing python\n"),
            stop_python(ThePyProc),
            io:fwrite("blinkoshow:showLoop 2 EXITING\n"),
            exit(normal);
        true ->
            ok
    end,
    receive
        {telemetry, M_Telemetry} ->
            io:fwrite("blinkoshow:showLoop received telemetry ~B, M_Telemetry=~w\n", [T_Count, M_Telemetry]),
            python:call(ThePyProc, 'TelemetryLED', 'telemetryToLEDshow', [M_Telemetry]),
            showLoop(ThePyProc, T_StopAtCount, T_Count + 1);
        {sensortelemetry, M_SensorName, M_Telemetry} ->
            io:fwrite("blinkoshow:showLoop received sensortelemetry ~B, M_SensorName=~w, M_Telemetry=~w\n", [T_Count, M_SensorName, M_Telemetry]),
            A_SegmentName = segmentNameFromSensorName(M_SensorName),
            python:call(ThePyProc, 'TelemetryLED', 'telemetryToLEDshowSegment', [A_SegmentName, M_Telemetry]),
            showLoop(ThePyProc, T_StopAtCount, T_Count + 1);
        {configuration, [M_NumLEDs, M_LEDsCommSpeed, M_LEDsMasterBrigtness, M_TelemetryToBrigtnessFactor, M_SortTelemetry, M_Transitions]} ->
            io:fwrite("blinkoshow:showLoop received configuration M_NumLEDs=~w, M_LEDsCommSpeed=~w, M_LEDsMasterBrigtness=~w, M_TelemetryToBrigtnessFactor=~w, M_SortTelemetry=~w, M_Transitions=~w\n", [M_NumLEDs, M_LEDsCommSpeed, M_LEDsMasterBrigtness, M_TelemetryToBrigtnessFactor, M_SortTelemetry, M_Transitions]),
            python:call(ThePyProc, 'TelemetryLED', 'configureLEDshow', [M_NumLEDs, M_LEDsCommSpeed, M_LEDsMasterBrigtness, M_TelemetryToBrigtnessFactor, M_SortTelemetry, M_Transitions]),
            showLoop(ThePyProc, T_StopAtCount, T_Count);
        {configuration, M_WrongConfiguration} ->
            io:fwrite("blinkoshow:showLoop received configuration WRONG M_WrongConfiguration=~w\n", [M_WrongConfiguration]),
            showLoop(ThePyProc, T_StopAtCount, T_Count);
        {configuration, _} ->
            io:fwrite("blinkoshow:showLoop received configuration UNMATCHED configuration\n"),
            showLoop(ThePyProc, T_StopAtCount, T_Count);
        {segment, [M_SegmentName, M_NumLEDs]} ->
            io:fwrite("blinkoshow:showLoop received segment M_SegmentName=~w, M_NumLEDs=~w\n", [M_SegmentName, M_NumLEDs]),
            python:call(ThePyProc, 'TelemetryLED', 'addLEDsSegment', [M_SegmentName, M_NumLEDs]),
            showLoop(ThePyProc, T_StopAtCount, T_Count);
        {segment, M_WrongSegment} ->
            io:fwrite("blinkoshow:showLoop received segment WRONG M_WrongSegment=~w\n", [M_WrongSegment]),
            showLoop(ThePyProc, T_StopAtCount, T_Count);
        {segment, _} ->
            io:fwrite("blinkoshow:showLoop received segment UNMATCHED segment\n"),
            showLoop(ThePyProc, T_StopAtCount, T_Count);
        finished ->
            io:fwrite("blinkoshow:showLoop received finished T_Count=~B, T_StopAtCount=~B\n", [T_Count, T_StopAtCount]),
            stop_python(ThePyProc),
            io:fwrite("blinkoshow:showLoop EXITING\n"),
            exit(normal);
        _ ->
            io:fwrite("blinkoshow:showLoop UNMATCHED ANYTHING\n"),
            showLoop(ThePyProc, T_StopAtCount, T_Count)
    end.


segmentNameFromSensorName(T_SensorName) ->
    T_SensorName.
