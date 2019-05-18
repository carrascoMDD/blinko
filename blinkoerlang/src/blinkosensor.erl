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
-module(blinkosensor).
-author("antonio.carrasco.valero").

%% API
-export([start/5]).

-define(BLINKOPYTHONPATH, "../blinkopython/blinkopython-sensor").
-define(READINGINTERVAL, 1). % Seconds


start(T_ShowProc, T_SensorName, T_SendSensorName, T_NumIterations, T_BadarithWhenLeft) ->
    io:fwrite("blinkosensor:start\n"),
    PyProc = start_python(),
    readingLoop(T_ShowProc, T_SensorName, T_SendSensorName, T_NumIterations, T_BadarithWhenLeft, PyProc).

start_python() ->
    io:fwrite("blinkosensor:start_python\n"),
    {ok, PyProc} = python:start([{python_path, filename:absname(?BLINKOPYTHONPATH)}]),
    PyProc.

stop_python(T_PyProc) ->
    io:fwrite("blinkosensor:stop_python\n"),
    python:stop(T_PyProc).



readingLoop(T_ShowProc, T_SensorName, T_SendSensorName, 0, T_BadarithWhenLeft, T_PyProc) ->
    io:fwrite("blinkosensor:readingLoop 0\n"),
    stop_python(T_PyProc),
    T_ShowProc ! finished,
    io:fwrite("blinkosensor:readingLoop EXITING\n"),
    exit(normal);
readingLoop(T_ShowProc, T_SensorName, T_SendSensorName, T_IterationsLeft, T_BadarithWhenLeft, T_PyProc) ->
    if
        T_IterationsLeft == T_BadarithWhenLeft ->
            io:fwrite("blinkosensor:readingLoop CAUSING BADARITH T_IterationsLeft=~w\n", [T_IterationsLeft]),
            12 / 0;
        true -> ok
    end,
    io:fwrite("blinkosensor:readingLoop T_IterationsLeft=~w\n", [T_IterationsLeft]),
    Reading = python:call(T_PyProc, 'TelemetrySensor', 'telemetrySensorRead', [?READINGINTERVAL]),
    if
        T_SendSensorName ->
            T_ShowProc ! {sensortelemetry, T_SensorName, Reading};
        false ->
            T_ShowProc ! {telemetry, Reading}
    end,
    readingLoop(T_ShowProc, T_SensorName, T_SendSensorName, T_IterationsLeft - 1, T_BadarithWhenLeft, T_PyProc).

