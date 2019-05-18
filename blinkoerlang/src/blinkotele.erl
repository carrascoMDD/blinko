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
-module(blinkotele).
-author("antonio.carrasco.valero").

%% API
-export([start/1]).

-define(BLINKOPYTHONPATH, "../blinkopython/blinkopython-show").


start(T_Telemetry) ->
    io:fwrite("blinkotele02:start\n"),
    PyProc = start_python(),
    sendTelemetry(PyProc, T_Telemetry).

start_python() ->
    io:fwrite("blinkotele02:start_python\n"),
    {ok, PyProc} = python:start([{python_path, filename:absname(?BLINKOPYTHONPATH)}]),
    PyProc.

stop_python(ThePyProc) ->
    io:fwrite("blinkotele02:stop_python\n"),
    python:stop(ThePyProc).

sendTelemetry(ThePyProc, T_Telemetry) ->
    io:fwrite("blinkotele02:switchOff\n"),
    python:call(ThePyProc, 'TelemetryLED', 'telemetryToLEDshow', [T_Telemetry]),
    io:fwrite("blinkotele02:switchOff DONE\n"),
    stop_python(ThePyProc),
    io:fwrite("blinkotele02:switchOff EXITING\n"),
    exit(normal).

