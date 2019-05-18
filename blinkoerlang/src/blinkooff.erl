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
-module(blinkooff).
-author("antonio.carrasco.valero").

%% API
-export([start/0]).

-define(BLINKOPYTHONPATH, "../blinkopython/blinkopython-show").


start() ->
    io:fwrite("blinkooff02:start\n"),
    PyProc = start_python(),
    switchOff(PyProc).

start_python() ->
    io:fwrite("blinkooff02:start_python\n"),
    {ok, PyProc} = python:start([{python_path, filename:absname(?BLINKOPYTHONPATH)}]),
    PyProc.

stop_python(ThePyProc) ->
    io:fwrite("blinkooff02:stop_python\n"),
    python:stop(ThePyProc).

switchOff(ThePyProc) ->
    io:fwrite("blinkooff02:switchOff\n"),
    python:call(ThePyProc, 'TelemetryLED', 'telemetryToLEDshow', [[0, 0]]),
    io:fwrite("blinkooff02:switchOff DONE\n"),
    stop_python(ThePyProc),
    io:fwrite("blinkooff02:switchOff EXITING\n"),
    exit(normal).

