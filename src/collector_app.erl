-module(collector_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("Starting...~n"),
    {ok, _} = application:ensure_all_started(inets),
    collector_sup:start_link().

stop(_State) ->
    ok.
