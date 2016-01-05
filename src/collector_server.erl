-module(collector_server).
-behaviour(gen_server).
-define(INITIAL_DELAY_MS, 1000).
-define(SERVER, ?MODULE).
%% URL of stat endpoint
-define(STAT_URL, "http://some.url").
-define(STAT_CHECK_INTERVAL_MS, 20000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    io:format("Started collector.~n"),
    erlang:send_after(?INITIAL_DELAY_MS,self(),tick),
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    {ok, RequestId} =
        httpc:request(get,
            {?STAT_URL, []},
            [],
            [{sync, false}]),
    {noreply, State};
handle_info({http, {RequestId, Result}}, State) ->
    print_stats(Result),
    erlang:send_after(?STAT_CHECK_INTERVAL_MS,self(),tick),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

print_stats(Result) ->
    {{_, 200, _}, _, Body} = Result,
    JsonMap = jsx:decode(Body, [return_maps]),
    OpenSessions = maps:get(<<"OPEN">>, maps:get(<<"sessions">>, JsonMap)),
    InQueue = maps:get(<<"INITIAL">>, maps:get(<<"integrationStatus">>, JsonMap)),
    Slots = maps:get(<<"availableAgents">>, JsonMap),
    io:format("~s open sessions: ~w, in queue: ~w, slots: ~w~n",
        [qdate:format("Y-m-d H:i:s",(erlang:localtime())), OpenSessions, InQueue, Slots]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
