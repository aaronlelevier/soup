%%%-------------------------------------------------------------------
%% @doc soup public API
%% @end
%%%-------------------------------------------------------------------

-module(soup_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    soup_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
