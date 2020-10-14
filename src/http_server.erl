%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc http_server.

-module(http_server).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the http_server server.
start() ->
    http_server_deps:ensure(),
    ensure_started(crypto),
    application:start(http_server).


%% @spec stop() -> ok
%% @doc Stop the http_server server.
stop() ->
    application:stop(http_server).
