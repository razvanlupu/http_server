%% @author Mochi Media <dev@mochimedia.com>
%% @copyright http_server Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the http_server application.

-module(http_server_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for http_server.
start(_Type, _StartArgs) ->
    http_server_deps:ensure(),
    http_server_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for http_server.
stop(_State) ->
    ok.
