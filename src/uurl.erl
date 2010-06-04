%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(uurl).
-author('gleicon <gleicon@gmail.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the uurl server.
start() ->
    uurl_deps:ensure(),
    ensure_started(crypto),
	ensure_started(emongo),
	ensure_started(erldis),
	emongo:add_pool(uurl_pool, "localhost", 27017, "uurl", 1),
    application:start(uurl).

%% @spec stop() -> ok
%% @doc Stop the uurl server.
stop() ->
    Res = application:stop(uurl),
	application:stop(erldis),
	application:stop(emongo),
    application:stop(crypto),
    Res.
