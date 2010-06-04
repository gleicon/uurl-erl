%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the uurl application.

-module(uurl_app).
-author('gleicon <gleicon@gmail.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for uurl.
start(_Type, _StartArgs) ->
    uurl_deps:ensure(),
    uurl_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for uurl.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
