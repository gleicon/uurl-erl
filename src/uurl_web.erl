%% @author gleicon <gleicon@gmail.com>
%% @copyright 2010 gleicon.

%% @doc Web server for uurl.

-module(uurl_web).
-author('gleicon <gleicon@gmail.com>').

-export([start/1, stop/0, loop/2]).
-define (STATS_RESOURCE, "stats/").

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,

	erltl:compile(DocRoot ++ "/stats.html", [{outdir, DocRoot},
			       report_errors, report_warnings, nowarn_unused_vars]),
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "main.css" ->
                    Req:serve_file("main.css", DocRoot);
                "jquery.sparkline.min.js" ->
                    Req:serve_file("jquery.sparkline.min.js", DocRoot);
                "favicon.ico" ->
                    Req:serve_file("favicon.ico", DocRoot);
                "index.html" ->
                    Req:serve_file("index.html", DocRoot);
	        [] ->
		    Req:serve_file("index.html", DocRoot);
	        _ ->
		    case string:str(Path, "stats") of
		        1 ->
			    UID = string:sub_string(Path, length(?STATS_RESOURCE)+1),
			    Stats = uurl_db:stats(UID),
                            case proplists:get_value("jsoncallback", Req:parse_qs()) of
                                undefined ->
                                    Req:ok({"text/plain", [], [Stats]});
                                Val ->
                                    Req:ok({"text/plain", [], [Val++"("++Stats++")"]})			
                            end;
                        _ ->
                            case lists:last(Path) of 
                                $! ->
                                    %% ! at the end of URL means stats
                                    O = stats:render(string:strip(Path, right, $!)),
                                    Req:ok({"text/html", [], O});

                                _ ->
                                    case uurl_db:translate_url(Path, Req:get_header_value("HTTP_REFERER"), Req:get(peer)) of
                                        not_found ->
                                            Req:not_found();
                                        Url ->
                                            Req:respond({302, [{"Location", Url}, 
                                                {"Content-Type", "text/html; charset=UTF-8"}],""})
                            end
                        end
                    end
		end;
        'POST' ->
            case Path of
                _ ->
                    Params = Req:parse_post(),
	            U = proplists:get_value("u", Params),
	            case {string:str(U, "http://"), string:str(U, "https://")} of
	                {0,0} ->
	                    URL = "http://" ++ U;
		        _ ->
		            URL = U
		    end,
						
	            case uurl_db:check_url(URL) of 
		        nil ->
		            {ok, UID} = uurl_db:create(URL);
		        Id ->
		            {ok, UID} = uurl_db:update(Id, Req:get_header_value("HTTP_REFERER"), Req:get(peer))
		    end,
     
                    O = stats:render(UID),
                    Req:ok({"text/html", [], O})
							
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.

