
%% @author gleicon <gleicon@gmail.com>
%% @copyright 2010 gleicon.	
%% @doc db handling for uurl.

-module(uurl_db).
-author('gleicon <gleicon@gmail.com>').
-export([create/1, update/3, stats/1, check_url/1, translate_url/3, datetime_helper/1]).

-define (UNIQUE_ID_REF, "URLSHORT:UNIQ").
-define (OBJECT_ID_REF, "OBJID:").
-define(AS_JSON(K, V), bson_to_json(proplists:get_value(K, V))).

create(URL) ->
    Id = create_unique_id(),
    UID = base62:encode(Id),
    Dtnow = calendar:now_to_datetime(now()),
    emongo:insert(uurl_pool, "urls", [{"uuid", Id}, {"uurl", UID}, {"url", URL}, {"clicks", 0}, {"date", Dtnow}]),
    cache_url(URL, UID),
    {ok, UID}.

update(UID, Referer, Visitor) ->
    Id = base62:decode(UID),
    emongo:update(uurl_pool, "urls", [{"uuid", Id}], [
        {"$inc", [{"clicks", 1}]},
        {"$inc", [{["clicks_per_hour", get_current_date()], 1} ]},
        {"$push", [{"referers", Referer}, {"visitors", Visitor} ] }], true),
    {ok, Id}.

stats(UID) -> 
    Id = base62:decode(UID),
    Stats = lists:flatten(emongo:find(uurl_pool, "urls", [{"uuid", Id}])),
    Obj = {struct, [
        {url, ?AS_JSON(<<"url">>, Stats)},
        {clicks, ?AS_JSON(<<"clicks">>, Stats)},
        {cph, ?AS_JSON(<<"clicks_per_hour">>, Stats)},
        {visitors, ?AS_JSON(<<"visitors">>, Stats)},
        {referers, ?AS_JSON(<<"referers">>, Stats)}
            ]},
    mochijson2:encode(Obj).
	

translate_url(UID, Referer, Visitor) ->
    {ok, Id} = update(UID, Referer, Visitor), 
    Stats = emongo:find(uurl_pool, "urls", [{"uuid", Id}]),	
    URL = proplists:get_value(list_to_bitstring("url"), lists:flatten(Stats)),
    bitstring_to_list(URL).


get_current_date() ->
    {Date, Time}=calendar:now_to_datetime(now()),
    DP = lists:map(fun(Dt) -> "." ++ integer_to_list(Dt) end, tuple_to_list(Date)),
    TP = lists:map(fun(Tm) -> "." ++ integer_to_list(Tm) end, tuple_to_list(Time)),
    lists:flatten(DP++lists:sublist(TP, 1, 1)).
	
create_unique_id() ->
    {ok, Client} = erldis:connect("localhost", 6379),
    case erldis:incr(Client, list_to_bitstring(?UNIQUE_ID_REF)) of
            true ->
                    Id2 = 1;
            Id ->
            Id2 = Id
    end,
    erldis_client:stop(Client),
	Id2.
	
check_url(Url) ->
    {ok, Client} = erldis:connect("localhost", 6379),
    case erldis:get(Client, list_to_bitstring(?OBJECT_ID_REF++Url)) of 
            nil ->
                    Val = nil;
            V -> 
                    Val = bitstring_to_list(V)
            end,
    erldis_client:stop(Client),
    Val.
    
cache_url(Url, Eid) ->
    {ok, Client} = erldis:connect("localhost", 6379),
    Val = erldis:set(Client, list_to_bitstring(?OBJECT_ID_REF++Url), Eid),
    erldis_client:stop(Client),
    Val.


datetime_helper(Local_time) when Local_time == [] -> "nan";
datetime_helper(Local_time) ->
        {{Year, Month, Day}, {Hour, Min, Sec}} = Local_time,
            lists:flatten(io_lib:fwrite("~2B/~2B/~4..0B ~2B:~2.10.0B:~2.10.0B",
                                        [Day, Month, Year, Hour, Min, Sec])).

%% BSON2JSON stuff from davisp / freenode's #erlang

bson_to_json(undefined) -> null;
bson_to_json(null) -> null;
bson_to_json(true) -> true;
bson_to_json(false) -> false;
bson_to_json(Val) when is_number(Val) -> Val;
bson_to_json(Val) when is_binary(Val) -> Val;
bson_to_json([{_,_} | _] = Obj) ->
    {struct, bson_to_json_obj(Obj)};
bson_to_json({array, Vals}) ->
    lists:map(fun bson_to_json/1, Vals);
bson_to_json({oid, Value}) ->
    {[{oid, base62:encode(Value)}]}.

bson_to_json_obj([]) -> [];
bson_to_json_obj([{Key, Value} | Rest]) ->
    [{Key, bson_to_json(Value)}] ++ bson_to_json_obj(Rest).

