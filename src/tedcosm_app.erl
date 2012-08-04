-module(tedcosm_app).

-include("include/tedcosm.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_link/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    tedcosm_sup:start_link().

stop(_State) ->
    ok.

start_link() ->
    inets:start(),
    debug_log("Spawning TEDCOSM"),
    loop().

loop() ->
    receive _ -> ok
    after ?UPLOAD_RATE ->
	    get_and_upload(),
	    loop()
    end.

get_and_upload() ->
    {Return, {_, _, Body}} = httpc:request(?TED_URL),
    if Return =:= ok ->
	    Power = get_power(Body),
	    upload_power(Power);
       true ->
	    debug_log("Unable to get TED data")
    end.

%
% Get a suitable timestamp using the underlying OS date command.  Timestamp is
% given in UTC.
%
timestamp() ->
    % remove trailing \n (0x0a, or 10, is the ASCII code for \n)
    string:strip(os:cmd("date -u '+%Y-%m-%dT%TZ'"), right, 10).

%
% Retrieve the PowerNow element value
%
get_power(Body) ->
    {Document, _} = xmerl_scan:string(Body),
    [PowerText|_] = xmerl_xpath:string("//LiveData/Power/Total/PowerNow/text()", Document),
    PowerText#xmlText.value.
    
%
% Upload power value to COSM
%
upload_power(Power) ->
    Json = "{\"datapoints\":[{\"at\":\"" ++ timestamp() ++ "\", \"value\": \"" ++ Power ++ "\"}]}",
    debug_log("Uploading ~p to COSM~n", [Json]),
    PostUrl = ?COSM_URL ++ "?key=" ++ ?COSM_API_KEY,
    httpc:request(post, {PostUrl,
			 [],
			 "application/json",
			 Json},
		  [], []).

debug_log(Format) ->
    debug_log(Format, []).

debug_log(Format, Args) ->
    case ?DEBUG of
	1 -> io:format(Format, Args);
	0 -> ok
    end.
			    


