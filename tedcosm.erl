%%
%% tedcosm
%%

-module(tedcosm).

-include("tedcosm.hrl").

-export([start/0, get_and_upload/1]).

-include_lib("xmerl/include/xmerl.hrl").

start() ->
    inets:start(),
    loop(0).

loop(Count) ->
    debug_log("Count = ~p~n", [Count]),
    timer:apply_after(?UPLOAD_RATE, tedcosm, get_and_upload, [Count]).

get_and_upload(Count) ->
    {Return, {_, _, Body}} =
	httpc:request(?TED_URL),
    if Return =:= ok ->
	    Power = get_power(Body),
	    upload_power(Power);
       true ->
	    debug_log("Unable to get TED data")
    end,
    loop(Count + 1).

%
% Get a suitable timestamp using the underlying OS date command.  Timestamp is
% given in UTC.
%
timestamp() ->
    % remove trailing \n
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
    {Return, {StatusLine, _, Body}} = httpc:request(post, {PostUrl,
							  [],
							  "application/json",
							  Json},
						   [], []),
    debug_log("return = ~p~p~p~n", [Return, StatusLine,Body]).

debug_log(Format) ->
    debug_log(Format, []).

debug_log(Format, Args) ->
    case ?DEBUG of
	1 -> io:format(Format, Args);
	0 -> ok
    end.
			    


