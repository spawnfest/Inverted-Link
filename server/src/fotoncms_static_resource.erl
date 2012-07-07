%% Copyright 2012 Alexander Tchitchigin
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%       http://www.apache.org/licenses/LICENSE-2.0
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and limitations under the License.


-module(fotoncms_static_resource).
-export([init/1]).
-export([allowed_methods/2,
         resource_exists/2,
         last_modified/2,
         content_types_provided/2,
         provide_content/2,
         generate_etag/2]).

-record(context, {root,response_body=undefined,metadata=[]}).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(ConfigProps) ->
    {root, Root1} = proplists:lookup(root, ConfigProps),
    %{ok, App} = application:get_application(),
    %PrivDir = code:priv_dir(App),
    {ok, Working} = file:get_cwd(),
    PrivDir = filename:join([Working, "priv"]),
    Root = filename:join([PrivDir, Root1]),
    %io:format("init: PrivDir = ~s, Root = ~s~n", [PrivDir, Root]),
    {ok, #context{root=Root}}.
    
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

%file_path(_Context, []) ->
%    false;
file_path(Context, Path) ->
    Root = Context#context.root,
    %io:format("file_path: Root = ~s~n", [Root]),
    case mochiweb_util:safe_relative_path(Path) of
        undefined -> false;
        RelPath ->
            FullPath = filename:join([Root, RelPath]),
	    %io:format("file_path: FullPath = ~s~n", [FullPath]),
            case filelib:is_dir(FullPath) of
                true ->
                    filename:join([FullPath, "index.html"]);
                false ->
                    FullPath
            end
    end.

file_exists(Context, Name) ->
    NamePath = file_path(Context, Name),
    case filelib:is_regular(NamePath) of 
        true ->
            {true, NamePath};
        false ->
            false
    end.

resource_exists(ReqData, Context) ->
    Path = wrq:disp_path(ReqData),
    %io:format("resource_exists: Path = ~s~n", [Path]),
    case file_exists(Context, Path) of 
        {true, _} ->
            {true, ReqData, Context};
        _ ->
            {false, ReqData, Context}
    end.

maybe_fetch_object(Context, Path) ->
    % if returns {true, NewContext} then NewContext has response_body
    case Context#context.response_body of
        undefined ->
            case file_exists(Context, Path) of 
                {true, FullPath} ->
                    {ok, Value} = file:read_file(FullPath),
                    {true, Context#context{response_body=Value}};
                false ->
                    {false, Context}
            end;
        _Body ->
            {true, Context}
    end.

content_types_provided(ReqData, Context) ->
    Path = wrq:disp_path(ReqData),
    CT = case Path of
	     [] -> "text/html";
	     _ -> webmachine_util:guess_mime(Path)
	 end,
    {[{CT, provide_content}], ReqData,
     Context#context{metadata=[{'content-type', CT}|Context#context.metadata]}}.

provide_content(ReqData, Context) ->
    case maybe_fetch_object(Context, wrq:disp_path(ReqData)) of 
        {true, NewContext} ->
            Body = NewContext#context.response_body,
            {Body, ReqData, Context};
        {false, NewContext} ->
            {error, ReqData, NewContext}
    end.

last_modified(ReqData, Context) ->
    {true, FullPath} = file_exists(Context,
                                   wrq:disp_path(ReqData)),
    LMod = filelib:last_modified(FullPath),
    {LMod, ReqData, Context#context{metadata=[{'last-modified',
                    httpd_util:rfc1123_date(LMod)}|Context#context.metadata]}}.

hash_body(Body) -> mochihex:to_hex(binary_to_list(crypto:sha(Body))).

generate_etag(ReqData, Context) ->
    case maybe_fetch_object(Context, wrq:disp_path(ReqData)) of
        {true, BodyContext} ->
            ETag = hash_body(BodyContext#context.response_body),
            {ETag, ReqData,
             BodyContext#context{metadata=[{etag,ETag}|
                                           BodyContext#context.metadata]}};
        _ ->
            {undefined, ReqData, Context}
    end.
