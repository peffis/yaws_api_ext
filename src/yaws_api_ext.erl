-module(yaws_api_ext).

-export([handle_large_body/4]).
-export([handle_large_body/5]).

-include_lib("yaws/include/yaws_api.hrl").


%% the body was not contained in the initial chunk
handle_large_body(#arg{state=undefined, clidata=D}, InitialState, ProcessPiece, Finalize)
  when is_binary(D) ->

    Finalize(ProcessPiece(D, InitialState));

%% first part of the body
handle_large_body(#arg{state=undefined, clidata={partial, D}}, InitialState,
		  ProcessPiece, _Finalize) when is_binary(D) ->

    {get_more, undefined, ProcessPiece(D, InitialState)};

%% piece of upload, neither first nor last
handle_large_body(#arg{state=State, clidata={partial, D}}, _, ProcessPiece, _) when is_binary(D) ->

    {get_more, undefined, ProcessPiece(D, State)};

%% last piece of upload
handle_large_body(#arg{state=State, clidata=D}, _, ProcessPiece, Finalize) when is_binary(D) ->

    Finalize(ProcessPiece(D, State)).



%% first and last part of the body
handle_large_body(#arg{state=undefined, clidata=D} = YawsArg, UserArg,
		  InitState, ProcessPiece, Finalize) when is_binary(D) ->

    InitialState = InitState(YawsArg, UserArg),
    Finalize(ProcessPiece(D, InitialState));

%% first part of the body
handle_large_body(#arg{state=undefined, clidata={partial, D}} = YawsArg, UserArg,
		  InitState, ProcessPiece, _Finalize) when is_binary(D) ->

    InitialState = InitState(YawsArg, UserArg),
    {get_more, undefined, ProcessPiece(D, InitialState)};

%% piece of upload, neither first nor last
handle_large_body(#arg{state=State, clidata={partial, D}}, _, _, ProcessPiece, _) when is_binary(D) ->

    {get_more, undefined, ProcessPiece(D, State)};

%% last piece of upload
handle_large_body(#arg{state=State, clidata=D}, _, _, ProcessPiece, Finalize) when is_binary(D) ->

    Finalize(ProcessPiece(D, State)).
