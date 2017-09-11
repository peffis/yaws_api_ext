-module(yaws_api_ext). 

-export([handle_large_body/5]).

-include_lib("yaws/include/yaws_api.hrl").


%% the body was not contained in the initial chunk 
handle_large_body(UserArgs, #arg{state=undefined, clidata=D} = YawsArg, 
		  InitState, ProcessPiece, Finalize) when is_binary(D) ->
    
    InitialState = InitState(D, YawsArg, UserArgs),
    FinalState = ProcessPiece(D, InitialState, UserArgs, YawsArg),
    Finalize(FinalState, UserArgs, YawsArg);
    
%% first part of the body
handle_large_body(UserArgs, 
		  #arg{state=undefined, clidata={partial, D}} = YawsArg, 
		  InitState, ProcessPiece, _Finalize) when is_binary(D) ->

    InitialState = InitState(D, YawsArg, UserArgs),
    NextState = ProcessPiece(D, InitialState, UserArgs, YawsArg),
    {get_more, undefined, NextState};

%% piece of upload, neither first nor last
handle_large_body(UserArgs, 
		  #arg{state=State, clidata={partial, D}} = YawsArg, 
		  _, ProcessPiece, _) when is_binary(D) ->
    NextState = ProcessPiece(D, State, UserArgs, YawsArg),
    {get_more, undefined, NextState};

%% last piece of upload
handle_large_body(UserArgs, 
		 #arg{state=State, clidata=D} = YawsArg, 
		 _, ProcessPiece, Finalize) when is_binary(D) ->
    FinalState = ProcessPiece(D, State, UserArgs, YawsArg),
    Finalize(FinalState, UserArgs, YawsArg). 



    
