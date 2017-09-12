yaws_api_ext
============

Yaws is an HTTP
server/library for Erlang. If you are not using it you are likely not
very interested in this library.
This library is a collection of some conveniant additions to the Yaws api
(http://yaws.hyber.org/yman.yaws?page=yaws.api).

Additions in this library
-------------------------
handle_large_body - Large bodies sent to Yaws are split up into
several calls to your out function in your
appmod. Typically you end up implementing some state machine for this like:

```erlang
out(#arg{state=undefined, clidata = Data} = A)
  when is_binary(Data) -> %% size of post < partial_post_size
    InitialState = init_state(),
    finalize_upload(process_data(Data, InitialState), A);


out(#arg{state=undefined, clidata = {partial, Data}})
  when is_binary(Data) -> %% first piece of chunked upload
    InitialState = init_state(),
    {get_more, undefined, process_data(Data, InitialState)};


out(#arg{state = State, clidata = {partial, Data}})
  when is_binary(Data) -> %% a piece in a chunked upload, neither first nor last
    {get_more, undefined, process_data(Data, State)};


out(#arg{state = State, clidata = Data} = A)
  when is_binary(Data) -> %% last piece of chunked upload
    finalize_upload(process_data(Data, State), A);

```

If you have many different places where you handle large uploads like
these you realize you are copying and pasting these four clauses over
and over. Therefore a conveniant layer was introduced for this in this
library called handle_large_body.

So instead of the four clauses above you would write one clause with
something like:
```erlang
out(A) ->
  yaws_api_ext:handle_large_body(A, undef, fun init_state/2, fun process_data/4, fun finalize_upload/3).
```


The arguments to handle_large_body/5 are:
- UserData (this is any user data that the user wants to be added as
argument to the three callback functions)
- A (the Yaws arg record of the request)
- init_state (should return the state you need to keep between
different chunks - this call takes the Yaws arg and the UserArgs as
argument)
- process_data (should process the incoming data chunk and return the
next state - this function receives the data and the state as arguments)
- finalize_upload (this is called when there will be no chunks
delivered and we have the final state - the argument is the Final
State).

Example:
```erlang
-record(test_state, {chunks = []}).

out(A) ->
    yaws_api_ext:handle_large_body(A, undefined,

                                   fun(_, _) ->
                                           #test_state{}
                                   end,

                                   fun(D, #test_state{chunks = Cs} = S) ->
                                           S#test_state{chunks = [D | Cs]}
                                   end,

                                   fun(#test_state{chunks = Cs}) ->
                                           file:write_file("/tmp/out.txt", lists:reverse(Cs)),
                                           {status, 200}
                                   end).
```


There is also a handle_large_body/4 that you can use if your initial
state does not have to be computed and can be given directly. Then you
do not supply the UserArg and instead of a fun to compute the initial
state you simply supply your initial state.

Example:
```erlang
-record(test_state, {chunks = []}).

out(A) ->
    yaws_api_ext:handle_large_body(A, #test_state{},

                                   fun(D, #test_state{chunks = Cs} = S) ->
                                           S#test_state{chunks = [D | Cs]}
                                   end,

                                   fun(#test_state{chunks = Cs}) ->
                                           file:write_file("/tmp/out.txt", lists:reverse(Cs)),
                                           {status, 200}
                                   end).
```
