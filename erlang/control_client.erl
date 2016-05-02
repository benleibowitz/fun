-module(control_client).
-export([run/0, client/2]).

run() ->
    lists:foreach(fun(_) ->
            spawn(control_client, client, ['localhost', 35000])
        end,
    lists:seq(1, 5)).

client(Host, Port) ->
    {ok, Sock} = gen_tcp:connect(Host, Port,
                                 [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, "Hello, from TCP client"),
    ok = gen_tcp:close(Sock).
