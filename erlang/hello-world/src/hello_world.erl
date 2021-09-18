-module(hello_world).

-export([hello/0]).

-spec hello() -> string().
hello() ->
    "Hello, World!".
