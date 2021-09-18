-module(series).

-export([slices/2]).

%% ---------------------------------------------------------------------
%% Better Version (after looking at community answers)

% -spec slices(pos_integer(), string()) -> [string()].
% slices(N, String) when length(String) >= N, N > 0 ->
%     [string:slice(String, I, N) || I <- lists:seq(0, length(String) - N)].

%% ---------------------------------------------------------------------
%% Tail-Recursive Version

-spec slices(pos_integer(), string()) -> [string()].
slices(N, String) when length(String) >= N ->
    slices_impl(N, String, []).

-spec slices_impl(pos_integer(), string(), [string()]) -> [string()].
slices_impl(N, String, Acc) when length(String) < N ->
    lists:reverse(Acc);
slices_impl(N, [_ | T] = String, Acc) ->
    slices_impl(N, T, [string:slice(String, 0, N) | Acc]).

%% ---------------------------------------------------------------------
%% Body-Recursive Version

% slices(N, String) when length(String) >= N ->
%     slices_impl(N, String).

% slices_impl(N, String) when length(String) < N ->
%     [];
% slices_impl(N, [_|T] = String) ->
%     [string:slice(String, 0, N) | slices_impl(N, T)].
