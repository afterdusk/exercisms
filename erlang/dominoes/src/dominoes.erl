-module(dominoes).

-export([can_chain/1]).

-type domino() :: {integer(), integer()}.

%% ---------------------------------------------------------------------
%% Scalable Version (?) (fewer linear searches over lists)

-spec can_chain([domino()]) -> boolean().
can_chain([]) ->
    true;
can_chain([{DFront, DBack} = D | Rest] = _Dominoes) ->
    {Fwd, Bwd} = lists:foldl(
        fun({Front, Back}, {FAcc, BAcc}) ->
            FList = maps:get(Front, FAcc, []),
            BList = maps:get(Back, BAcc, []),
            {FAcc#{Front => [Back | FList]}, BAcc#{Back => [Front | BList]}}
        end,
        {#{}, #{}},
        Rest
    ),
    can_chain_impl(Fwd, Bwd, D) orelse can_chain_impl(Fwd, Bwd, {DBack, DFront}).

-spec can_chain_impl(Map, Map, domino()) -> boolean() when Map :: #{integer() => [integer()]}.
can_chain_impl(Fwd, Bwd, {X, X}) when map_size(Fwd) == 0, map_size(Bwd) == 0 ->
    true;
can_chain_impl(Fwd0, Bwd0, {Start, End}) ->
    try
        symmetric_take(Fwd0, Bwd0, End),
        symmetric_take(Bwd0, Fwd0, End)
    catch
        {Fwd, Bwd, Next} ->
            can_chain_impl(Fwd, Bwd, {Start, Next}) orelse can_chain_impl(Fwd, Bwd, {Next, Start})
    end.

-spec symmetric_take(Map, Map, integer()) -> false | no_return().
symmetric_take(Main0, Other0, Key) ->
    case maps:take(Key, Main0) of
        error ->
            false;
        {[Next | Rest], Main1} ->
            Main =
                case Rest of
                    [] -> Main1;
                    _ -> Main1#{Key => Rest}
                end,
            {List, Other1} = maps:take(Next, Other0),
            Other =
                case lists:delete(Key, List) of
                    [] ->
                        Other1;
                    OtherRest ->
                        Other1#{Next => OtherRest}
                end,
            throw({Main, Other, Next})
    end.

%% ---------------------------------------------------------------------
%% Clean Functional Version (from community answers:
%% https://exercism.org/tracks/erlang/exercises/dominoes/solutions/edgerunner)

% -spec can_chain([domino()]) -> boolean().
% can_chain([]) ->
%     true;
% can_chain([{X, X}]) ->
%     true;
% can_chain([{_, _}]) ->
%     false;
% can_chain([err | _]) ->
%     false;
% can_chain([First | Rest]) ->
%     lists:any(
%         fun(D) ->
%             can_chain([merge(First, D) | Rest -- [D]])
%         end,
%         Rest
%     ).

% -spec merge(domino(), domino()) -> domino().
% merge({A, M}, {M, B}) ->
%     {A, B};
% merge({A, M}, {B, M}) ->
%     {A, B};
% merge({M, A}, {M, B}) ->
%     {A, B};
% merge({M, A}, {B, M}) ->
%     {A, B};
% merge(_, _) ->
%     err.
