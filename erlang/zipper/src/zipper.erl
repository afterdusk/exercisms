-module(zipper).

-export([
    new_tree/3,
    from_tree/1,
    to_tree/1,
    up/1,
    left/1,
    right/1,
    value/1,
    set_value/2,
    set_left/2,
    set_right/2
]).

-record(tree, {
    left :: tree_node(),
    right :: tree_node(),
    value :: any()
}).

-type tree_node() :: nil | #tree{}.

-record(zipper, {
    trail :: [{left | right, #tree{}}],
    focus :: #tree{}
}).

-spec new_tree(any(), tree_node(), tree_node()) -> #tree{}.
new_tree(Value, Left, Right) ->
    #tree{left = Left, right = Right, value = Value}.

-spec to_tree(#zipper{}) -> #tree{}.
to_tree(#zipper{trail = [], focus = Focus}) ->
    Focus;
to_tree(Zipper) ->
    to_tree(up(Zipper)).

-spec from_tree(#tree{}) -> #zipper{}.
from_tree(Tree) ->
    #zipper{trail = [], focus = Tree}.

-spec value(#zipper{}) -> any().
value(#zipper{focus = #tree{value = Value}}) ->
    Value.

-spec set_value(#zipper{}, any()) -> #zipper{}.
set_value(#zipper{focus = Focus} = Zipper, Value) ->
    Zipper#zipper{focus = Focus#tree{value = Value}}.

-spec set_left(#zipper{}, tree_node()) -> #zipper{}.
set_left(#zipper{focus = Focus} = Zipper, Value) ->
    Zipper#zipper{focus = Focus#tree{left = Value}}.

-spec set_right(#zipper{}, tree_node()) -> #zipper{}.
set_right(#zipper{focus = Focus} = Zipper, Value) ->
    Zipper#zipper{focus = Focus#tree{right = Value}}.

-spec left(#zipper{}) -> #zipper{} | nil.
%% expected behaviour that's not documented...
left(#zipper{focus = #tree{left = nil}}) ->
    nil;
left(#zipper{trail = Trail, focus = Focus} = Zipper) ->
    io:format("Going left... Trail: ~p, focus: ~p, left: ~p~n", [Trail, Focus, Focus#tree.left]),
    Zipper#zipper{trail = [{left, Focus} | Trail], focus = Focus#tree.left}.

-spec right(#zipper{}) -> #zipper{} | nil.
right(#zipper{focus = #tree{right = nil}}) ->
    nil;
right(#zipper{trail = Trail, focus = Focus} = Zipper) ->
    Zipper#zipper{trail = [{right, Focus} | Trail], focus = Focus#tree.right}.

-spec up(#zipper{}) -> #zipper{}.
up(#zipper{trail = []}) ->
    nil;
up(#zipper{trail = [{left, Prev} | Trail], focus = Focus} = Zipper) ->
    Zipper#zipper{trail = Trail, focus = Prev#tree{left = Focus}};
up(#zipper{trail = [{right, Prev} | Trail], focus = Focus} = Zipper) ->
    Zipper#zipper{trail = Trail, focus = Prev#tree{right = Focus}}.
