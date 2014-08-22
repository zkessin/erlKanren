-module(kanren_tests).
-include_lib("eunit/include/eunit.hrl").

disj_test() ->
    [2, 3]    =  (kanren:disj(fun kanren:foo/1, fun kanren:bar/1)) (1),
    [2, 3, 4] =  (kanren:disj([fun kanren:foo/1, fun kanren:bar/1, fun kanren:baz/1])) (1).

conj_test() ->
    [4] =  (kanren:conj(fun kanren:foo/1, fun kanren:bar/1)) (1),
    [7] =  (kanren:conj([fun kanren:foo/1, fun kanren:bar/1, fun kanren:baz/1])) (1).

disj_conj_test() ->
    [1, 2, 2, 3, 3] =
         (kanren:disj(kanren:disj(fun kanren:fail/1, fun kanren:succeed/1),
                      kanren:conj(kanren:disj(fun kanren:foo/1, fun kanren:bar/1),
                                  kanren:disj(fun kanren:succeed/1, fun kanren:succeed/1)))) (1).

is_lvar_test() ->
    true = kanren:is_lvar(l_hai),
    false = kanren:is_lvar(hai),
    false = kanren:is_lvar("l_hai").

assq_test() ->
    bar = kanren:assq(foo, [[bar|1], [foo|bar]]).

lookup_test() ->
    1 = kanren:lookup(l_x, [[l_y|1], [l_x|l_y]]),
    l_x = kanren:lookup(l_y, [[l_y|l_x]]).

lookup_deep_test() ->
    [1, 2] = kanren:lookup_deep(l_q, [[l_l3p, 2], [l_q, l_h|l_l3p], [l_t], [l_h|1]]).

unify_test() ->
    [[l_x|l_y]] = kanren:unify(l_x, l_y, []),
    [[l_y|1], [l_x|l_y]] = kanren:unify(l_x, 1, kanren:unify(l_x, l_y, [])),
    1 = kanren:lookup(l_y, kanren:unify(l_x, 1, kanren:unify(l_x, l_y, []))),
    [[l_y|1], [l_x|l_y]] = kanren:unify([l_x|l_y], [l_y|1], []).

membero_test() ->
    [[]] = kanren:run(kanren:membero(2, [1, 2, 3])),
    [] = kanren:run(kanren:membero(10, [1, 2, 3])),
    [[[l_q|1]], [[l_q|2]], [[l_q|3]]] = kanren:run(kanren:membero(l_q, [1, 2, 3])),
    [[[l_q|2]], [[l_q|3]]] = kanren:run(kanren:conj(kanren:membero(l_q, [1, 2, 3]),
                                                    kanren:membero(l_q, [2, 3, 4]))).

conso_test() ->
    [[[l_x, 1, 2, 3]]] = kanren:run(kanren:conso(1, [2, 3], l_x)),
    [[[l_y, 2, 3], [l_x|1]]] = kanren:run(kanren:conso(l_x, l_y, [1, 2, 3])).


appendo_test() ->
    [[1, 2]] = kanren:run(l_q, kanren:appendo([1], [2], l_q)),
    [] = kanren:run(l_q, kanren:appendo([1], [2], [1])),
    [[2, 3]] = kanren:run(l_q, kanren:appendo([1], l_q, [1, 2, 3])).
