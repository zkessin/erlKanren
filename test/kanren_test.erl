-module(kanren_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(kanren,[fail/1, succeed/1, disj/1, disj/2, conj/1, conj/2,
         is_lvar/1, ext_s/3, lookup/2, unify/3,
         eq/2, membero/2, conso/3, appendo/3,assq/2,lookup_deep/2,
         run/1, run/2]).




foo(X) ->
    succeed(X + 1).
bar(X) ->
    succeed(X + 2).
baz(X) ->
    succeed(X + 3).


disj_test() ->
    [2, 3]    = (disj(fun foo/1, fun bar/1))(1),
    [2, 3, 4] = (disj([fun foo/1, fun bar/1, fun baz/1]))(1).

conj_test() ->
    [4] = (conj(fun foo/1, fun bar/1))(1),
    [7] = (conj([fun foo/1, fun bar/1, fun baz/1]))(1).

disj_conj_test() ->
    [1, 2, 2, 3, 3] =
        (disj(disj(fun kanren:fail/1, fun kanren:succeed/1),
              conj(disj(fun foo/1, fun bar/1),
                   disj(fun kanren:succeed/1, fun kanren:succeed/1))))(1).


is_lvar_test() ->
    true = is_lvar(l_hai),
    false = is_lvar(hai),
    false = is_lvar("l_hai").

assq_test() ->
    bar = assq(foo, [[bar|1], [foo|bar]]).

lookup_deep_test() ->
    [1, 2] = lookup_deep(l_q,
                         [[l_l3p, 2],
                          [l_q, l_h|l_l3p], 
                          [l_t], 
                          [l_h|1]]).

unify_test() ->
    [[l_x|l_y]] = unify(l_x, l_y, []),
    [[l_y|1], [l_x|l_y]] = unify(l_x, 1, unify(l_x, l_y, [])),
    1 = lookup(l_y, unify(l_x, 1, unify(l_x, l_y, []))),
    [[l_y|1], [l_x|l_y]] = unify([l_x|l_y], [l_y|1], []).

membero_test() ->
    [[]] = run(membero(2, [1, 2, 3])),
    [] = run(membero(10, [1, 2, 3])),
    [[[l_q|1]], [[l_q|2]], [[l_q|3]]] = run(membero(l_q, [1, 2, 3])),
    [[[l_q|2]], [[l_q|3]]] = run(conj(membero(l_q, [1, 2, 3]),
                                      membero(l_q, [2, 3, 4]))).



conso_test() ->
    [[[l_x|[1, 2, 3]]]] = run(conso(1, [2, 3], l_x)),
    [[[l_y|[2, 3]], [l_x|1]]] = run(conso(l_x, l_y, [1, 2, 3])).

appendo_test() ->
    [[1, 2]] = run(l_q, appendo([1], [2], l_q)),
    [] = run(l_q, appendo([1], [2], [1])),
    [[2, 3]] = run(l_q, appendo([1], l_q, [1, 2, 3])).

    % appendo doesn't consider L1 with more than 1 element - why?

    %% [[4, 5]] = run(l_q, appendo([1, 2, 3], l_q, [1, 2, 3, 4, 5])),
    %% [[1, 2, 3, 4, 5], [2, 3, 4, 5], [3, 4, 5], [4, 5], [5]] =
    %%     run(l_q, appendo(l_x, l_q, [1, 2, 3, 4, 5])).
