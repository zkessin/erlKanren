-module(kanren).
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-compile(export_all).
-endif.

-export([fail/1, succeed/1, disj/1, disj/2, conj/1, conj/2,
         is_lvar/1, ext_s/3, lookup/2, unify/3,
         eq/2, membero/2, conso/3, appendo/3,
         run/1, run/2]).


-spec disj(_,_) -> fun((_) -> any()).
-spec disj([any()]) -> fun((_) -> any()).
-spec conj(_,_) -> fun((_) -> [any()]).
-spec conj(maybe_improper_list()) -> any().
-spec string_is_lvar(_) -> boolean().
-spec is_lvar(_) -> boolean().
-spec ext_s(_,_,_) -> nonempty_maybe_improper_list().
-spec assq(_,maybe_improper_list()) -> any().
-spec lookup(_,_) -> any().
-spec lookup_deep(_,_,_) -> any().
-spec lookup_deep(_,_) -> any().
-spec unify(_,_,_,_,_) -> any().
-spec unify(_,_,_) -> any().
-spec run(fun((_) -> any())) -> any().
-spec run(_,fun((_) -> any())) -> [any()].
-spec eq(_,_) -> fun((_) -> [any()]).
-spec membero(_,[any()]) -> fun((_) -> any()).
-spec conso(_,_,_) -> fun((_) -> [any()]).
-spec appendo(_,_,_) -> fun((_) -> any()).


%% Non-deterministic functions
fail(_) ->
    [].

succeed(X) ->
    [X].


disj(F1, F2) ->
    fun(X) -> F1(X) ++ F2(X) end.

disj([]) -> fun fail/1;
disj([F1, F2]) -> disj(F1, F2);
disj([H|T]) -> disj(H, disj(T)).

conj(F1, F2) ->
    fun(X) -> lists:append([F2(Y) || Y <- F1(X)]) end.

conj([]) -> fun succeed/1;
conj([F1]) -> F1;
conj([H|T]) -> conj(H, fun(S) -> (conj(T))(S) end).

foo(X) ->
    succeed(X + 1).
bar(X) ->
    succeed(X + 2).
baz(X) ->
    succeed(X + 3).

%% Logic variables

string_is_lvar("l_" ++ _) -> true;
string_is_lvar(_) -> false.

is_lvar(X) ->
    is_atom(X) andalso string_is_lvar(atom_to_list(X)).

ext_s(Var, Value, S) ->
    [[Var|Value]|S].

assq(_, []) -> false;
assq(Var, [[Key|Value]|_]) when Key =:= Var -> Value;
assq(Var, [_|T]) -> assq(Var, T).

lookup(Var, S) ->
    case is_lvar(Var) of
        false -> Var;
        true ->
            Value = assq(Var, S),
            case Value of
                false -> Var;
                _ -> lookup(Value, S)
            end
    end.

lookup_deep(Var, true, _) -> Var;
lookup_deep([H|T], _, S) -> [lookup_deep(H, S) | lookup_deep(T, S)];
lookup_deep(Var, _, _) -> Var.

lookup_deep(Var, S) ->
    V = lookup(Var, S),
    lookup_deep(V, is_lvar(V), S).

unify(Term, _, Term, _, S) -> S;
unify(T1, true, T2, _, S) -> ext_s(T1, T2, S);
unify(T1, _, T2, true, S) -> ext_s(T2, T1, S);
unify([H1|T1], _, [H2|T2], _, S) ->
    case unify(H1, H2, S) of
        false -> false;
        S2 -> unify(T1, T2, S2)
    end;
unify(_, _, _, _, _) -> false.

unify(Term1, Term2, S) ->
    T1 = lookup(Term1, S),
    LT1 = is_lvar(T1),
    T2 = lookup(Term2, S),
    LT2 = is_lvar(T2),
    unify(T1, LT1, T2, LT2, S).


%% Logic engine

run(G)      when is_function(G) -> G([]).
run(Var, G) when is_function(G) ->
    [lookup_deep(Var, X) || X <- G([])].

eq(T1, T2) ->
    fun(S) ->
            R = unify(T1, T2, S),
            case R of
                false -> fail(S);
                _ -> succeed(R)
            end
    end.

membero(_, []) -> fun fail/1;
membero(Var, [H|T]) ->
    disj(eq(Var, H), membero(Var, T)).

conso(A, B, L) ->
    eq([A|B], L).

appendo(L1, L2, L3) ->
    disj(conj(eq(L1, []), eq(L2, L3)),
         conj([conso(l_h, l_t, L1),
               conso(l_h, l_l3p, L3),
               fun(S) -> (appendo(l_t, L2, l_l3p))(S) end])).

