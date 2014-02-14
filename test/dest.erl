-module(dest).
-export([hbfunmap/0, add/2, hbfun1/2]).

add(Var1, Var2) -> Var1 + Var2.

hbfun1(Var1, Var2) -> Var1 * Var2.

qyfunmap() -> [{{<<"mult">>,2},hbfun1},{{<<"add">>,2},add}].
