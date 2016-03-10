-module(triangle).
-export([start/1, usage/0, t/1, s/1]).

s(N) -> s(N, 0, 0).

s(0, _, J) -> J;
s(N, I, J) when J =< I -> s(N - 1, I, J + 1);
s(N, I, _) -> s(N - 1, I + 1, 1).

t(N) -> t(N, 0, 0, 0).

t(0, _, _, Acc) -> Acc;
t(N, I, J, Acc) when J =< I -> t(N - 1, I, J + 1, J + 1 + Acc);
t(N, I, _, Acc) -> t(N - 1, I + 1, 1, 1 + Acc).

usage() ->
  io:format("erl -noshell -s triangle.erl <n>").

start([Arg|_]) ->
  case string:to_integer(atom_to_list(Arg)) of
    {error, Reason} -> io:format(Reason);
    {N, _} -> io:format("~b~n", [t(N)])
  end,
  init:stop();
start([]) -> usage().
