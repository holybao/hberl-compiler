%%%-------------------------------------------------------------------
%%% author    libao he (holybao@hotmail.com & blog.holybao.com)
%%%-------------------------------------------------------------------
-module(hberl_allowedfuns).
-export([get_mod_func/3]).
-export([get_include_lib/1]).

get_mod_func("erlang","abs",1) -> true;
get_mod_func("erlang","atom_to_binary",1) -> true;
get_mod_func("erlang","atom_to_list",1) -> true;
get_mod_func("erlang","binary_part",_) -> true;
get_mod_func("erlang","binary_to_existing_atom",1) -> true;
get_mod_func("erlang","binary_to_list",1) -> true;
get_mod_func("erlang","list_to_binary",1) -> true;

get_mod_func("lists",_,_) -> true;

get_mod_func(_Mod,_Fun,_Arity) -> true.

get_include_lib("kernel") -> true;
get_include_lib("ssh") -> true;
get_include_lib(_) -> false.
