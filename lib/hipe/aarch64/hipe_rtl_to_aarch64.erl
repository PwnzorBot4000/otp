%% -*- erlang-indent-level: 2 -*-
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(hipe_rtl_to_aarch64).
-export([translate/1]).

-include("../rtl/hipe_rtl.hrl").

translate(RTL) ->
  hipe_gensym:init(aarch64),
  hipe_gensym:set_var(aarch64, hipe_aarch64_registers:first_virtual()),
  hipe_gensym:set_label(aarch64, hipe_gensym:get_label(rtl)),
  Map0 = vmap_empty(),
  Formals = [],
  OldData = hipe_rtl:rtl_data(RTL),
  {Code, NewData} = conv_insn_list(hipe_rtl:rtl_code(RTL), Map0, OldData),
  IsClosure = hipe_rtl:rtl_is_closure(RTL),
  IsLeaf = hipe_rtl:rtl_is_leaf(RTL),
  hipe_aarch64:mk_defun(hipe_rtl:rtl_fun(RTL),
		    Formals,
		    IsClosure,
		    IsLeaf,
		    Code,
		    NewData,
		    [], 
		    []).

conv_insn_list([H|T], Map, Data) ->
  {NewH, NewMap, NewData1} = conv_insn(H, Map, Data),
  %% io:format("~w \n  ==>\n ~w\n- - - - - - - - -\n",[H,NewH]),
  {NewT, NewData2} = conv_insn_list(T, NewMap, NewData1),
  {NewH ++ NewT, NewData2};
conv_insn_list([], _, Data) ->
  {[], Data}.

conv_insn(I, Map, Data) ->
  case I of
    #call{} -> conv_call(I, Map, Data);
    #enter{} -> conv_enter(I, Map, Data);
    #label{} -> conv_label(I, Map, Data);
    #load_atom{} -> conv_load_atom(I, Map, Data);
    #return{} -> conv_return(I, Map, Data);
    _ -> exit({?MODULE,conv_insn,I})
  end.

conv_call(I, Map, Data) ->
  {Args, Map0} = conv_src_list(hipe_rtl:call_arglist(I), Map),
  {Dsts, Map1} = conv_dst_list(hipe_rtl:call_dstlist(I), Map0),
  {Fun, Map2} = conv_fun(hipe_rtl:call_fun(I), Map1),
  ContLab = hipe_rtl:call_continuation(I),
  ExnLab = hipe_rtl:call_fail(I),
  Linkage = hipe_rtl:call_type(I),
  I2 = mk_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage),
  {I2, Map2, Data}.

mk_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage) ->
  case hipe_aarch64:is_prim(Fun) of
    true ->
      mk_primop_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage);
    false ->
      mk_general_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage)
  end.

mk_primop_call(Dsts, Prim, Args, ContLab, ExnLab, Linkage) ->
  case hipe_aarch64:prim_prim(Prim) of
    %% no AARCH64-specific primops defined yet
    _ ->
      mk_general_call(Dsts, Prim, Args, ContLab, ExnLab, Linkage)
  end.

mk_general_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage) ->
  %% The backend does not support pseudo_calls without a
  %% continuation label, so we make sure each call has one.
  {RealContLab, Tail} =
    case mk_call_results(Dsts) of
      [] ->
	%% Avoid consing up a dummy basic block if the moves list
	%% is empty, as is typical for calls to suspend/0.
	%% This should be subsumed by a general "optimise the CFG"
	%% module, and could probably be removed.
	case ContLab of
	  [] ->
	    NewContLab = hipe_gensym:get_next_label(aarch64),
	    {NewContLab, [hipe_aarch64:mk_label(NewContLab)]};
	  _ ->
	    {ContLab, []}
	end;
      Moves ->
	%% Change the call to continue at a new basic block.
	%% In this block move the result registers to the Dsts,
	%% then continue at the call's original continuation.
	NewContLab = hipe_gensym:get_next_label(aarch64),
	case ContLab of
	  [] ->
	    %% This is just a fallthrough
	    %% No jump back after the moves.
	    {NewContLab,
	     [hipe_aarch64:mk_label(NewContLab) |
	      Moves]};
	  _ ->
	    %% The call has a continuation. Jump to it.
	    {NewContLab,
	     [hipe_aarch64:mk_label(NewContLab) |
	      Moves ++
	      [hipe_aarch64:mk_b_label(ContLab)]]}
	end
    end,
  SDesc = hipe_aarch64:mk_sdesc(ExnLab, 0, length(Args), {}),
  CallInsn = hipe_aarch64:mk_pseudo_call(Fun, SDesc, RealContLab, Linkage),
  {RegArgs,StkArgs} = split_args(Args),
  mk_push_args(StkArgs, move_actuals(RegArgs, [CallInsn | Tail])).

mk_call_results([]) ->
  [];
mk_call_results([Dst]) ->
  RV = hipe_aarch64:mk_temp(hipe_aarch64_registers:return_value(), 'tagged'),
  [hipe_aarch64:mk_pseudo_move(Dst, RV)];
mk_call_results(Dsts) ->
  exit({?MODULE,mk_call_results,Dsts}).

mk_push_args(StkArgs, Tail) ->
  case length(StkArgs) of
    0 ->
      Tail;
    NrStkArgs ->
      [hipe_aarch64:mk_pseudo_call_prepare(NrStkArgs) |
       mk_store_args(StkArgs, NrStkArgs * word_size(), Tail)]
  end.
  
mk_store_args([Arg|Args], PrevOffset, Tail) ->
  Offset = PrevOffset - word_size(),
  {Src,FixSrc} =
    case hipe_aarch64:is_temp(Arg) of
      true ->
	{Arg, []};
      _ ->
	Tmp = new_tagged_temp(),
	{Tmp, mk_li(Tmp, Arg)}
    end,
  NewTail = hipe_aarch64:mk_store('str', Src, mk_sp(), Offset, 'new', Tail),
  mk_store_args(Args, Offset, FixSrc ++ NewTail);
mk_store_args([], _, Tail) ->
  Tail.

conv_enter(I, Map, Data) ->
  {Args, Map0} = conv_src_list(hipe_rtl:enter_arglist(I), Map),
  {Fun, Map1} = conv_fun(hipe_rtl:enter_fun(I), Map0),
  I2 = mk_enter(Fun, Args, hipe_rtl:enter_type(I)),
  {I2, Map1, Data}.

mk_enter(Fun, Args, Linkage) ->
  Arity = length(Args),
  [hipe_aarch64:mk_pseudo_tailcall_prepare(),
	hipe_aarch64:mk_pseudo_tailcall(Fun, Arity, [], Linkage)].

conv_label(I, Map, Data) ->
  I2 = [hipe_aarch64:mk_label(hipe_rtl:label_name(I))],
  {I2, Map, Data}.

conv_load_atom(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_atom_dst(I), Map),
  Src = hipe_rtl:load_atom_atom(I),
  I2 = [hipe_aarch64:mk_pseudo_li(Dst, Src)],
  {I2, Map0, Data}.

mk_move(Dst, Src, Tail) ->
  case hipe_aarch64:is_temp(Src) of
    true -> [hipe_aarch64:mk_pseudo_move(Dst, Src) | Tail];
    _ -> mk_li(Dst, Src, Tail)
  end.

conv_return(I, Map, Data) ->
%% TODO: multiple-value returns
{[Arg], Map0} = conv_src_list(hipe_rtl:return_varlist(I), Map),
I2 = mk_move(mk_rv(), Arg,
         [hipe_aarch64:mk_pseudo_blr()]),
{I2, Map0, Data}.

%%% Load an integer constant into a register.

mk_li(Dst, Value) -> mk_li(Dst, Value, []).

mk_li(Dst, Value, Tail) ->
  hipe_aarch64:mk_li(Dst, Value, Tail).

%%% Split a list of formal or actual parameters into the
%%% part passed in registers and the part passed on the stack.
%%% The parameters passed in registers are also tagged with
%%% the corresponding registers.

split_args(Args) ->
  split_args(0, hipe_aarch64_registers:nr_args(), Args, []).

split_args(I, N, [Arg|Args], RegArgs) when I < N ->
  Reg = hipe_aarch64_registers:arg(I),
  Temp = hipe_aarch64:mk_temp(Reg, 'tagged'),
  split_args(I+1, N, Args, [{Arg,Temp}|RegArgs]);
split_args(_, _, StkArgs, RegArgs) ->
  {RegArgs, StkArgs}.

%%% Convert a list of actual parameters passed in
%%% registers (from split_args/1) to a list of moves.

move_actuals([{Src,Dst}|Actuals], Rest) ->
  move_actuals(Actuals, mk_move(Dst, Src, Rest));
move_actuals([], Rest) ->
  Rest.

%%% Convert a 'fun' operand (MFA, prim, or temp)

conv_fun(Fun, Map) ->
  case hipe_rtl:is_var(Fun) of
    false ->
      case hipe_rtl:is_reg(Fun) of
    false ->
      if is_atom(Fun) ->
          exit("atom");
         true ->
          {conv_mfa(Fun), Map}
      end
      end
  end.

%%% Convert an MFA operand.

conv_mfa({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
  hipe_aarch64:mk_mfa(M, F, A).

%%% Convert an RTL source operand (imm/var/reg).
%%% Returns a temp or a naked integer.

conv_src(Opnd, Map) ->
  case hipe_rtl:is_imm(Opnd) of
    true ->
      Value = hipe_rtl:imm_value(Opnd),
      if is_integer(Value) ->
	  {Value, Map}
      end;
    false ->
      conv_dst(Opnd, Map)
  end.

conv_src_list([O|Os], Map) ->
  {V, Map1} = conv_src(O, Map),
  {Vs, Map2} = conv_src_list(Os, Map1),
  {[V|Vs], Map2};
conv_src_list([], Map) ->
  {[], Map}.

%%% Convert an RTL destination operand (var/reg).

conv_dst(Opnd, Map) ->
  {Name, Type} =
    case hipe_rtl:is_var(Opnd) of
      true ->
	{hipe_rtl:var_index(Opnd), 'tagged'};
      false ->
	case hipe_rtl:is_fpreg(Opnd) of
	  true ->
	    {hipe_rtl:fpreg_index(Opnd), 'double'};
	  false ->
	    {hipe_rtl:reg_index(Opnd), 'untagged'}
	end
    end,
  IsPrecoloured =
    case Type of
      'double' -> false; %hipe_arm_registers:is_precoloured_fpr(Name);
      _ -> hipe_aarch64_registers:is_precoloured_gpr(Name)
    end,
  case IsPrecoloured of
    false ->
      case vmap_lookup(Map, Opnd) of
	{value, NewTemp} ->
	  {NewTemp, Map};
	_ ->
	  NewTemp = hipe_aarch64:mk_new_temp(Type),
	  {NewTemp, vmap_bind(Map, Opnd, NewTemp)}
    end
  end.

conv_dst_list([O|Os], Map) ->
  {Dst, Map1} = conv_dst(O, Map),
  {Dsts, Map2} = conv_dst_list(Os, Map1),
  {[Dst|Dsts], Map2};
conv_dst_list([], Map) ->
  {[], Map}.

%%% Create a temp representing the stack pointer register.

mk_sp() ->
  hipe_aarch64:mk_temp(hipe_aarch64_registers:stack_pointer(), 'untagged').

%%% Create a temp representing the return value register.

mk_rv() ->
  hipe_aarch64:mk_temp(hipe_aarch64_registers:return_value(), 'tagged').

%%% new_tagged_temp -- conjure up a tagged scratch reg

new_tagged_temp() ->
  hipe_aarch64:mk_new_temp('tagged').

%%% Map from RTL var/reg operands to temps.

vmap_empty() ->
  gb_trees:empty().

vmap_lookup(Map, Key) ->
  gb_trees:lookup(Key, Map).

vmap_bind(Map, Key, Val) ->
  gb_trees:insert(Key, Val, Map).

word_size() ->
  8.
