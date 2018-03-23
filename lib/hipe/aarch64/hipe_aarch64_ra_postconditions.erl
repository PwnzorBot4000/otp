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

-module(hipe_aarch64_ra_postconditions).

-export([check_and_rewrite/3, check_and_rewrite2/3]).

-include("hipe_aarch64.hrl").

check_and_rewrite(CFG, Coloring, Allocator) ->
  TempMap = hipe_temp_map:cols2tuple(Coloring, hipe_aarch64_specific, no_context),
  check_and_rewrite2(CFG, TempMap, Allocator).

check_and_rewrite2(CFG, TempMap, Allocator) ->
  Strategy = strategy(Allocator),
  do_bbs(hipe_aarch64_cfg:labels(CFG), TempMap, Strategy, CFG, false).

strategy(Allocator) ->
  case Allocator of
    'normal' -> 'new';
    'linearscan' -> 'fixed';
    'naive' -> 'fixed'
  end.

do_bbs([], _, _, CFG, DidSpill) -> {CFG, DidSpill};
do_bbs([Lbl|Lbls], TempMap, Strategy, CFG0, DidSpill0) ->
  Code0 = hipe_bb:code(BB = hipe_aarch64_cfg:bb(CFG0, Lbl)),
  {Code, DidSpill} = do_insns(Code0, TempMap, Strategy, [], DidSpill0),
  CFG = hipe_aarch64_cfg:bb_add(CFG0, Lbl, hipe_bb:code_update(BB, Code)),
  do_bbs(Lbls, TempMap, Strategy, CFG, DidSpill).

do_insns([I|Insns], TempMap, Strategy, Accum, DidSpill0) ->
  {NewIs, DidSpill1} = do_insn(I, TempMap, Strategy),
  do_insns(Insns, TempMap, Strategy, lists:reverse(NewIs, Accum), DidSpill0 or DidSpill1);
do_insns([], _TempMap, _Strategy, Accum, DidSpill) ->
  {lists:reverse(Accum), DidSpill}.

do_insn(I, TempMap, Strategy) ->
  case I of
    #move{} -> do_move(I, TempMap, Strategy);
    #pseudo_call{} -> do_pseudo_call(I, TempMap, Strategy);
    #pseudo_li{} -> do_pseudo_li(I, TempMap, Strategy);
    #pseudo_move{} -> do_pseudo_move(I, TempMap, Strategy);
    #pseudo_tailcall{} -> do_pseudo_tailcall(I, TempMap, Strategy);
    #pseudo_blr{} -> {[I], false};
    #pseudo_tailcall_prepare{} -> {[I], false}
    %_ -> {[I], false} %temp adding all default cases explicitly
  end.

%%% Fix relevant instruction types.

do_move(I=#move{dst=Dst,am1=Am1}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill1} = fix_dst(Dst, TempMap, Strategy),
  {FixAm1,NewAm1,DidSpill2} = fix_am1(Am1, TempMap, Strategy),
  NewI = I#move{dst=NewDst,am1=NewAm1},
  {FixAm1 ++ [NewI | FixDst], DidSpill1 or DidSpill2}.

do_pseudo_call(I=#pseudo_call{funv=FunV}, TempMap, Strategy) ->
  {FixFunV,NewFunV,DidSpill} = fix_funv(FunV, TempMap, Strategy),
  NewI = I#pseudo_call{funv=NewFunV},
  {FixFunV ++ [NewI], DidSpill}.

do_pseudo_li(I=#pseudo_li{dst=Dst}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill} = fix_dst(Dst, TempMap, Strategy),
  NewI = I#pseudo_li{dst=NewDst},
  {[NewI | FixDst], DidSpill}.

do_pseudo_move(I=#pseudo_move{dst=Dst,src=Src}, TempMap, Strategy) ->
  %% Either Dst or Src (but not both) may be a pseudo temp.
  %% pseudo_move, pseudo_spill_move, and pseudo_tailcall
  %% are special cases: in all other instructions, all
  %% temps must be non-pseudos after register allocation.
  case temp_is_spilled(Dst, TempMap)
    andalso temp_is_spilled(Dst, TempMap)
  of
    true -> % Turn into pseudo_spill_move
      Temp = clone(Src, temp1(Strategy)),
      NewI = #pseudo_spill_move{dst=Dst, temp=Temp, src=Src},
      {[NewI], true};
    _ ->
      {[I], false}
  end.

do_pseudo_tailcall(I=#pseudo_tailcall{funv=FunV}, TempMap, Strategy) ->
  {FixFunV,NewFunV,DidSpill} = fix_funv(FunV, TempMap, Strategy),
  NewI = I#pseudo_tailcall{funv=NewFunV},
  {FixFunV ++ [NewI], DidSpill}.

%%% Fix Dst and Src operands.

fix_funv(FunV, TempMap, Strategy) ->
  case FunV of
    #aarch64_temp{} -> fix_src3(FunV, TempMap, Strategy);
    _ -> {[], FunV, false}
  end.

fix_am1(Am1, _, _) ->
  case Am1 of
    {_Size,_Imm,_Shift} -> {[], Am1, false}
  end.

temp1('new') -> [];
temp1('fixed') -> hipe_aarch64_registers:temp1().

fix_src3(Src, TempMap, Strategy) ->
  fix_src(Src, TempMap, temp3(Strategy)).

temp3('new') -> [];
temp3('fixed') -> hipe_aarch64_registers:temp3().

fix_src(Src, TempMap, RegOpt) ->
  case temp_is_spilled(Src, TempMap) of
    true ->
      NewSrc = clone(Src, RegOpt),
      {[hipe_aarch64:mk_pseudo_move(NewSrc, Src)],
       NewSrc,
       true};
    _ ->
      {[], Src, false}
  end.

fix_dst(Dst, TempMap, Strategy) ->
  fix_dst_common(Dst, TempMap, temp1(Strategy)).

fix_dst_common(Dst, TempMap, RegOpt) ->
  case temp_is_spilled(Dst, TempMap) of
    true ->
      NewDst = clone(Dst, RegOpt),
      {[hipe_aarch64:mk_pseudo_move(Dst, NewDst)], NewDst, true};
    _ ->
      {[], Dst, false}
  end.

%%% Check if an operand is a pseudo-temp.

temp_is_spilled(Temp, []) -> % special case for naive regalloc
  not(hipe_aarch64:temp_is_precoloured(Temp));
temp_is_spilled(Temp, TempMap) ->
  case hipe_aarch64:temp_is_allocatable(Temp) of
    true ->
      Reg = hipe_aarch64:temp_reg(Temp),
      tuple_size(TempMap) > Reg andalso hipe_temp_map:is_spilled(Reg, TempMap);
    false -> true
  end.

%%% Make a certain reg into a clone of Temp.

clone(Temp, RegOpt) ->
  Type = hipe_aarch64:temp_type(Temp),
  case RegOpt of
    [] -> hipe_aarch64:mk_new_temp(Type);
    Reg -> hipe_aarch64:mk_temp(Reg, Type)
  end.
