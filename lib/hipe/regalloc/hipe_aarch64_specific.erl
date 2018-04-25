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

-module(hipe_aarch64_specific).

-export([analyze/2,
    liveout/3,
    allocatable/1,
    all_precoloured/1,
    is_precoloured/2,
    labels/2,
    number_of_temporaries/2,
    check_and_rewrite/3,
    reverse_postorder/2,
    livein/3,
    non_alloc/2,
    physical_name/2,
    bb/3,
    def_use/2,
    uses/2,
    defines/2,
    defines_all_alloc/2,
    is_move/2,
    is_spill_move/2,
    reg_nr/2
	]).

%% for hipe_ls_regalloc:
-export([args/2, is_arg/2, is_global/2, new_spill_index/2]).
-export([breadthorder/2, postorder/2]).

%% callbacks for hipe_regalloc_prepass, hipe_range_split
-export([mk_move/3,
	mk_goto/2,
    redirect_jmp/4,
    new_label/1,
    new_reg_nr/1,
    update_reg_nr/3,
    update_bb/4,
    subst_temps/3
    ]).

%% callbacks for hipe_bb_weights, hipe_range_split
-export([branch_preds/2]).

check_and_rewrite(CFG, Coloring, no_context) ->
  hipe_aarch64_ra_postconditions:check_and_rewrite(CFG, Coloring, 'normal').

reverse_postorder(CFG, _) ->
  hipe_aarch64_cfg:reverse_postorder(CFG).

non_alloc(CFG, no_context) ->
  non_alloc_1(hipe_aarch64_registers:nr_args(), hipe_aarch64_cfg:params(CFG)).

%% same as hipe_aarch64_frame:fix_formals/2
non_alloc_1(0, Rest) -> Rest;
non_alloc_1(N, [_|Rest]) -> non_alloc_1(N-1, Rest);
non_alloc_1(_, []) -> [].

%% Liveness stuff

analyze(CFG, _) ->
  hipe_aarch64_liveness_gpr:analyse(CFG).

livein(Liveness,L,_) ->
  [X || X <- hipe_aarch64_liveness_gpr:livein(Liveness,L),
	hipe_aarch64:temp_is_allocatable(X)].

liveout(BB_in_out_liveness,Label,_) ->
  [X || X <- hipe_aarch64_liveness_gpr:liveout(BB_in_out_liveness,Label),
	hipe_aarch64:temp_is_allocatable(X)].

%% Registers stuff

allocatable(no_context) ->
  hipe_aarch64_registers:allocatable_gpr().

all_precoloured(no_context) ->
  hipe_aarch64_registers:all_precoloured().

is_precoloured(Reg, _) ->
  hipe_aarch64_registers:is_precoloured_gpr(Reg).

physical_name(Reg, _) ->
  Reg.

%% CFG stuff

labels(CFG, _) ->
  hipe_aarch64_cfg:labels(CFG).

number_of_temporaries(_CFG, _) ->
  Highest_temporary = hipe_gensym:get_var(aarch64),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG,L,_) ->
  hipe_aarch64_cfg:bb(CFG,L).

update_bb(CFG,L,BB,_) ->
  hipe_aarch64_cfg:bb_add(CFG,L,BB).

branch_preds(Branch,_) ->
  hipe_aarch64_cfg:branch_preds(Branch).

%% AARCH64 stuff

def_use(Instruction, Ctx) ->
  {defines(Instruction, Ctx), uses(Instruction, Ctx)}.

uses(I, _) ->
  [X || X <- hipe_aarch64_defuse:insn_use_gpr(I),
	hipe_aarch64:temp_is_allocatable(X)].

defines(I, _) ->
  [X || X <- hipe_aarch64_defuse:insn_def_gpr(I),
	hipe_aarch64:temp_is_allocatable(X)].

defines_all_alloc(I, _) ->
  hipe_aarch64_defuse:insn_defs_all_gpr(I).

is_move(Instruction, _) ->
  case hipe_aarch64:is_pseudo_move(Instruction) of
    true ->
      Dst = hipe_aarch64:pseudo_move_dst(Instruction),
      case hipe_aarch64:temp_is_allocatable(Dst) of
	false -> false;
	_ ->
	  Src = hipe_aarch64:pseudo_move_src(Instruction),
	  hipe_aarch64:temp_is_allocatable(Src)
      end;
    false -> false
  end.

is_spill_move(Instruction, _) ->
  hipe_aarch64:is_pseudo_spill_move(Instruction).

reg_nr(Reg, _) ->
  hipe_aarch64:temp_reg(Reg).

mk_move(Src, Dst, _) ->
  hipe_aarch64:mk_pseudo_move(Dst, Src).

mk_goto(Label, _) ->
  hipe_aarch64:mk_b_label(Label).

redirect_jmp(Jmp, ToOld, ToNew, _) when is_integer(ToOld), is_integer(ToNew) ->
  Ref = make_ref(),
  put(Ref, false),
  I = hipe_aarch64_subst:insn_lbls(
	fun(Tgt) ->
	    if Tgt =:= ToOld -> put(Ref, true), ToNew;
	       is_integer(Tgt) -> Tgt
	    end
	end, Jmp),
  true = erase(Ref), % Assert that something was rewritten
  I.

new_label(_) ->
  hipe_gensym:get_next_label(aarch64).

new_reg_nr(_) ->
  hipe_gensym:get_next_var(aarch64).

update_reg_nr(Nr, Temp, _) ->
  hipe_aarch64:mk_temp(Nr, hipe_aarch64:temp_type(Temp)).

subst_temps(SubstFun, Instr, _) ->
  hipe_aarch64_subst:insn_temps(
    fun(Op) ->
	case hipe_aarch64:temp_is_allocatable(Op) of
	  true -> SubstFun(Op);
	  false -> Op
	end
    end, Instr).

%%% Linear Scan stuff

new_spill_index(SpillIndex, _) when is_integer(SpillIndex) ->
  SpillIndex+1.

breadthorder(CFG, _) ->
  hipe_aarch64_cfg:breadthorder(CFG).

postorder(CFG, _) ->
  hipe_aarch64_cfg:postorder(CFG).

is_global(R, _) ->
  R =:= hipe_aarch64_registers:temp1() orelse
  R =:= hipe_aarch64_registers:temp2() orelse
  R =:= hipe_aarch64_registers:temp3() orelse
  hipe_aarch64_registers:is_fixed(R).

is_arg(R, _) ->
  hipe_aarch64_registers:is_arg(R).

args(CFG, _) ->
  hipe_aarch64_registers:args(hipe_aarch64_cfg:arity(CFG)).