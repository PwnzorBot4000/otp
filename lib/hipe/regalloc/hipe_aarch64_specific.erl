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
    labels/2,
    number_of_temporaries/2,
    reverse_postorder/2,
    non_alloc/2,
    bb/3,
    def_use/2,
    defines_all_alloc/2,
    is_move/2,
    reg_nr/2
	]).

%% callbacks for hipe_regalloc_prepass, hipe_range_split
-export([update_bb/4,
    subst_temps/3
    ]).

reverse_postorder(CFG, _) ->
  hipe_aarch64_cfg:reverse_postorder(CFG).

non_alloc(CFG, no_context) ->
  non_alloc_1(hipe_aarch64_registers:nr_args(), hipe_aarch64_cfg:params(CFG)).

%% same as hipe_arm_frame:fix_formals/2
non_alloc_1(0, Rest) -> Rest;
non_alloc_1(N, [_|Rest]) -> non_alloc_1(N-1, Rest);
non_alloc_1(_, []) -> [].

%% Liveness stuff

analyze(CFG, _) ->
  hipe_aarch64_liveness_gpr:analyse(CFG).

liveout(BB_in_out_liveness,Label,_) ->
  [X || X <- hipe_aarch64_liveness_gpr:liveout(BB_in_out_liveness,Label),
	hipe_aarch64:temp_is_allocatable(X)].

%% Registers stuff

allocatable(no_context) ->
  hipe_aarch64_registers:allocatable_gpr().

all_precoloured(no_context) ->
  hipe_aarch64_registers:all_precoloured().

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

reg_nr(Reg, _) ->
  hipe_aarch64:temp_reg(Reg).

subst_temps(SubstFun, Instr, _) ->
  hipe_aarch64_subst:insn_temps(
    fun(Op) ->
	case hipe_aarch64:temp_is_allocatable(Op) of
	  true -> SubstFun(Op);
	  false -> Op
	end
    end, Instr).
