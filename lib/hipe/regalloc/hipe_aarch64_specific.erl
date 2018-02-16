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
    labels/2,
    number_of_temporaries/2,
    reverse_postorder/2,
    bb/3,
    reg_nr/2,
    def_use/2
	]).

reverse_postorder(CFG, _) ->
  hipe_aarch64_cfg:reverse_postorder(CFG).

%% Liveness stuff

analyze(CFG, _) ->
  hipe_aarch64_liveness_gpr:analyse(CFG).

%% CFG stuff

labels(CFG, _) ->
  hipe_aarch64_cfg:labels(CFG).

number_of_temporaries(_CFG, _) ->
  Highest_temporary = hipe_gensym:get_var(aarch64),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG,L,_) ->
  hipe_aarch64_cfg:bb(CFG,L).

%% AARCH64 stuff

def_use(Instruction, Ctx) ->
  {defines(Instruction, Ctx), uses(Instruction, Ctx)}.

uses(I, _) ->
  [X || X <- hipe_aarch64_defuse:insn_use_gpr(I),
	hipe_aarch64:temp_is_allocatable(X)].

defines(I, _) ->
  [X || X <- hipe_aarch64_defuse:insn_def_gpr(I),
	hipe_aarch64:temp_is_allocatable(X)].

reg_nr(Reg, _) ->
  hipe_aarch64:temp_reg(Reg).
