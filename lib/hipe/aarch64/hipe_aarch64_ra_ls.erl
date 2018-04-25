%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% Linear Scan register allocator for aarch64

-module(hipe_aarch64_ra_ls).
-export([ra/4]).

ra(CFG, Liveness, SpillIndex, Options) ->
  SpillLimit = hipe_aarch64_specific:number_of_temporaries(CFG, no_context),
  alloc(CFG, Liveness, SpillIndex, SpillLimit, Options).

alloc(CFG, Liveness, SpillIndex, SpillLimit, Options) ->
  {Coloring, _NewSpillIndex} =
    regalloc(
      CFG, Liveness,
      hipe_aarch64_registers:allocatable_gpr()--
      [hipe_aarch64_registers:temp3(),
       hipe_aarch64_registers:temp2(),
       hipe_aarch64_registers:temp1()],
      [hipe_aarch64_cfg:start_label(CFG)],
      SpillIndex, SpillLimit, Options,
      hipe_aarch64_specific, no_context),
  {NewCFG, _DidSpill} =
    hipe_aarch64_ra_postconditions:check_and_rewrite(
      CFG, Coloring, 'linearscan'),
  TempMap = hipe_temp_map:cols2tuple(Coloring, hipe_aarch64_specific, no_context),
  {SpillMap, _NewSpillIndex2} =
    hipe_spillmin:stackalloc(CFG, Liveness, [], SpillIndex, Options,
			     hipe_aarch64_specific, no_context, TempMap),
  Coloring2 =
    hipe_spillmin:mapmerge(hipe_temp_map:to_substlist(TempMap), SpillMap),
  {NewCFG, Liveness, Coloring2}.

regalloc(CFG, Liveness, PhysRegs, Entrypoints, SpillIndex, DontSpill, Options,
	 TgtMod, TgtCtx) ->
  hipe_ls_regalloc:regalloc(CFG, Liveness, PhysRegs, Entrypoints, SpillIndex,
			    DontSpill, Options, TgtMod, TgtCtx).
