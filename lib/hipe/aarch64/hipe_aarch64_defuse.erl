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

-module(hipe_aarch64_defuse).
-export([insn_def_all/1, insn_use_all/1]).
-export([insn_def_gpr/1, insn_use_gpr/1]).
-export([insn_defs_all_gpr/1]).
-include("hipe_aarch64.hrl").

%%%
%%% Defs and uses for both general-purpose and floating-point registers.
%%% This is needed for the frame module, alas.
%%%
insn_def_all(I) ->
  insn_def_gpr(I).

insn_use_all(I) ->
  insn_use_gpr(I).

%%%
%%% Defs and uses for general-purpose (integer) registers only.
%%%
insn_def_gpr(I) ->
  case I of
    #move{dst=Dst} -> [Dst];
    #pseudo_li{dst=Dst} -> [Dst];
    #pseudo_tailcall{} -> [];
    #pseudo_tailcall_prepare{} -> tailcall_clobbered_gpr();
    #pseudo_blr{} -> []
    %_ -> [] % temporarily including all default cases explicitly
  end.

insn_defs_all_gpr(I) ->
case I of
  #pseudo_call{} -> true;
  _ -> false
end.

tailcall_clobbered_gpr() ->
  [hipe_aarch64:mk_temp(R, T)
   || {R,T} <- hipe_aarch64_registers:tailcall_clobbered() ++ all_fp_pseudos()].

all_fp_pseudos() -> [].	% XXX: for now

insn_use_gpr(I) ->
  case I of
    #move{am1=Am1} -> am1_use(Am1, []);
    #pseudo_blr{} ->
      LR = hipe_aarch64:mk_temp(hipe_aarch64_registers:lr(), 'untagged'),
      RV = hipe_aarch64:mk_temp(hipe_aarch64_registers:return_value(), 'tagged'),
      [RV, LR];
    #pseudo_tailcall{funv=FunV,arity=Arity,stkargs=StkArgs} ->
      addargs(StkArgs, addtemps(tailcall_clobbered_gpr(), funv_use(FunV, arity_use_gpr(Arity))));
    #pseudo_li{} -> [];
    #pseudo_tailcall_prepare{} -> []
    %_ -> [] % temporarily including all default cases explicitly
  end.

addargs([Arg|Args], Set) ->
  addargs(Args, addarg(Arg, Set));
addargs([], Set) ->
  Set.

addarg(Arg, Set) ->
  case Arg of
    #aarch64_temp{} -> addtemp(Arg, Set);
    _ -> Set
  end.

arity_use_gpr(Arity) ->
  [hipe_aarch64:mk_temp(R, 'tagged')
   || R <- hipe_aarch64_registers:args(Arity)].

funv_use(FunV, Set) ->
  case FunV of
    #aarch64_temp{} -> addtemp(FunV, Set);
    _ -> Set
  end.

am1_use(Am1, Set) ->
  case Am1 of
    {_Size,_Imm,_Shift} -> Set
  end.

%%%
%%% Auxiliary operations on sets of temps
%%% These sets are small. No point using gb_trees, right?
%%%

addtemps([Arg|Args], Set) ->
  addtemps(Args, addtemp(Arg, Set));
addtemps([], Set) ->
  Set.

addtemp(Temp, Set) ->
  case lists:member(Temp, Set) of
    false -> [Temp|Set];
    _ -> Set
  end.
