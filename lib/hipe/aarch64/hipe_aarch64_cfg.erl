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

-module(hipe_aarch64_cfg).

-export([init/1,
         labels/1, start_label/1,
         succ/2,
         map_bbs/2, fold_bbs/3,
         bb/2, bb_add/3]).
-export([postorder/1]).
-export([linearise/1]).
-export([params/1, reverse_postorder/1]).
-export([arity/1]). % for linear scan
-export([branch_preds/1]).

%%% these tell cfg.inc what to define (ugly as hell)
-define(BREADTH_ORDER,true).  % for linear scan
-define(PARAMS_NEEDED,true).
-define(START_LABEL_UPDATE_NEEDED,true).
-define(MAP_FOLD_NEEDED,true).

-include("hipe_aarch64.hrl").
-include("../flow/cfg.hrl").
-include("../flow/cfg.inc").

init(Defun) ->
  Code = hipe_aarch64:defun_code(Defun),
  StartLab = hipe_aarch64:label_label(hd(Code)),
  Data = hipe_aarch64:defun_data(Defun),
  IsClosure = hipe_aarch64:defun_is_closure(Defun),
  Name = hipe_aarch64:defun_mfa(Defun),
  IsLeaf = hipe_aarch64:defun_is_leaf(Defun),
  Formals = hipe_aarch64:defun_formals(Defun),
  CFG0 = mk_empty_cfg(Name, StartLab, Data, IsClosure, IsLeaf, Formals),
  take_bbs(Code, CFG0).

is_branch(I) ->
  case I of
    #b_fun{} -> true;
    #b_label{'cond'='al'} -> true;
    #pseudo_bc{} -> true;
    #pseudo_blr{} -> true;
    #pseudo_bx{} -> true;
    #pseudo_call{} -> true;
    #pseudo_switch{} -> true;
    #pseudo_tailcall{} -> true;
    _ -> false
  end.

branch_successors(Branch) ->
  case Branch of
    #b_fun{} -> [];
    #b_label{'cond'='al',label=Label} -> [Label];
    #pseudo_bc{true_label=TrueLab,false_label=FalseLab} -> [FalseLab,TrueLab];
    #pseudo_blr{} -> [];
    #pseudo_bx{} -> [];
    #pseudo_call{contlab=ContLab, sdesc=#aarch64_sdesc{exnlab=ExnLab}} ->
      case ExnLab of
	[] -> [ContLab];
	_ -> [ContLab,ExnLab]
      end;
    #pseudo_tailcall{} -> []
  end.

branch_preds(Branch) ->
  case Branch of
    #pseudo_bc{true_label=TrueLab,false_label=FalseLab,pred=Pred} ->
      [{FalseLab, 1.0-Pred}, {TrueLab, Pred}];
    #pseudo_call{contlab=ContLab, sdesc=#aarch64_sdesc{exnlab=[]}} ->
      %% A function can still cause an exception, even if we won't catch it
      [{ContLab, 1.0-hipe_bb_weights:call_exn_pred()}];
    #pseudo_call{contlab=ContLab, sdesc=#aarch64_sdesc{exnlab=ExnLab}} ->
      CallExnPred = hipe_bb_weights:call_exn_pred(),
      [{ContLab, 1.0-CallExnPred}, {ExnLab, CallExnPred}];
    #pseudo_switch{labels=_Labels} ->
      throw(unimplemented);
    _ ->
      case branch_successors(Branch) of
	[] -> [];
	[Single] -> [{Single, 1.0}]
      end
  end.

mk_goto(Label) ->
  hipe_aarch64:mk_b_label(Label).

is_label(I) ->
  hipe_aarch64:is_label(I).

label_name(Label) ->
  hipe_aarch64:label_label(Label).

mk_label(Name) ->
  hipe_aarch64:mk_label(Name).

linearise(CFG) ->	% -> defun, not insn list
  MFA = function(CFG),
  Formals = params(CFG),
  Code = linearize_cfg(CFG),
  Data = data(CFG),
  VarRange = hipe_gensym:var_range(aarch64),
  LabelRange = hipe_gensym:label_range(aarch64),
  IsClosure = is_closure(CFG),
  IsLeaf = is_leaf(CFG),
  hipe_aarch64:mk_defun(MFA, Formals, IsClosure, IsLeaf,
            Code, Data, VarRange, LabelRange).

arity(CFG) ->
  {_M, _F, A} = function(CFG),
  A.
