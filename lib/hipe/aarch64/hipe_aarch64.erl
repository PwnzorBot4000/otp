%%% -*- erlang-indent-level: 2 -*-
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

-module(hipe_aarch64).
-export([
	 mk_temp/2,
     mk_new_temp/1,
	 is_temp/1,

	 mk_mfa/3,

	 mk_label/1,
	 is_label/1,
	 label_label/1,

	 mk_pseudo_li/2,

     mk_pseudo_tailcall/4,
     mk_pseudo_tailcall_prepare/0,

     mk_pseudo_blr/0,

	 mk_li/3,

	 mk_defun/8,
	 defun_mfa/1,
	 defun_formals/1,
	 defun_is_closure/1,
	 defun_is_leaf/1,
	 defun_code/1,
	 defun_data/1
	]).

-include("hipe_aarch64.hrl").

mk_temp(Reg, Type, Allocatable) ->
  #aarch64_temp{reg=Reg, type=Type, allocatable=Allocatable}.
mk_temp(Reg, Type) -> mk_temp(Reg, Type, true).
mk_new_temp(Type, Allocatable) ->
  mk_temp(hipe_gensym:get_next_var(aarch64), Type, Allocatable).
mk_new_temp(Type) -> mk_new_temp(Type, true).
is_temp(X) -> case X of #aarch64_temp{} -> true; _ -> false end.

mk_mfa(M, F, A) -> #aarch64_mfa{m=M, f=F, a=A}.

mk_label(Label) -> #label{label=Label}.
is_label(I) -> case I of #label{} -> true; _ -> false end.
label_label(#label{label=Label}) -> Label.

mk_pseudo_li(Dst, Imm) ->
  #pseudo_li{dst=Dst, imm=Imm, label=hipe_gensym:get_next_label(aarch64)}.

mk_pseudo_tailcall(FunV, Arity, StkArgs, Linkage) ->
  #pseudo_tailcall{funv=FunV, arity=Arity, stkargs=StkArgs, linkage=Linkage}.

mk_pseudo_tailcall_prepare() -> #pseudo_tailcall_prepare{}.

mk_pseudo_blr() -> #pseudo_blr{}.

%%% Load an integer constant into a register.

mk_li(_, Value, _) -> % (Dst, Value, Rest)
  %% XXX: expand to handle 2-instruction sequences
  case try_aluop_imm('mov', Value) of
    {_,_} -> % {NewMovOp,Am1} ->
      exit('unimplemented: try_aluop_imm, case 1');
      %[mk_move(NewMovOp, false, Dst, Am1) | Rest];
    [] ->
      exit('unimplemented: try_aluop_imm, case 2')
      %[mk_pseudo_li(Dst, Value) | Rest]
  end.

try_aluop_imm(AluOp, Imm) -> % -> {NewAluOp,Am1} or []
  case imm_to_am1(Imm) of
    (Am1={_Imm8,_Imm4}) -> {AluOp, Am1};
    [] ->
      case invert_aluop_imm(AluOp, Imm) of
	{NewAluOp,NewImm} ->
	  case imm_to_am1(NewImm) of
	    (Am1={_Imm8,_Imm4}) -> {NewAluOp, Am1};
	    [] -> []
	  end;
	[] -> []
      end
  end.

invert_aluop_imm(AluOp, Imm) ->
  case AluOp of
    'mov' -> {'mvn', bnot Imm}
  end.

imm_to_am1(Imm) -> imm_to_am1(Imm band 16#FFFFFFFF, 16).
imm_to_am1(Imm, RotCnt) ->
  throw('imm_to_am1 was called!'),
  if Imm >= 0, Imm =< 255 -> {Imm, RotCnt band 15};
     true ->
      NewRotCnt = RotCnt - 1,
      if NewRotCnt =:= 0 -> []; % full circle, no joy
	 true ->
	  NewImm = (Imm bsr 2) bor ((Imm band 3) bsl 30),
	  imm_to_am1(NewImm, NewRotCnt)
      end
  end.     

mk_defun(MFA, Formals, IsClosure, IsLeaf, Code, Data, VarRange, LabelRange) ->
  #defun{mfa=MFA, formals=Formals, code=Code, data=Data,
	 isclosure=IsClosure, isleaf=IsLeaf,
	 var_range=VarRange, label_range=LabelRange}.
defun_mfa(#defun{mfa=MFA}) -> MFA.
defun_formals(#defun{formals=Formals}) -> Formals.
defun_is_closure(#defun{isclosure=IsClosure}) -> IsClosure.
defun_is_leaf(#defun{isleaf=IsLeaf}) -> IsLeaf.
defun_code(#defun{code=Code}) -> Code.
defun_data(#defun{data=Data}) -> Data.
