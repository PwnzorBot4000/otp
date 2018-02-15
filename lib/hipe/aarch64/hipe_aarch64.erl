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
