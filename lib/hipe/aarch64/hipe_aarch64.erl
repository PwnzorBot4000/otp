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
     temp_reg/1,
	 temp_type/1,
	 temp_is_allocatable/1,
     temp_is_precoloured/1,

	 mk_mfa/3,

     mk_prim/1,

     mk_b_fun/2,

     mk_b_label/2,
     mk_b_label/1,

     mk_bl/3,

     mk_cmp/3,

     mk_sdesc/4,

	 mk_label/1,
	 is_label/1,
	 label_label/1,

	 mk_load/3,
	 mk_load/6,

	 mk_pseudo_bc/4,

	 mk_pseudo_li/2,

	 is_pseudo_move/1,
	 pseudo_move_dst/1,
	 pseudo_move_src/1,

     is_pseudo_spill_move/1,

     mk_pseudo_tailcall/4,
	 pseudo_tailcall_funv/1,
	 pseudo_tailcall_stkargs/1,
	 pseudo_tailcall_linkage/1,

     mk_pseudo_tailcall_prepare/0,

     mk_store/3,
     mk_store/6,

     mk_pseudo_blr/0,
     mk_bx/1,
     mk_mflr/1,
     mk_mtlr/1,
     mk_lr/0,

	 mk_li/3,

     mk_addi/4,

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
temp_reg(#aarch64_temp{reg=Reg}) -> Reg.
temp_type(#aarch64_temp{type=Type}) -> Type.
temp_is_allocatable(#aarch64_temp{allocatable=A}) -> A.
temp_is_precoloured(#aarch64_temp{reg=Reg}) ->
  hipe_aarch64_registers:is_precoloured_gpr(Reg).

mk_mfa(M, F, A) -> #aarch64_mfa{m=M, f=F, a=A}.

mk_prim(Prim) -> #aarch64_prim{prim=Prim}.

mk_alu(AluOp, S, Dst, Src, Am1) ->
  #alu{aluop=AluOp, s=S, dst=Dst, src=Src, am1=Am1}.
mk_alu(AluOp, Dst, Src, Am1) -> mk_alu(AluOp, false, Dst, Src, Am1).

mk_b_fun(Fun, Linkage) -> #b_fun{'fun'=Fun, linkage=Linkage}.

mk_b_label(Cond, Label) -> #b_label{'cond'=Cond, label=Label}.
mk_b_label(Label) -> mk_b_label('al', Label).

mk_bl(Fun, SDesc, Linkage) -> #bl{'fun'=Fun, sdesc=SDesc, linkage=Linkage}.

mk_cmp(CmpOp, Src, Am1) -> #cmp{cmpop=CmpOp, src=Src, am1=Am1}.

mk_sdesc(ExnLab, FSize, Arity, Live) ->
  #aarch64_sdesc{exnlab=ExnLab, fsize=FSize, arity=Arity, live=Live}.

mk_label(Label) -> #label{label=Label}.
is_label(I) -> case I of #label{} -> true; _ -> false end.
label_label(#label{label=Label}) -> Label.

% Register - relative loads in aarch64 will accept a 12-bit scaled
% unsigned immediate offset, meaning a range of 0 - 32760.
% For greater range or negative offsets, we use register offset
% via a scratch register.
% The scratch register can be the destination register, since
% its contents are ultimately going to be overridden.

mk_load(LdOp, Dst, Am2) -> #load{ldop=LdOp, dst=Dst, am2=Am2}.

mk_load(LdOp, Dst, Base, Offset, Scratch, Rest) when is_integer(Offset) ->
  if Offset >= 0 andalso Offset =< 32760 ->
      Am2 = #am2{src=Base,offset=Offset},
      [mk_load(LdOp, Dst, Am2) | Rest];
     true ->
      Index =
    begin
      DstReg = temp_reg(Dst),
      BaseReg = temp_reg(Base),
      if DstReg =/= BaseReg -> Dst;
         true -> mk_scratch(Scratch)
      end
    end,
      Am2 = #am2{src=Base,offset=Index},
      mk_li(Index, Offset,
	    [mk_load(LdOp, Dst, Am2) | Rest])
  end.

mk_scratch(Scratch) ->
  case Scratch of
    'temp2' -> mk_temp(hipe_aarch64_registers:temp2(), 'untagged');
    'new' -> mk_new_temp('untagged')
  end.

mk_move(MovOp, S, Dst, Am1) -> #move{movop=MovOp, s=S, dst=Dst, am1=Am1}.
mk_move(Dst, Am1) -> mk_move('mov', false, Dst, Am1).

mk_pseudo_bc(Cond, TrueLab, FalseLab, Pred) ->
  if Pred >= 0.5 ->
      mk_pseudo_bc_simple(negate_cond(Cond), FalseLab,
			  TrueLab, 1.0-Pred);
     true ->
      mk_pseudo_bc_simple(Cond, TrueLab, FalseLab, Pred)
  end.

mk_pseudo_bc_simple(Cond, TrueLab, FalseLab, Pred) when Pred =< 0.5 ->
  #pseudo_bc{'cond'=Cond, true_label=TrueLab,
	     false_label=FalseLab, pred=Pred}.

negate_cond(Cond) ->
  case Cond of
    'lt' -> 'ge';	% <, >=
    'ge' -> 'lt';	% >=, <
    'gt' -> 'le';	% >, <=
    'le' -> 'gt';	% <=, >
    'eq' -> 'ne';	% ==, !=
    'ne' -> 'eq';	% !=, ==
    'hi' -> 'ls';	% >u, <=u
    'ls' -> 'hi';	% <=u, >u
    'hs' -> 'lo';	% >=u, <u
    'lo' -> 'hs';	% <u, >=u
    'vs' -> 'vc';	% overflow, not_overflow
    'vc' -> 'vs'	% not_overflow, overflow
  end.

mk_pseudo_li(Dst, Imm) ->
  #pseudo_li{dst=Dst, imm=Imm, label=hipe_gensym:get_next_label(aarch64)}.

is_pseudo_move(I) -> case I of #pseudo_move{} -> true; _ -> false end.
pseudo_move_dst(#pseudo_move{dst=Dst}) -> Dst.
pseudo_move_src(#pseudo_move{src=Src}) -> Src.

is_pseudo_spill_move(I) -> is_record(I, pseudo_spill_move).

mk_pseudo_tailcall(FunV, Arity, StkArgs, Linkage) ->
  #pseudo_tailcall{funv=FunV, arity=Arity, stkargs=StkArgs, linkage=Linkage}.
pseudo_tailcall_funv(#pseudo_tailcall{funv=FunV}) -> FunV.
pseudo_tailcall_stkargs(#pseudo_tailcall{stkargs=StkArgs}) -> StkArgs.
pseudo_tailcall_linkage(#pseudo_tailcall{linkage=Linkage}) -> Linkage.

mk_pseudo_tailcall_prepare() -> #pseudo_tailcall_prepare{}.

% Register-relative stores in aarch64 will accept a 12-bit scaled
% unsigned immediate offset, meaning a range of 0 - 32760.
% For greater range or negative offsets, we use register offset
% via a scratch register.

mk_store(StOp, Src, Am2) -> #store{stop=StOp, src=Src, am2=Am2}.

mk_store(StOp, Src, Base, Offset, Scratch, Rest) when is_integer(Offset) ->
  if Offset >= 0 andalso Offset =< 32760 ->
      Am2 = #am2{src=Base,offset=Offset},
      [mk_store(StOp, Src, Am2) | Rest];
     true ->
      Index = mk_scratch(Scratch),
      Am2 = #am2{src=Base,offset=Index},
      mk_li(Index, Offset,
	    [mk_store(StOp, Src, Am2) | Rest])
  end.

mk_pseudo_blr() -> #pseudo_blr{}.
mk_bx(Src) -> #pseudo_bx{src=Src}.
mk_mflr(Dst) -> mk_move(Dst, mk_lr()).
mk_mtlr(Src) -> mk_move(mk_lr(), Src).
mk_lr() -> mk_temp(hipe_aarch64_registers:lr(), 'untagged').

%%% Load an integer constant into a register.

mk_li(Dst, Value, Rest) ->
  %% XXX: expand to handle 2-instruction sequences
  case try_aluop_imm('mov', Value) of
    {NewMovOp,Am1} ->
      [mk_move(NewMovOp, false, Dst, Am1) | Rest];
    [] ->
      [mk_pseudo_li(Dst, Value) | Rest]
  end.

%%% Add an integer constant. Dst may equal Src,
%%% in which case temp2 may be clobbered.

mk_addi(Dst, Src, Value, Rest) ->
  case try_aluop_imm('add', Value) of
    {NewAluOp,Am1} ->
      [mk_alu(NewAluOp, Dst, Src, Am1) | Rest];
    [] ->
      Tmp =
	begin
	  DstReg = temp_reg(Dst),
	  SrcReg = temp_reg(Src),
	  if DstReg =:= SrcReg ->
	      mk_temp(hipe_aarch64_registers:temp2(), 'untagged');
	     true -> Dst
	  end
	end,
      [mk_pseudo_li(Tmp, Value), mk_alu('add', Dst, Src, Tmp) | Rest]
  end.

%%% Arithmetic operations (add, cmp) accept a 12-bit immediate.
%%% Move operations accept a 16-bit immediate.

try_aluop_imm(AluOp, Imm) -> % -> {NewAluOp,Am1} or []
  ImmSize = case AluOp of
    'mov' -> imm16;
    _     -> imm12
  end,
  case imm_to_am1(Imm, ImmSize) of
    (Am1={_Size,_Imm,_Imm2}) -> {AluOp, Am1};
    [] ->
      case invert_aluop_imm(AluOp, Imm) of
	{NewAluOp,NewImm} ->
	  case imm_to_am1(NewImm, ImmSize) of
	    (Am1={_Size,_Imm,_Imm2}) -> {NewAluOp, Am1};
	    [] -> []
	  end;
	[] -> []
      end
  end.

invert_aluop_imm(AluOp, Imm) ->
  case AluOp of
    'mov' -> {'mvn', bnot Imm};
    'add' -> {'sub', -Imm}
  end.

%%% Create a 'shifter operand'.
%%% Immediates on moves have to be 16 bits long, however
%%% immediate - accepting operations can be accompanied by this
%%% value of 2 bits in size, which denotes the number of positions
%%% the immediate will be shifted left, which can be 0, 16, 32 or 48.
%%% Immediates on alu's are 12 bits long, and can be shifted
%%% by 0 or 12 bits to the left.
%%% Here we are converting a 64-bit value into a
%%% 12/16-bit immediate and a 2-bit shifter operand.

imm_to_am1(Imm, Size) ->
  Imm64 = Imm band 16#FFFFFFFFFFFFFFFF,
  SplitImm = case Size of
    imm12 ->
      if (Imm64 =< 16#FFFFFF) ->
        imm_to_am1(Imm64, 12, 16#FFF, 0);
      true ->
        [] % Alu's can't shift more than 12 bits.
      end;
    imm16 ->
      imm_to_am1(Imm64, 16, 16#FFFF, 0)
  end,
  case SplitImm of
    [] -> [];
    {ShiftedImm, Shifts} -> {Size, ShiftedImm, Shifts}
  end.
imm_to_am1(Imm, Size, MaxImm, RotCnt) ->
  if Imm >= 0, Imm =< MaxImm -> {Imm, RotCnt band 3};
     true ->
      if (Imm band MaxImm) =/= 0 -> []; % Imm can't fit in single op
     true ->
      NewRotCnt = RotCnt + 1,
	  NewImm = Imm bsr Size,
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
