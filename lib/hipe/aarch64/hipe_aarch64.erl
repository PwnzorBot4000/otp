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
	 mk_new_nonallocatable_temp/1,
	 is_temp/1,
     temp_reg/1,
	 temp_type/1,
	 temp_is_allocatable/1,
     temp_is_precoloured/1,

	 mk_mfa/3,

     mk_prim/1,
	 is_prim/1,
     prim_prim/1,

	 mk_alu/4,
	 mk_alu/5,

     mk_b_fun/2,

     mk_b_label/2,
     mk_b_label/1,

     mk_bl/3,

     mk_blx/2,

     mk_cmp/3,

	 mk_comment/1,

     mk_sdesc/4,

     mk_am2/2,

	 mk_label/1,
	 is_label/1,
	 label_label/1,

	 mk_load/3,
	 mk_load/6,

	 mk_pseudo_call/4,
	 pseudo_call_contlab/1,
	 pseudo_call_funv/1,
	 pseudo_call_sdesc/1,
	 pseudo_call_linkage/1,

	 mk_pseudo_call_prepare/1,
	 pseudo_call_prepare_nrstkargs/1,

     mk_move/2,

	 mk_pseudo_bc/4,

	 mk_pseudo_li/2,

	 mk_pseudo_move/2,
	 is_pseudo_move/1,
	 pseudo_move_dst/1,
	 pseudo_move_src/1,

     is_pseudo_spill_move/1,

	 mk_pseudo_switch/3,

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

	 mk_li/2,
	 mk_li/3,

     mk_addi/4,

	 try_aluop_imm/2,
     bitmask_decode_pattern/3,

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
mk_new_nonallocatable_temp(Type) -> mk_new_temp(Type, false).
is_temp(X) -> case X of #aarch64_temp{} -> true; _ -> false end.
temp_reg(#aarch64_temp{reg=Reg}) -> Reg.
temp_type(#aarch64_temp{type=Type}) -> Type.
temp_is_allocatable(#aarch64_temp{allocatable=A}) -> A.
temp_is_precoloured(#aarch64_temp{reg=Reg}) ->
  hipe_aarch64_registers:is_precoloured_gpr(Reg).

mk_mfa(M, F, A) -> #aarch64_mfa{m=M, f=F, a=A}.

mk_prim(Prim) -> #aarch64_prim{prim=Prim}.
is_prim(X) -> case X of #aarch64_prim{} -> true; _ -> false end.
prim_prim(#aarch64_prim{prim=Prim}) -> Prim.

mk_am2(Src, Offset) -> #am2{src=Src, offset=Offset}.

mk_alu(AluOp, S, Dst, Src, Am1) ->
  #alu{aluop=AluOp, s=S, dst=Dst, src=Src, am1=Am1}.
mk_alu(AluOp, Dst, Src, Am1) -> mk_alu(AluOp, false, Dst, Src, Am1).

mk_b_fun(Fun, Linkage) -> #b_fun{'fun'=Fun, linkage=Linkage}.

mk_b_label(Cond, Label) -> #b_label{'cond'=Cond, label=Label}.
mk_b_label(Label) -> mk_b_label('al', Label).

mk_bl(Fun, SDesc, Linkage) -> #bl{'fun'=Fun, sdesc=SDesc, linkage=Linkage}.

mk_blx(Src, SDesc) -> #blx{src=Src, sdesc=SDesc}.

mk_cmp(CmpOp, Src, Am1) -> #cmp{cmpop=CmpOp, src=Src, am1=Am1}.

mk_sdesc(ExnLab, FSize, Arity, Live) ->
  #aarch64_sdesc{exnlab=ExnLab, fsize=FSize, arity=Arity, live=Live}.

mk_comment(Term) -> #comment{term=Term}.

mk_label(Label) -> #label{label=Label}.
is_label(I) -> case I of #label{} -> true; _ -> false end.
label_label(#label{label=Label}) -> Label.

% In loads, the scratch register can be the destination register,
% since its contents are ultimately going to be overridden.

mk_load(LdOp, Dst, Am2) -> #load{ldop=LdOp, dst=Dst, am2=Am2}.

mk_load(LdOp, Dst, Base, Offset, Scratch, Rest) ->
  Size = case LdOp of
    'ldr' -> word;
    'ldr32' -> int32;
    'ldrh' -> halfword;
    'ldrb' -> byte
  end,
  Am2 = imm_to_am2(Base, Offset, Size),
  case Am2 of
    #am2{} ->
      [mk_load(LdOp, Dst, Am2) | Rest];
    [] ->
      Index =
    begin
      DstReg = temp_reg(Dst),
      BaseReg = temp_reg(Base),
      if DstReg =/= BaseReg -> Dst;
         true -> mk_scratch(Scratch)
      end
    end,
      NewAm2 = mk_am2(Base, Index),
      mk_li(Index, Offset,
        [mk_load(LdOp, Dst, NewAm2) | Rest])
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

mk_pseudo_call(FunV, SDesc, ContLab, Linkage) ->
  #pseudo_call{funv=FunV, sdesc=SDesc, contlab=ContLab, linkage=Linkage}.
pseudo_call_funv(#pseudo_call{funv=FunV}) -> FunV.
pseudo_call_sdesc(#pseudo_call{sdesc=SDesc}) -> SDesc.
pseudo_call_contlab(#pseudo_call{contlab=ContLab}) -> ContLab.
pseudo_call_linkage(#pseudo_call{linkage=Linkage}) -> Linkage.

mk_pseudo_call_prepare(NrStkArgs) ->
  #pseudo_call_prepare{nrstkargs=NrStkArgs}.
pseudo_call_prepare_nrstkargs(#pseudo_call_prepare{nrstkargs=NrStkArgs}) ->
  NrStkArgs.

mk_pseudo_li(Dst, Imm) ->
  #pseudo_li{dst=Dst, imm=Imm, label=hipe_gensym:get_next_label(aarch64)}.

mk_pseudo_move(Dst, Src) -> #pseudo_move{dst=Dst, src=Src}.
is_pseudo_move(I) -> case I of #pseudo_move{} -> true; _ -> false end.
pseudo_move_dst(#pseudo_move{dst=Dst}) -> Dst.
pseudo_move_src(#pseudo_move{src=Src}) -> Src.

is_pseudo_spill_move(I) -> is_record(I, pseudo_spill_move).

mk_pseudo_switch(JTab, Index, Labels) ->
  #pseudo_switch{jtab=JTab, index=Index, labels=Labels}.

mk_pseudo_tailcall(FunV, Arity, StkArgs, Linkage) ->
  #pseudo_tailcall{funv=FunV, arity=Arity, stkargs=StkArgs, linkage=Linkage}.
pseudo_tailcall_funv(#pseudo_tailcall{funv=FunV}) -> FunV.
pseudo_tailcall_stkargs(#pseudo_tailcall{stkargs=StkArgs}) -> StkArgs.
pseudo_tailcall_linkage(#pseudo_tailcall{linkage=Linkage}) -> Linkage.

mk_pseudo_tailcall_prepare() -> #pseudo_tailcall_prepare{}.

mk_store(StOp, Src, Am2) -> #store{stop=StOp, src=Src, am2=Am2}.

mk_store(StOp, Src, Base, Offset, Scratch, Rest) ->
  Size = case StOp of
    'str' -> word;
    'str32' -> int32;
    'strh' -> halfword;
    'strb' -> byte
  end,
  Am2 = imm_to_am2(Base, Offset, Size),
  case Am2 of
    #am2{} ->
      [mk_store(StOp, Src, Am2) | Rest];
    [] ->
      Index = mk_scratch(Scratch),
      NewAm2 = mk_am2(Base, Index),
      mk_li(Index, Offset,
        [mk_store(StOp, Src, NewAm2) | Rest])
  end.

mk_pseudo_blr() -> #pseudo_blr{}.
mk_bx(Src) -> #pseudo_bx{src=Src}.
mk_mflr(Dst) -> mk_move(Dst, mk_lr()).
mk_mtlr(Src) -> mk_move(mk_lr(), Src).
mk_lr() -> mk_temp(hipe_aarch64_registers:lr(), 'untagged').

%%% Load an integer constant into a register.
mk_li(Dst, Value) -> mk_li(Dst, Value, []).

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
%%% Arithmetic and move operations may accept a shifter operand.
%%% Logical operations accept a bitmask in the form 1:6:6, which
%%% describes a pattern of bits that will generate the immediate.
%%% Shift operations accept a 6-bit unsigned immediate.
%%% Negative immediates in alus and moves are inserted by inverting
%%% the operation (i.e. sub for add, cmn for cmp, mvn for mov).

try_aluop_imm(AluOp, Imm) ->
  case is_logical_op(AluOp) of
    true ->
      case imm_to_bitmask(Imm) of
        (Am1 = {bitmask, _, _, _}) -> {AluOp, Am1};
        [] -> []
      end;
    false ->
  case is_shift_op(AluOp) of
    true ->
      if Imm =/= (Imm band 2#111111) ->
        []; % Imm can't fit in 6 bits
      true ->
        {AluOp, {imm6, Imm}}
      end;
    false -> % Arithmetic / move op
      ImmSize = case AluOp of
        'mov' -> imm16;
        'mvn' -> imm16;
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
      end
  end
  end.

invert_aluop_imm(AluOp, Imm) ->
  case AluOp of
    'mov' -> {'mvn', bnot Imm};
    'mvn' -> {'mov', bnot Imm};
    'cmp' -> {'cmn', -Imm};
    'cmn' -> {'cmp', -Imm};
    'add' -> {'sub', -Imm};
    'sub' -> {'add', -Imm}
  end.

%%% Aarch64 has a special way to encode immediates for logical instructions, 
%%% where a 13-bit pattern generator is used to create the actual immediate
%%% during execution. The generation mechanism uses replication of a base
%%% subpattern, created by filling part of it with ones and then rotating it
%%% a number of positions. The metrics in the process are E, the size of the
%%% pattern to be replicated (power of 2 up to 64), S, the number of ones in
%%% the pattern (minus 1), and R, the rotation amount. The way those metrics
%%% are encoded in the 13 bits of the bitmask is N:imms:immr (1:6:6), where
%%% R = immr, S = imms with the unused bits (due to pattern size) set to 1,
%%% and N denotes no replication (64-bit pattern).

imm_to_bitmask(Imm) ->
  case Imm of
    % All-zeros and all-ones are invalid bitmask immediate cases.
    0 -> [];
    16#FFFFFFFFFFFFFFFF -> [];
    _ -> imm_to_bitmask(Imm, 2)
  end.
imm_to_bitmask(Imm, Esize) when Esize =< 64 ->
  Pattern = Imm band ones(Esize),
  case replicates64(Pattern, Imm, Esize, Esize) of
    false ->
      imm_to_bitmask(Imm, Esize * 2);
    true ->
      Rones = countbits(1, Pattern, Esize),
      Rzeros = countbits(0, Pattern bsr Rones, Esize - Rones),
      R0 = Rzeros + Rones,
      Sones = countbits(1, Pattern bsr R0, Esize - R0),
      S = Sones + Rones - 1,
      R = Esize - R0,
      Generated = bitmask_decode_pattern(S, R, Esize),
      if (Generated == Pattern) ->
          Imms = (S band (Esize - 1)) bor (ones(6) - (Esize - 1)),
          N = if (Esize == 64) -> 1;
            true -> 0
          end,
          {bitmask, N, Imms, R};
        true -> []
      end
  end.

%%% Generate the pattern to be replicated from the encode.
bitmask_decode_pattern(S, R, Esize) ->
  ror(ones(S + 1), R, Esize).

%%% Return a binary number consisting of N 'one' bits.
ones(N) -> (1 bsl N) - 1.

%%% Binary rotate right.
ror(Number, Positions, Size) ->
  Shifted = Number bsr Positions,
  Recycled = (Number band ones(Positions)) bsl (Size - Positions),
  Shifted bor Recycled.

%%% Test whether a pattern replicates inside
%%% a 64-bit binary number.
replicates64(_Pattern, _Number, _Size, Start) when Start >= 64 ->
  64 = Start, % sanity check for misalignment
  true;
replicates64(Pattern, Number, Size, Start) ->
  Mask = ones(Size) bsl Start,
  if ((Mask band Number) bsr Start == Pattern) ->
      replicates64(Pattern, Number, Size, Start + Size);
    true -> false
  end.

%%% Count the consequent bits of a given Number that are
%%% equal to Value, starting from the end.
countbits(_Value, _Number, 0) -> 0;
countbits(Value, Number, Length) ->
  if (Number band 1 == Value) ->
      1 + countbits(Value, Number bsr 1, Length - 1);
    true -> 0
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

imm_to_am1(Imm, _Size) when Imm < 0 -> [];
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
imm_to_am1(Imm, Size, Mask, RotCnt) ->
  if Imm >= 0, Imm =< Mask -> {Imm, RotCnt band 3};
     true ->
      if (Imm band Mask) =/= 0 -> []; % Imm can't fit in single op
     true ->
      NewRotCnt = RotCnt + 1,
	  NewImm = Imm bsr Size,
	  imm_to_am1(NewImm, Size, Mask, NewRotCnt)
      end
  end.

is_logical_op(AluOp) ->
  case AluOp of
    'and' -> true;
    'orr' -> true;
    'eor' -> true;
    'tst' -> true;
    'b.eq' -> true;
    _ -> false
  end.

is_shift_op(AluOp) ->
  case AluOp of
    'asr' -> true;
    'lsl' -> true;
    'lsr' -> true;
    'ror' -> true;
    _ -> false
  end.

% 64-bit loads and stores in aarch64 will accept a 12-bit 8x scaled
% unsigned immediate offset, meaning a range of 0 - 16#7FF8.
% 32-bit loads and stores are 4x scaled, meaning a range
% of 0 - 16#3FFC.
% 16-bit loads and stores (halfwords) are 2x scaled, meaning a range
% of 0 - 16#1FFE.
% Byte loads and stores are unscaled, meaning a range of 0 - 16#FFF.
% For unaligned offsets, unscaled 9-bit immediate instructions exist,
% with a range of -256 - 255.
% For greater ranges, we use register offset via a scratch register.

imm_to_am2(Base, Offset, Size) when is_integer(Offset) ->
  {ULim, Align} = case Size of
    word -> {16#7FF8, 2#111};
    int32 -> {16#3FFC, 2#11};
    halfword -> {16#1FFE, 2#1};
    byte -> {16#FFF, 0}
  end,
  if (((Offset band Align) =:= 0 andalso
       Offset >= 0 andalso Offset =< ULim) orelse
      (Offset >= -256 andalso Offset =< 255))->
      #am2{src=Base,offset=Offset};
    true -> []
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
