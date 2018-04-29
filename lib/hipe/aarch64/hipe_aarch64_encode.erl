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
%%% Encode symbolic AARCH64 instructions to binary form.
%%% Copyright (C) 2018  Thanasis Papoutsidakis
%%%
%%% Instruction Operands:
%%%
%%% S		    ::= {s,0} | {s,1}
%%% R		    ::= {r,RNum}
%%%
%%% Cond	    ::= {cond,CondName}
%%% CondName	    ::= al
%%%
%%% Imm<N>	    ::= {imm<N>,<N bits>} for N in 2, 12 and 16
%%%
%%% ShiftedImmediate
%%%		    ::=	{'immediate', {Imm12,Imm2}}
%%%		      | {'immediate', {Imm16,Imm2}}

-module(hipe_aarch64_encode).

-export([insn_encode/2]).

-define(ASSERT(G),
	if G -> [];
	   true -> exit({assertion_failed,?MODULE,?LINE,??G})
	end).

bf(LeftBit, RightBit, Value) ->
  ?ASSERT(32 > LeftBit),
  ?ASSERT(LeftBit >= RightBit),
  ?ASSERT(RightBit >= 0),
  ?ASSERT(Value >= 0),
  ?ASSERT(Value < (1 bsl ((LeftBit - RightBit) + 1))),
  Value bsl RightBit.

% Bitfield accepting a signed number.
bfs(LeftBit, RightBit, Value) ->
  ?ASSERT(32 > LeftBit),
  ?ASSERT(LeftBit >= RightBit),
  ?ASSERT(RightBit >= 0),
  ?ASSERT(Value >= (-1 bsl (LeftBit - RightBit))),
  ?ASSERT(Value < (1 bsl (LeftBit - RightBit))),
  (Value bsl RightBit) band ((1 bsl (LeftBit + 1)) - 1).

-define(BF(LB,RB,V), bf(LB,RB,V)).
-define(BFS(LB,RB,V), bfs(LB,RB,V)).
-define(BIT(Pos,Val), ?BF(Pos,Pos,Val)).
%%-define(BITS(N,Val), ?BF(N,0,Val)).

'cond'(Cond) ->
  case Cond of
    'eq' -> 2#0000;	% equal
    'ne' -> 2#0001;	% not equal
    'cs' -> 2#0010;	% carry set
    'hs' -> 2#0010;	% unsigned higher or same
    'cc' -> 2#0011;	% carry clear
    'lo' -> 2#0011;	% unsigned lower
    'mi' -> 2#0100;	% minus / negative
    'pl' -> 2#0101;	% plus/positive or zero
    'vs' -> 2#0110;	% overflow
    'vc' -> 2#0111;	% no overflow
    'hi' -> 2#1000;	% unsigned higher
    'ls' -> 2#1001;	% unsigned lower or same
    'ge' -> 2#1010;	% signed greater than or equal
    'lt' -> 2#1011;	% signed less than
    'gt' -> 2#1100;	% signed greater than
    'le' -> 2#1101;	% signed less than or equal
    'al' -> 2#1110 	% always
  end.

%%%
%%% AARCH64 Instructions
%%%

%%% Data Processing - Arithmetic

data_imm_addsub_form(Sf, Op, S, Shift, Imm12, Rn, Rd) ->
  ?BIT(31,Sf) bor ?BIT(30,Op) bor ?BIT(29,S) bor ?BF(28,24,2#10001) bor ?BF(23,22,Shift) bor ?BF(21,10,Imm12) bor ?BF(9,5,Rn) bor ?BF(4,0,Rd).

data_reg_ext_addsub_form(Sf, Op, S, Opt, Rm, Option, Imm3, Rn, Rd) ->
  ?BIT(31,Sf) bor ?BIT(30,Op) bor ?BIT(29,S) bor ?BF(28,24,2#01011) bor ?BF(23,22,Opt) bor ?BIT(21, 1) bor ?BF(20,16,Rm) bor ?BF(15,13,Option) bor ?BF(12,10,Imm3) bor ?BF(9,5,Rn) bor ?BF(4,0,Rd).

data_addsub_form(Op, S, {r,Rd}, {r,Rn}, AmOpnd) ->
  case AmOpnd of
    {'immediate', {imm12, Imm12}, {imm2, Imm2}} ->
      data_imm_addsub_form(1, Op, S, Imm2, Imm12, Rn, Rd);
    {r, Reg} ->
      data_reg_ext_addsub_form(1, Op, S, 2#00, Reg, 2#011, 2#000, Rn, Rd)
  end.

add({{'cond', 'al'}, {s,S}, Dst, Opnd, AmOpnd}) ->
  data_addsub_form(0, S, Dst, Opnd, AmOpnd).

sub({{'cond', 'al'}, {s,S}, Dst, Opnd, AmOpnd}) ->
  data_addsub_form(1, S, Dst, Opnd, AmOpnd).

cmp({{'cond', 'al'}, Opnd, AmOpnd}) ->
  data_addsub_form(1, 1, {r, 31}, Opnd, AmOpnd).

cmn({{'cond', 'al'}, Opnd, AmOpnd}) ->
  data_addsub_form(0, 1, {r, 31}, Opnd, AmOpnd).

%%% Data Processing - Bitfield

data_imm_bitfield_form(Sf, Opc, N, Immr, Imms, Rn, Rd) ->
  ?BIT(31,Sf) bor ?BF(30,29,Opc) bor ?BF(28,23,2#100110) bor ?BIT(22,N) bor ?BF(21,16,Immr) bor ?BF(15,10,Imms) bor ?BF(9,5,Rn) bor ?BF(4,0,Rd).

data_reg_2src_form(Sf, S, Rm, Opcode, Rn, Rd) ->
  ?BIT(31,Sf) bor ?BIT(30,0) bor ?BIT(29,S) bor ?BF(28,21,2#11010110) bor ?BF(20,16,Rm) bor ?BF(15,10,Opcode) bor ?BF(9,5,Rn) bor ?BF(4,0,Rd).

sbfm({r,Dst}, {r,Src}, Immr, Imms) ->
  data_imm_bitfield_form(1, 2#00, 1, Immr, Imms, Src, Dst).

ubfm({r,Dst}, {r,Src}, Immr, Imms) ->
  data_imm_bitfield_form(1, 2#10, 1, Immr, Imms, Src, Dst).

asr({{'cond', 'al'}, _S, Dst, Src, Shift}) ->
  case Shift of
    {'immediate', {imm6, Imm6}} ->
      sbfm(Dst, Src, Imm6, 2#111111)
  end.

lsl({{'cond', 'al'}, _S, Dst, Src, Shift}) ->
  case Shift of
    {'immediate', {imm6, Imm6}} when Imm6 =/= 0 ->
      ubfm(Dst, Src, 64 - Imm6, 63 - Imm6);
    {r, Rm} ->
      {r, Rn} = Src,
      {r, Rd} = Dst,
      data_reg_2src_form(1, 0, Rm, 2#001000, Rn, Rd)
  end.

lsr({{'cond', 'al'}, _S, Dst, Src, Shift}) ->
  case Shift of
    {'immediate', {imm6, Imm6}} when Imm6 =/= 0 ->
      ubfm(Dst, Src, Imm6, 63);
    {r, Rm} ->
      {r, Rn} = Src,
      {r, Rd} = Dst,
      data_reg_2src_form(1, 0, Rm, 2#001001, Rn, Rd)
  end.

%%% Data Processing - Logical

data_imm_logical_form(Sf, Opc, N, Imms, Immr, Rn, Rd) ->
  ?BIT(31,Sf) bor ?BF(30,29,Opc) bor ?BF(28,23,2#100100) bor ?BIT(22,N) bor ?BF(21,16,Immr) bor ?BF(15,10,Imms) bor ?BF(9,5,Rn) bor ?BF(4,0,Rd).

data_reg_shift_logical_form(Sf, Opc, Shift, N, Rm, Imm6, Rn, Rd) ->
  ?BIT(31,Sf) bor ?BF(30,29,Opc) bor ?BF(28,24,2#01010) bor ?BF(23,22,Shift) bor ?BIT(21,N) bor ?BF(20,16,Rm) bor ?BF(15,10,Imm6) bor ?BF(9,5,Rn) bor ?BF(4,0,Rd).

orr({{'cond', 'al'}, {s,_S}, {r,Dst}, {r,Opnd}, AmOpnd}) ->
  case AmOpnd of
    {'bitmask', {n, N}, {imms, Imms}, {immr, Immr}} ->
      data_imm_logical_form(1, 2#01, N, Imms, Immr, Opnd, Dst);
    {r, Register} ->
      data_reg_shift_logical_form(1, 2#01, 2#00, 0, Register, 2#000000, Opnd, Dst)
  end.

'and'({{'cond', 'al'}, {s,_S}, {r,Dst}, {r,Opnd}, AmOpnd}) ->
  case AmOpnd of
    {'bitmask', {n, N}, {imms, Imms}, {immr, Immr}} ->
      data_imm_logical_form(1, 2#00, N, Imms, Immr, Opnd, Dst);
    {r, Register} ->
      data_reg_shift_logical_form(1, 2#00, 2#00, 0, Register, 2#000000, Opnd, Dst)
  end.

tst({{'cond', 'al'}, {r, Opnd}, AmOpnd}) ->
  case AmOpnd of
    {'bitmask', {n, N}, {imms, Imms}, {immr, Immr}} ->
      data_imm_logical_form(1, 2#11, N, Imms, Immr, Opnd, 31);
    {r, Register} ->
      data_reg_shift_logical_form(1, 2#11, 2#00, 0, Register, 2#000000, Opnd, 31)
  end.

%%% Data Processing - Move

data_imm_mov_form(Sf, Opc, Hw, Imm16, Rd) ->
  ?BIT(31,Sf) bor ?BF(30,29,Opc) bor ?BF(28,23,2#100101) bor ?BF(22,21,Hw) bor ?BF(20,5,Imm16) bor ?BF(4,0,Rd).

mov({{'cond', 'al'}, {s,0}, {r, Dst}, Src}) ->
  case Src of
    {'immediate', {imm16, Imm16}, {imm2, Imm2}} ->
      data_imm_mov_form(1, 2#10, Imm2, Imm16, Dst);
    {r, Register} ->
      data_reg_shift_logical_form(1, 2#01, 2#00, 0, Register, 2#000000, 2#11111, Dst)
  end.

mvn({{'cond', 'al'}, {s,0}, {r, Dst}, Src}) ->
  case Src of
    {'immediate', {imm16, Imm16}, {imm2, Imm2}} ->
      data_imm_mov_form(1, 2#00, Imm2, Imm16, Dst)
  end.

%%% Data Processing - Multiply

data_reg_3src_form(Sf, Op54, Op31, Rm, O0, Ra, Rn, Rd) ->
  ?BIT(31,Sf) bor ?BF(30,29,Op54) bor ?BF(28,24,2#11011) bor ?BF(23,21,Op31) bor ?BF(20,16,Rm) bor ?BIT(15,O0) bor ?BF(14,10,Ra) bor ?BF(9,5,Rn) bor ?BF(4,0, Rd).

smaddl(Dst, Src1, Src2, SrcAdd) ->
  data_reg_3src_form(1, 2#00, 2#001, Src1, 0, SrcAdd, Src2, Dst).

smull({{'cond', 'al'}, {s,0}, {r, Dst}, {r, Src1}, {r, Src2}}) ->
  smaddl(Dst, Src1, Src2, 31).

smulh({{'cond', 'al'}, {s,0}, {r, Dst}, {r, Src1}, {r, Src2}}) ->
  data_reg_3src_form(1, 2#00, 2#010, Src1, 0, 31, Src2, Dst).

%%% Loads / Stores

ldstr_imm_form(Size, V, Opc, Imm12, Rn, Rt) ->
  ?BF(31,30,Size) bor ?BF(29,27,2#111) bor ?BIT(26,V) bor ?BF(25,24,2#01) bor ?BF(23,22,Opc) bor ?BF(21,10,Imm12) bor ?BF(9,5,Rn) bor ?BF(4,0,Rt).

ldstr_unscaled_form(Size, V, Opc, Imm9, Rn, Rt) ->
  ?BF(31,30,Size) bor ?BF(29,27,2#111) bor ?BIT(26,V) bor ?BF(25,24,2#00) bor ?BF(23,22,Opc) bor ?BIT(21,0) bor ?BFS(20,12,Imm9) bor ?BF(12,10,2#00) bor ?BF(9,5,Rn) bor ?BF(4,0,Rt).

ldstr_reg_form(Size, V, Opc, Rm, Option, S, Rn, Rt) ->
  ?BF(31,30,Size) bor ?BF(29,27,2#111) bor ?BIT(26,V) bor ?BF(25,24,2#00) bor ?BF(23,22,Opc) bor ?BIT(21,1) bor ?BF(20,16,Rm) bor ?BF(15,13,Option) bor ?BIT(12,S) bor ?BF(11,10,2#10) bor ?BF(9,5,Rn) bor ?BF(4,0,Rt).

ldstr_pcrel_form(Opc, V, Imm19, Rt) ->
  ?BF(31,30,Opc) bor ?BF(29,27,2#011) bor ?BIT(26,V) bor ?BF(25,24,2#00) bor ?BFS(23,5,Imm19) bor ?BF(4,0,Rt).

ldr({Size, {r, Dst}, Src}) ->
  case Src of
    {'immediate_offset', {r, Base}, {imm12, Offset}} ->
      ldstr_imm_form(Size, 2#0, 2#01, Offset, Base, Dst);
    {'unscaled_offset', {r, Base}, {imm9, Offset}} ->
      ldstr_unscaled_form(Size, 2#0, 2#01, Offset, Base, Dst);
    {'register_offset', {r, Base}, {r, Offset}} ->
      ldstr_reg_form(Size, 2#0, 2#01, Offset, 2#011, 0, Base, Dst);
    {'pc-relative', {imm19, Offset}} ->
      ldstr_pcrel_form(2#01, 2#0, Offset, Dst)
  end.

ldrb({_Size, Dst, Src}) ->
  ldr({2#00, Dst, Src}).

str({Size, {r, Src}, Dst}) ->
  case Dst of
    {'immediate_offset', {r, Base}, {imm12, Offset}} ->
      ldstr_imm_form(Size, 2#0, 2#00, Offset, Base, Src);
    {'unscaled_offset', {r, Base}, {imm9, Offset}} ->
      ldstr_unscaled_form(Size, 2#0, 2#00, Offset, Base, Src);
    {'register_offset', {r, Base}, {r, Offset}} ->
      ldstr_reg_form(Size, 2#0, 2#00, Offset, 2#011, 0, Base, Src)
  end.

strb({_Size, Src, Dst}) ->
  str({2#00, Src, Dst}).

%%% Branches

b_imm_form(Op, Imm26) ->
  ?BIT(31, Op) bor ?BF(30,26,2#00101) bor ?BFS(25,0,Imm26).

b_reg_form(Opc, Op2, Op3, Rn, Op4) ->
  ?BF(31,25,2#1101011) bor ?BF(24,21,Opc) bor ?BF(20,16,Op2) bor ?BF(15,10,Op3) bor ?BF(9,5,Rn) bor ?BF(4,0,Op4).

bcond_imm_form(O1, Imm19, O0, Cond) ->
  ?BF(31,25,2#0101010) bor ?BIT(24, O1) bor ?BFS(23,5,Imm19) bor ?BIT(4, O0) bor ?BF(3,0,Cond).

b({{'cond', 'al'}, {imm26, Offset}}) ->
  b_imm_form(0, Offset);
b({{'cond', Cond}, {imm19, Offset}}) ->
  bcond_imm_form(0, Offset, 0, 'cond'(Cond)).

bl({{'cond', 'al'}, {imm26, Offset}}) ->
  b_imm_form(1, Offset).

br({r, Dst}) ->
  b_reg_form(2#0000, 2#11111, 2#000000, Dst, 2#00000).

blr({{'cond', 'al'}, {r, Dst}}) ->
  b_reg_form(2#0001, 2#11111, 2#000000, Dst, 2#00000).

ret(_Opnds) ->
  b_reg_form(2#0010, 2#11111, 2#000000, 30, 2#00000).

%%%
%%% Main Encode Dispatch
%%%

insn_encode(Op, Opnds) ->
  case Op of
    'add' -> add(Opnds);
    'and' -> 'and'(Opnds);
    'asr' -> asr(Opnds);
    'b'   -> b(Opnds);
    'bl'  -> bl(Opnds);
    'blr' -> blr(Opnds);
    'br'  -> br(Opnds);
    'cmp' -> cmp(Opnds);
    'cmn' -> cmn(Opnds);
    'ldr' -> ldr(Opnds);
    'ldrb' -> ldrb(Opnds);
    'lsl' -> lsl(Opnds);
    'lsr' -> lsr(Opnds);
    'mov' -> mov(Opnds);
    'mvn' -> mvn(Opnds);
    'orr' -> orr(Opnds);
    'ret' -> ret(Opnds);
    'smulh' -> smulh(Opnds);
    'smull' -> smull(Opnds);
    'str' -> str(Opnds);
    'strb' -> strb(Opnds);
    'sub' -> sub(Opnds);
    'tst' -> tst(Opnds);
    _ -> exit({?MODULE,insn_encode,Op})
  end.
