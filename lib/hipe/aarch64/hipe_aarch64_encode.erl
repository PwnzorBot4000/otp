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

-define(BF(LB,RB,V), bf(LB,RB,V)).
-define(BIT(Pos,Val), ?BF(Pos,Pos,Val)).
%%-define(BITS(N,Val), ?BF(N,0,Val)).

%%%
%%% AARCH64 Instructions
%%%

%%% Data Processing

data_imm_addsub_form(Sf, Op, S, Shift, Imm12, Rn, Rd) ->
  ?BIT(31,Sf) bor ?BIT(30,Op) bor ?BIT(29,S) bor ?BF(28,24,2#10001) bor ?BF(23,22,Shift) bor ?BF(21,10,Imm12) bor ?BF(9,5,Rn) bor ?BF(4,0,Rd).

data_addsub_form(Op, {{'cond',_Cond},{s,S},{r,Rd},{r,Rn},Opnd}) ->
  case Opnd of
    {'immediate', {imm12, Imm12}, {imm2, Imm2}} ->
      data_imm_addsub_form(2#1, Op, S, Imm2, Imm12, Rn, Rd)
  end.

add(Opnds) -> data_addsub_form(2#0, Opnds).
sub(Opnds) -> data_addsub_form(2#1, Opnds).

data_mov_form(Sf, Opc, Hw, Imm16, Rd) ->
  ?BIT(31,Sf) bor ?BF(30,29,Opc) bor ?BF(28,23,2#100101) bor ?BF(22,21,Hw) bor ?BF(20,5,Imm16) bor ?BF(4,0,Rd).

mov({_Cond, _S, {r, Dst}, Src}) ->
  case Src of
    {'immediate', {imm16, Imm16}, {imm2, Imm2}} ->
      data_mov_form(2#1, 2#00, Imm2, Imm16, Dst)
  end.

%%% Loads / Stores

ldstr_imm_form(Size, V, Opc, Imm12, Rn, Rt) ->
  ?BF(31,30,Size) bor ?BF(29,27,2#111) bor ?BIT(26,V) bor ?BF(25,24,2#01) bor ?BF(23,22,Opc) bor ?BF(21,10,Imm12) bor ?BF(9,5,Rn) bor ?BF(4,0,Rt).

ldstr_pcrel_form(Opc, V, Imm19, Rt) ->
  ?BF(31,30,Opc) bor ?BF(29,27,2#011) bor ?BIT(26,V) bor ?BF(25,24,2#00) bor ?BF(23,5,Imm19) bor ?BF(4,0,Rt).

ldr({_Cond, {r, Dst}, Src}) ->
  case Src of
    {'immediate_offset', {r, Base}, {imm12, Offset}} ->
      ldstr_imm_form(2#11, 2#0, 2#01, Offset, Base, Dst);
    {'pc-relative', {imm19, Offset}} ->
      ldstr_pcrel_form(2#01, 2#0, Offset, Dst)
  end.

str({_Cond, {r, Src}, Dst}) ->
  case Dst of
    {'immediate_offset', {r, Base}, {imm12, Offset}} ->
      ldstr_imm_form(2#11, 2#0, 2#00, Offset, Base, Src)
  end.

%%% Branches

b_imm_form(Op, Imm26) ->
  ?BIT(31, Op) bor ?BF(30,26,2#00101) bor ?BF(25,0,Imm26).

b_reg_form(Opc, Op2, Op3, Rn, Op4) ->
  ?BF(31,25,2#1101011) bor ?BF(24,21,Opc) bor ?BF(20,16,Op2) bor ?BF(15,10,Op3) bor ?BF(9,5,Rn) bor ?BF(4,0,Op4).

b({_Cond, {imm26, Offset}}) ->
  b_imm_form(2#0, Offset).

bl({_Cond, {imm26, Offset}}) ->
  b_imm_form(2#1, Offset).

ret(_Opnds) ->
  b_reg_form(2#0010, 2#11111, 2#000000, 30, 2#00000).

%%%
%%% Main Encode Dispatch
%%%

insn_encode(Op, Opnds) ->
  case Op of
    'add' -> add(Opnds);
    'b'   -> b(Opnds);
    'bl'  -> bl(Opnds);
    'ldr' -> ldr(Opnds);
    'mov' -> mov(Opnds);
    'ret' -> ret(Opnds);
    'str' -> str(Opnds);
    'sub' -> sub(Opnds);
    _ -> exit({?MODULE,insn_encode,Op})
  end.
