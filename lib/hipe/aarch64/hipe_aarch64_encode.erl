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

data_imm_addsub_form(Op, S, Imm2, Imm12, Rn, Rd) ->
  ?BIT(31,1) bor ?BIT(30,Op) bor ?BIT(29,S) bor ?BF(28,24,2#10001) bor ?BF(23,22,Imm2) bor ?BF(21,10,Imm12) bor ?BF(9,5,Rn) bor ?BF(4,0,Rd).

data_addsub_form(Op, {{'cond',_Cond},{s,S},{r,Rd},{r,Rn},Opnd}) ->
  case Opnd of
    {'immediate', {{imm12,Imm12},{imm2,Imm2}}} ->
      data_imm_addsub_form(Op, S, Imm2, Imm12, Rn, Rd)
  end.

sub(Opnds) -> data_addsub_form(2#1, Opnds).

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

%%%
%%% Main Encode Dispatch
%%%

insn_encode(Op, Opnds) ->
  case Op of
    'ldr' -> ldr(Opnds);
    'str' -> str(Opnds);
    'sub' -> sub(Opnds);
    _ -> exit({?MODULE,insn_encode,Op})
  end.
