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

-module(hipe_aarch64_registers).

-export([first_virtual/0,
     is_precoloured_gpr/1,
     all_precoloured/0,
	 return_value/0,
	 temp1/0,	% C callee-save, not parameter, may be allocatable
	 temp2/0,	% not parameter, must not be allocatable (frame)
	 temp3/0,	% not parameter, may be allocatable
	 heap_pointer/0,
	 stack_pointer/0,
	 proc_pointer/0,
	 lr/0,
     allocatable_gpr/0,
	 nr_args/0,
	 arg/1,
	 args/1,
     call_clobbered/0,
     tailcall_clobbered/0,
	 live_at_return/0
	 ]).

-include("../rtl/hipe_literals.hrl").

-define(X0, 0).
-define(X1, 1).
-define(X2, 2).
-define(X3, 3).
-define(X4, 4).
-define(X5, 5).
-define(X6, 6).
-define(X7, 7).
-define(X8, 8).
-define(X9, 9).
-define(X10, 10).
-define(X11, 11).
-define(X12, 12).
-define(X13, 13).
-define(X14, 14).
-define(X15, 15).
-define(X16, 16).
-define(X17, 17).
-define(X18, 18).
-define(X19, 19).
-define(X20, 20).
-define(X21, 21).
-define(X22, 22).
-define(X23, 23).
-define(X24, 24).
-define(X25, 25). % TEMP_LR callee-save
-define(X26, 26). % heap pointer (HP)
-define(X27, 27). % native stack pointer (NSP)
-define(X28, 28). % proc pointer (P)
-define(X29, 29). % frame pointer
-define(X30, 30). % link register (LR)
-define(SP, 31). % C stack pointer (restricted use)
-define(LAST_PRECOLOURED, 31). % must handle both GPR and FPR ranges

-define(LR, ?X30).

-define(ARG0, ?X1).
-define(ARG1, ?X2).
-define(ARG2, ?X3).
-define(ARG3, ?X4).
-define(ARG4, ?X5).
-define(ARG5, ?X6).

-define(TEMP1, ?X25).	% stores LR around inc_stack calls, must be C calleE-save
-define(TEMP2, ?X29).
-define(TEMP3, ?X24).

-define(RETURN_VALUE, ?X0).
-define(HEAP_POINTER, ?X26).
-define(STACK_POINTER, ?X27).
-define(PROC_POINTER, ?X28).

%%% Must handle both GPR and FPR ranges.
first_virtual() -> ?LAST_PRECOLOURED + 1.

%%% These two tests have the same implementation, but that's
%%% not something we should cast in stone in the interface. 
is_precoloured_gpr(R) -> R =< ?LAST_PRECOLOURED.

all_precoloured() ->
  %% XXX: SP should be skipped as it never is used anywhere.
  %% Unfortunately, gaps in the list of precoloured registers
  %% cause the graph_color register allocator to create bogus
  %% assignments for those "registers", which in turn causes
  %% the "precoloured reg must map to itself" sanity check in
  %% the frame module to signal errors.
  [ ?X0,  ?X1,  ?X2,  ?X3,  ?X4,  ?X5,  ?X6,  ?X7,
    ?X8,  ?X9,  ?X10, ?X11, ?X12, ?X13, ?X14, ?X15,
    ?X16,  ?X17, ?X18, ?X19, ?X20, ?X21, ?X22, ?X23,
    ?X24,  ?X25, ?X26, ?X27, ?X28, ?X29, ?X30, ?SP].

return_value() -> ?RETURN_VALUE.

temp1() -> ?TEMP1.
temp2() -> ?TEMP2.
temp3() -> ?TEMP3.

heap_pointer() -> ?HEAP_POINTER.

stack_pointer() -> ?STACK_POINTER.

proc_pointer() -> ?PROC_POINTER.

lr() -> ?LR.

allocatable_gpr() ->
  %% x26, x27, and x28 are fixed global registers.
  %% TODO? r12 may be used by the frame module for large load/store offsets.
  %% SP is reserved for C.
  [ ?X0,  ?X1,  ?X2,  ?X3,  ?X4,  ?X5,  ?X6,  ?X7,
    ?X8,  ?X9,  ?X10, ?X11, ?X12, ?X13, ?X14, ?X15,
    ?X16,  ?X17, ?X18, ?X19, ?X20, ?X21, ?X22, ?X23,
    ?X24,  ?X25,                   ?X29, ?X30    ].

nr_args() -> ?AARCH64_NR_ARG_REGS.

args(Arity) when is_integer(Arity) ->
  N = erlang:min(Arity, ?AARCH64_NR_ARG_REGS),
  args(N-1, []).

args(-1, Rest) -> Rest;
args(I, Rest) -> args(I-1, [arg(I) | Rest]).

arg(N) ->
  if N < ?AARCH64_NR_ARG_REGS ->
      case N of
	0 -> ?ARG0;
	1 -> ?ARG1;
	2 -> ?ARG2;
	3 -> ?ARG3;
	4 -> ?ARG4;
	5 -> ?ARG5;
	_ -> exit({?MODULE, arg, N})
      end;
     true ->
      exit({?MODULE, arg, N})
  end.

%% Note: the fact that allocatable_gpr() is a subset of call_clobbered() is
%% hard-coded in hipe_arm_defuse:insn_defs_all_gpr/1
call_clobbered() ->		% does the RA strip the type or not?
  [{?X0,tagged},{?X0,untagged},
   {?X1,tagged},{?X1,untagged},
   {?X2,tagged},{?X2,untagged},
   {?X3,tagged},{?X3,untagged},
   {?X4,tagged},{?X4,untagged},
   {?X5,tagged},{?X5,untagged},
   {?X6,tagged},{?X6,untagged},
   {?X7,tagged},{?X7,untagged},
   {?X8,tagged},{?X8,untagged},
   {?X9,tagged},{?X9,untagged},
   {?X10,tagged},{?X10,untagged},
   {?X11,tagged},{?X11,untagged},
   {?X12,tagged},{?X12,untagged},
   {?X13,tagged},{?X13,untagged},
   {?X14,tagged},{?X14,untagged},
   {?X15,tagged},{?X15,untagged},
   {?X16,tagged},{?X16,untagged},
   {?X17,tagged},{?X17,untagged},
   {?X18,tagged},{?X18,untagged},
   {?X19,tagged},{?X19,untagged},
   {?X20,tagged},{?X20,untagged},
   {?X21,tagged},{?X21,untagged},
   {?X22,tagged},{?X22,untagged},
   {?X23,tagged},{?X23,untagged},
   {?X24,tagged},{?X24,untagged},
   {?X25,tagged},{?X25,untagged},
   % X26 is restricted (HP)
   % X27 is restricted (NSP)
   % X28 is restricted (P)
   {?X29,tagged},{?X29,untagged},
   {?X30,tagged},{?X30,untagged}
   % SP is restricted
  ].

tailcall_clobbered() ->		% tailcall crapola needs one temp
  [{?TEMP1,tagged},{?TEMP1,untagged}
  ,{?LR,tagged},{?LR,untagged}
  ].

live_at_return() ->
  [%%{?LR,untagged},
   {?HEAP_POINTER,untagged},
   {?STACK_POINTER,untagged},
   {?PROC_POINTER,untagged}
  ].
