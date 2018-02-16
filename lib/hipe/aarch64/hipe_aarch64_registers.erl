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
	 return_value/0,
	 lr/0,
	 args/1,
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
-define(X25, 25). % TEMP_LR callee-save
-define(X26, 26). % heap pointer (HP)
-define(X27, 27). % native stack pointer (NSP)
-define(X28, 28). % proc pointer (P)
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

-define(RETURN_VALUE, ?X0).
-define(HEAP_POINTER, ?X26).
-define(STACK_POINTER, ?X27).
-define(PROC_POINTER, ?X28).

%%% Must handle both GPR and FPR ranges.
first_virtual() -> ?LAST_PRECOLOURED + 1.

%%% These two tests have the same implementation, but that's
%%% not something we should cast in stone in the interface. 
is_precoloured_gpr(R) -> R =< ?LAST_PRECOLOURED.

return_value() -> ?RETURN_VALUE.

lr() -> ?LR.

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
