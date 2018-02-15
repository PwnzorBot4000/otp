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
	 live_at_return/0
	 ]).

-include("../rtl/hipe_literals.hrl").

-define(X0, 0).
-define(X29, 29). % frame pointer
-define(SP, 31). % stack pointer (restricted use)
-define(LAST_PRECOLOURED, 31). % must handle both GPR and FPR ranges

-define(RETURN_VALUE, ?X0).
-define(STACK_POINTER, ?SP).
-define(PROC_POINTER, ?X29).

%%% Must handle both GPR and FPR ranges.
first_virtual() -> ?LAST_PRECOLOURED + 1.

%%% These two tests have the same implementation, but that's
%%% not something we should cast in stone in the interface. 
is_precoloured_gpr(R) -> R =< ?LAST_PRECOLOURED.

return_value() -> ?RETURN_VALUE.

live_at_return() ->
  [%%{?LR,untagged},
   {?STACK_POINTER,untagged},
   {?PROC_POINTER,untagged}
  ].
