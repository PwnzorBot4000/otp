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

-module(hipe_aarch64_defuse).
-export([insn_def_gpr/1, insn_use_gpr/1]).
-include("hipe_aarch64.hrl").

%%%
%%% Defs and uses for general-purpose (integer) registers only.
%%%
insn_def_gpr(I) ->
  case I of
    #move{dst=Dst} -> [Dst]
  end.

insn_use_gpr(I) ->
  case I of
    #move{am1=Am1} -> am1_use(Am1, [])
  end.

am1_use(Am1, Set) ->
  case Am1 of
    {_,_} -> Set
  end.
