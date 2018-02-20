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

-module(hipe_aarch64_ra_finalise).
-export([finalise/3]).
-include("hipe_aarch64.hrl").

finalise(CFG, TempMap, _FPMap0=[]) ->
  {_, SpillLimit} = hipe_gensym:var_range(aarch64),
  Map = mk_ra_map(TempMap, SpillLimit),
  hipe_aarch64_cfg:map_bbs(fun(_Lbl, BB) -> ra_bb(BB, Map) end, CFG).

ra_bb(BB, Map) ->
  hipe_bb:code_update(BB, ra_code(hipe_bb:code(BB), Map, [])).

ra_code([I|Insns], Map, Accum) ->
  ra_code(Insns, Map, ra_insn(I, Map, Accum));
ra_code([], _Map, Accum) ->
  lists:reverse(Accum).

ra_insn(I, Map, Accum) ->
  case I of
    #pseudo_move{} -> ra_pseudo_move(I, Map, Accum);
    _ -> [ra_insn_1(I, Map) | Accum]
  end.

ra_insn_1(I, Map) ->
  case I of
    #move{} -> ra_move(I, Map);
    #pseudo_li{} -> ra_pseudo_li(I, Map);
    #pseudo_tailcall{} -> ra_pseudo_tailcall(I, Map);
    #pseudo_blr{} -> I;
    #pseudo_tailcall_prepare{} -> I
    % _ -> I % temporarily adding all default cases explicitly.
  end.

ra_move(I=#move{dst=Dst,am1=Am1}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewAm1 = ra_am1(Am1, Map),
  I#move{dst=NewDst,am1=NewAm1}.

ra_pseudo_li(I=#pseudo_li{dst=Dst}, Map) ->
  NewDst = ra_temp(Dst, Map),
  I#pseudo_li{dst=NewDst}.

ra_pseudo_tailcall(I=#pseudo_tailcall{funv=FunV,stkargs=StkArgs}, Map) ->
  NewFunV = ra_funv(FunV, Map),
  NewStkArgs = ra_args(StkArgs, Map),
  I#pseudo_tailcall{funv=NewFunV,stkargs=NewStkArgs}.

ra_pseudo_move(I=#pseudo_move{dst=Dst,src=Src}, Map, Accum) ->
  NewDst = ra_temp(Dst, Map),
  NewSrc = ra_temp(Src, Map),
  case NewSrc#aarch64_temp.reg =:= NewDst#aarch64_temp.reg of
    true -> Accum;
    false -> [I#pseudo_move{dst=NewDst,src=NewSrc} | Accum]
  end.

%%% Tailcall stack arguments.

ra_args([Arg|Args], Map) ->
  [ra_temp_or_imm(Arg, Map) | ra_args(Args, Map)];
ra_args([], _) ->
  [].

ra_temp_or_imm(Arg, Map) ->
  case hipe_aarch64:is_temp(Arg) of
    true ->
      ra_temp(Arg, Map);
    false ->
      Arg
  end.

%%% FunV, Am, and Temp operands.

ra_funv(FunV, Map) ->
  case FunV of
    #aarch64_temp{} -> ra_temp(FunV, Map);
    _ -> FunV
  end.

ra_am1(Am1, _) ->
  case Am1 of
    {_,_} ->
      Am1
  end.

ra_temp(Temp, Map) ->
  Reg = hipe_aarch64:temp_reg(Temp),
  case hipe_aarch64:temp_type(Temp) of
    'double' ->
      exit({?MODULE,ra_temp,Temp});
    _ ->
      case hipe_aarch64_registers:is_precoloured_gpr(Reg) of
	true -> Temp;
	_ -> ra_temp_common(Reg, Temp, Map)
      end
  end.

ra_temp_common(Reg, Temp, Map) ->
  case gb_trees:lookup(Reg, Map) of
    {value,NewReg} -> Temp#aarch64_temp{reg=NewReg};
    _ -> Temp
  end.

mk_ra_map(TempMap, SpillLimit) ->
  %% Build a partial map from pseudo to reg or spill.
  %% Spills are represented as pseudos with indices above SpillLimit.
  %% (I'd prefer to use negative indices, but that breaks
  %% hipe_aarch64_registers:is_precoloured/1.)
  %% The frame mapping proper is unchanged, since spills look just like
  %% ordinary (un-allocated) pseudos.
  lists:foldl(fun(MapLet, Map) ->
		  {Key,Val} = conv_ra_maplet(MapLet, SpillLimit, is_precoloured_gpr),
		  gb_trees:insert(Key, Val, Map)
	      end,
	      gb_trees:empty(),
	      TempMap).

conv_ra_maplet(MapLet = {From,To}, SpillLimit, IsPrecoloured) ->
  %% From should be a pseudo, or a hard reg mapped to itself.
  if is_integer(From), From =< SpillLimit ->
      case hipe_aarch64_registers:IsPrecoloured(From) of
	false -> [];
	_ ->
	  case To of
	    {reg, From} -> [];
	    _ -> exit({?MODULE,conv_ra_maplet,MapLet})
	  end
      end;
     true -> exit({?MODULE,conv_ra_maplet,MapLet})
  end,
  %% end of From check
  case To of
    {reg, NewReg} ->
      %% NewReg should be a hard reg, or a pseudo mapped
      %% to itself (formals are handled this way).
      if is_integer(NewReg) ->
	  case hipe_aarch64_registers:IsPrecoloured(NewReg) of
	    true -> [];
	    _ -> if From =:= NewReg -> [];
		    true ->
		     exit({?MODULE,conv_ra_maplet,MapLet})
		 end
	  end;
	 true -> exit({?MODULE,conv_ra_maplet,MapLet})
      end,
      %% end of NewReg check
      {From, NewReg};
    {spill, SpillIndex} ->
      %% SpillIndex should be >= 0.
      if is_integer(SpillIndex), SpillIndex >= 0 -> [];
	 true -> exit({?MODULE,conv_ra_maplet,MapLet})
      end,
      %% end of SpillIndex check
      ToTempNum = SpillLimit+SpillIndex+1,
      MaxTempNum = hipe_gensym:get_var(aarch64),
      if MaxTempNum >= ToTempNum -> ok;
	 true -> hipe_gensym:set_var(aarch64, ToTempNum)
      end,
      {From, ToTempNum};
    _ -> exit({?MODULE,conv_ra_maplet,MapLet})
  end.

