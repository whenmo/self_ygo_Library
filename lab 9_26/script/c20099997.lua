require("expansions/script/c20099998")
if fucf then return end
fucf, fugf = { }, { }
fugf.__index = fugf 
-------------------------------------- Group function
function fugf.Get(_tp, _loc)
	return Duel.GetFieldGroup(_tp, fusf.Get_Loc(_loc))
end
function fugf.Filter(_g, _func, _val, _n)
	-- prepare
	if type(_val) ~= "table" then _val = { _val } end
	if type(_func) == "function" then 
		_g = _g:Filter(_func, nil, table.unpack(_val))
		if _n then return #_g >= _n end
		return _g
	end
	if type(_func) == "string" then _func = fusf.PostFix_Trans(_func, _val) end
	-- trans _val (
	local temp_val, ex_val, place = { }, { }, 1
	while #_val > 0 do
		local f_val = table.remove(_val, 1)
		if type(f_val) == "string" then
			for _, val in ipairs(fusf.CutString(f_val, ",", "Value_Trans")) do
				if val == "" then
					val = nil
				elseif val:match("%%") then
					local n = tonumber(val:sub(2, #val))
					if not ex_val[n] then ex_val[n] = table.remove(_val, 1) end
					val = ex_val[n]
				end
				temp_val[place] = val
				place = place + 1
			end
		else
			temp_val[place] = f_val
			place = place + 1
		end
	end
	_val, place, temp_val, ex_val = temp_val, 1
	-- Filter
	if #_func == 1 then
		if type(_func[1]) == "string" then 
			_func[1] = fucf[_func[1] ] or Card[_func[1] ] or aux[_func[1] ]
		end
		_g = _g:Filter(_func[1], nil, table.unpack(_val))
	else
		local temp_g, valL, valR = { }
		for _, func in ipairs(_func) do
			if func == "~" then
				temp_g[#temp_g] = _g - temp_g[#temp_g]
			elseif type(func) == "string" and #func == 1 then
				valR = table.remove(temp_g)
				valL = table.remove(temp_g)
				local Cal = {
					["+"] = valL & valR,
					["-"] = valL - valR,
					["/"] = valL + valR 
				}
				table.insert(temp_g, Cal[func])
			else
				if type(func) == "string" then 
					func = fucf[func] or Card[func] or aux[func]
				end
				local val = _val[place]
				place = place + 1
				if type(val) ~= "table" then val = { val } end
				table.insert(temp_g, _g:Filter(func, nil, table.unpack(val)))
			end
		end
		_g = table.remove(temp_g)
	end
	if _n then return #_g >= _n end
	return _g
end
function fugf.GetFilter(_tp, _loc, _func, _val, _n)
	return fugf.Filter(fugf.Get(_tp, _loc), _func, _val, _n)
end
function fugf.SelectFilter(_tp, _loc, _func, _val, _min, _max, _sp, _c)
	return fugf.GetFilter(_tp, _loc, _func, _val):Select(sp or _tp, _min or 1, _max or _min or 1, _c)
end
function fugf.SelectTg(_tp, _loc, _func, _val, _min, _max, _sp, _c)
	local g = fugf.SelectFilter(_tp, _loc, _func, _val, _min, _max, _sp, _c)
	Duel.SetTargetCard(g)
	return g
end
--------------------------------------"Card function"
function fucf.Filter(c, func, ...)
	return #fugf.Exist(Group.FromCards(c), func, {...})>0
end
fucf.IsRk   = fusf.IsN("GetRank")
fucf.IsLv   = fusf.IsN("GetLevel")
fucf.IsRLv  = fusf.IsN("GetRitualLevel")
fucf.IsLk   = fusf.IsN("GetLink")
fucf.IsAtk  = fusf.IsN("GetAttack")
fucf.IsDef  = fusf.IsN("GetDefense")
fucf.IsSeq  = fusf.IsN("GetSequence")
fucf.IsPSeq = fusf.IsN("GetPreviousSequence")
function fucf.Not(c,val)
	if aux.GetValueType(val) == "Card" then
		return c ~= val
	elseif aux.GetValueType(val) == "Effect" then
		return c ~= val:GetHandler()
	elseif aux.GetValueType(val) == "Group" then
		return not val:IsContains(c)
	elseif aux.GetValueType(val) == "function" then
		return not val(c)
	end
	return false
end
function fucf.IsSet(c,set)
	if type(set) == "number" then return c:IsSetCard(set) end
	for _,Set in ipairs(fusf.CutString(set, "/")) do
		Set=tonumber(Set,16)
		if Set and c:IsSetCard(Set) then return true end
	end
	return false
end
function fucf.AbleTo(c,loc)
	local func = {
		["H"] = "Hand"   ,
		["D"] = "Deck"   ,
		["G"] = "Grave"  ,
		["R"] = "Remove",
		["E"] = "Extra"  ,
	}
	local iscos = string.sub(loc,1,1) == "*"
	if iscos then loc = string.sub(loc,2) end
	return Card["IsAbleTo"..func[loc]..(iscos and "AsCost" or "")](c)
end
function fucf.CanSp(c, e, typ, tp, nochk, nolimit, pos, totp, zone)
	if not tp then tp = e:GetHandlerPlayer() end
	return c:IsCanBeSpecialSummoned(e, typ, tp, nochk or false, nolimit or false, pos or POS_FACEUP, totp or tp,zone or 0xff)
end
function fucf.IsCode(c, _cod)
	local cod
	if aux.GetValueType(_cod) == "number" then
		cod = {_cod}
	elseif aux.GetValueType(_cod) == "string" then
		cod = fusf.CutString(_cod, "+")
	elseif aux.GetValueType(_cod) == "table" then
		cod = _cod
	end
	for i,v in ipairs(cod) do
		cod[i] = fusf.M_chk(tonumber(v))
	end
	return c:IsCode(table.unpack(cod))
end
function fucf.AddCode(c, ...)
	local codes = { }
	for _, _code in ipairs({...}) do
		if type(_code) == "string" then 
			for _, cod in ipairs(fusf.CutString(_code, ",", "AddCode")) do
				codes[#codes + 1] = fusf.M_chk(cod)
			end
		else
			codes[#codes + 1] = fusf.M_chk(_code)
		end
	end
	aux.AddCodeList(c, table.unpack(codes))
end
function fucf.HasCode(c, _cod)
	if not c.card_code_list then return false end
	local cod, has
	if aux.GetValueType(_cod) == "number" then
		cod = {_cod}
	elseif aux.GetValueType(_cod) == "string" then
		cod = fusf.CutString(_cod, "+")
	end
	for i,v in ipairs(cod) do
		has = has or c.card_code_list[fusf.M_chk(tonumber(v))]
	end
	return has
end
fucf.TgChk  = Card.IsCanBeEffectTarget
fucf.GChk   = function(c) return not c:IsHasEffect(EFFECT_NECRO_VALLEY) end
fucf.IsImm  = Card.IsImmuneToEffect
fucf.IsCon  = Card.IsControler
fucf.IsPCon = Card.IsPreviousControler
fucf.IsLoc  = function(c,loc) return c:IsLocation(fusf.Get_Loc(loc)) end
fucf.IsPLoc = function(c,loc) return c:IsPreviousLocation(fusf.Get_Loc(loc)) end
fucf.IsRea  = fusf.Is_Cons("GetReason", "rea")
fucf.IsTyp  = fusf.Is_Cons("GetType", "typ")
fucf.IsSTyp = fusf.Is_Cons("GetSummonType", "styp")
fucf.IsOTyp = fusf.Is_Cons("GetOriginalType", "typ")
fucf.IsAtt  = fusf.Is_Cons("GetAttribute", "att")
fucf.IsRac  = fusf.Is_Cons("GetRace", "rac")
fucf.IsPos  = fusf.Is_Cons("GetPosition", "pos")
fucf.IsPPos = fusf.Is_Cons("GetPreviousPosition", "pos")