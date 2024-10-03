require("expansions/script/c20099999")
if fusf then return end
fusf = { }
--------------------------------------"Support function"
function fusf.CutString(_str, _cut, _from)
	if type(_str) ~= "string" then Debug.Message("Invalid _str in CutString <- ".._from) end
	if type(_cut) ~= "string" or #_cut == 0 then Debug.Message("Invalid _cut in CutString <- ".._from) end
	local str, list, char = _str.._cut, {}, ""
	for unit in str:gmatch(".") do
		if unit == _cut then
			table.insert(list, char)
			char = ""
		else
			char = char..unit
		end
	end
	return list
end
function fusf.GetCardTable(_c)
	local cs, typ = { }, aux.GetValueType(_c)
	if typ == "Effect" then
		cs[1] = _c:GetHandler()
	elseif typ == "Card" then
		cs[1] = _c
	elseif typ == "Group" then
		for c in aux.Next(_c) do
			cs[#cs+1] = c
		end
	end
	return cs
end
function fusf.IsNil(...)
	local vals = {...}
	if #vals == 0 then return true end
	vals = #vals == 1 and vals[1] or vals
	if type(vals) == "string" then
		return vals == ""  -- 非空字串
	elseif type(vals) == "table" then
		return not next(vals)  -- 表有內容
	end
	return not vals
end
function fusf.NotNil(...)
	return not fusf.IsNil(...)
end
function fusf.Get_Constant(_constable, _vals)
	-- string chk
	if type(_vals) ~= "string" then return _vals end
	local _res, _first = 0
	-- cod chk
	if _constable == "cod" then 
		-- EVENT_CUSTOM
		if _vals:match("CUS") then
			_vals = _vals:sub(5)
			-- owner code or number
			_res = EVENT_CUSTOM + fusf.M_chk(_vals)
		-- EVENT_PHASE or EVENT_PHASE_START
		elseif _vals:match("PH") then
			for _,_var in ipairs(fusf.CutString(_vals, "+", "Get_Constant_1")) do
				local _constable = _var:match("PH") and "cod" or "pha"
				_res = _res + fucs[_constable][_var]
			end
		end
		if _res ~= 0 then return _res end
	end
	-- find _constable
	for i,_val in ipairs(fusf.CutString(_vals, "+", "Get_Constant_2")) do
		if i == 1 then _first = _val end
		_res = _res + (fusf.NotNil(_val) and fucs[_constable][_val:upper()] or 0)
	end
	return _res, _first
end
function fusf.Get_Loc(_loc1, _loc2, _from)
	-- nil chk
	if not fusf.NotNil(_loc1, _loc2) then 
		Debug.Message(_from..", Get_Loc")
		return nil
	end
	local _locs = {0, 0}
	-- _loc2 chk
	if _loc2 then _locs[2] = _loc2 end
	-- _loc1 string chk
	if type(_loc1) ~= "string" then 
		_locs[1] = _loc1 
		return table.unpack(_locs)
	end
	-- _loc1 is string and find fucs.ran
	local _res = 0
	for i,_loc in ipairs(fusf.CutString(_loc1, "+", "Get_Loc")) do
		for j = 1,#_loc do
			_locs[i] = _locs[i] + fucs.ran[_loc:sub(j,j):upper()]
		end
	end
	return table.unpack(_locs)
end
function fusf.M_chk(_val)
	if _val < 19999999 then return _val + 20000000 end
	return _val
end
function fusf.PostFix_Trans(_str, ...)
	local vals, res, temp, i = {...}, { }, { }, 1
	while i <= #_str do
		local ch = _str:sub(i, i)
		if ch:match("%a") then
			_, i, ch = _str:find("^([%a]+)", i)
			table.insert(res, ch)
		elseif ch == "%" then
			_, i, ch = _str:find("^([%d]+)", i)
			ch = vals[tonumber(ch)]
			if type(ch) == "boolean" then
				local b = ch
				ch = function() return b end
			end
			table.insert(res, ch)
		elseif ch == "(" or ch == "~" then
			table.insert(temp, ch)
		elseif ch == ")" then
			while #temp > 0 and temp[#temp] ~= "(" do
				table.insert(res, table.remove(temp))
			end
			table.remove(temp)
		elseif ch == "+" or ch == "-" then
			while #temp > 0 and temp[#temp] ~= "(" do
				table.insert(res, table.remove(temp))
			end
			table.insert(temp, ch)
		elseif ch == "/" then
			while #temp > 0 and temp[#temp] == "/" do
				table.insert(res, table.remove(temp))
			end
			table.insert(temp, ch)
		end
		if temp[#temp] == "~" and ch:match("^[%a%)%%]") then
			table.insert(res, table.remove(temp))
		end
		i = i + 1
	end
	while #temp > 0 do
		table.insert(res, table.remove(temp))
	end
	return res
end
function fusf.IsN(_func)
	return function(_c, _val, _exval)
		if type(_val) == "string" and _val:match("[%+%-]") then
			local _, _, val = _str:find("([%d]+)")
			local Cal = {
				["+"] = Card[_func](_c, _exval) >= math.abs(tonumber(val)),
				["-"] = Card[_func](_c, _exval) <= math.abs(tonumber(val))
			}
			return Cal[_val:match("[%+%-]")]
		end
		_val = tonumber(_val)
		if _val > 0 then return Card[_func](_c, _exval) == tonumber(_val) end
		return Card[_func](_c, _exval) <= tonumber(_val) -- _val = -n
	end
end
function fusf.Is_Cons(_func, _key)
	return function(c, _cons)
		if type(_cons) ~= "string" then return Card[_func](c) & _cons == _cons end
		local res, valL, valR = { }
		for _, val in ipairs(fusf.PostFix_Trans(_cons)) do
			if val:match("[%-%~]") then
				res[#res] = not res[#res]
			elseif val:match("[%+%/]") then
				valR = table.remove(res)
				valL = table.remove(res)
				local Cal = {
					["+"] = valL and valR,
					["/"] = valL or valR
				}
				table.insert(res, Cal[val])
			else
				_cons = fucs[_key][val:upper()]
				table.insert(res, Card[_func](c) & _cons == _cons)
			end
		end
		return res[#res]
	end
end
function fusf.Get_Func(_c, _func, ...)
	if type(_func) ~= "string" then return _func end
	local vals = {...}
	local lib = _c.lib or {}
	local cm = _G["c".._c:GetOriginalCode()]
	local loc = _func:find("%(")
	-- find cm, lib, fuef, aux
	if not loc then 
		local func = cm[_func] or lib[_func] or fuef[_func] or aux[_func]
		if #vals > 0 then return func(table.unpack(vals)) end
		return func
	end
	-- _func = "func(a,,b)" -> "func", {"a", nil, "b"}
	local func = _func:sub(1, loc - 1)
	local f_vals = fusf.CutString(_func:sub(loc + 1, #_func - 1), ",", "Get_Func")
	-- translate vals 
	for i, f_val in ipairs(f_vals) do
		if tonumber(f_val) then
			f_vals[i] = tonumber(f_val) 
		elseif f_val:match("%%") then
			f_vals[i] = vals[tonumber(f_val:sub(2))]
		end
	end
	for _, Lib in ipairs({cm, lib, fuef, aux}) do
		if Lib[func] then return Lib[func](table.unpack(f_vals)) end
	end
	return nil
end
function fusf.Val_Cuts(_val, ...) -- "f1,f2(v1,v2),(v3)" -> {"f1", {"f2", "v1", "v2"}, {"v3"}}, ... use in vi = %i
	local res, res_ind, temp = { }, 0, { }
	--local place, f_chk, f_temp, sets = 1, 0, "", { }
	for _, val in ipairs(fusf.CutString(_val, ",", "Val_Cuts_1")) do
		res_ind = res_ind + 1
		local is_st = val:match("%(")
		local is_ed = val:match("%)")
		-- is f(v1)
		if is_st and is_ed then -- f(v1) -> {"f", "v1"}
			res[res_ind] = fusf.Val_Cuts_Table_Process(val, ...)
		elseif is_st then -- f(v1,v2,v3) st f(v1
			temp, res_ind = val, res_ind - 1
		elseif is_ed then -- f(v1,v2,v3) ed v3) -> {"f", "v1", "v2", "v3"}
			res[res_ind] = fusf.Val_Cuts_Table_Process(temp..","..val, ...)
			temp = ""
		elseif #temp > 0 then -- f(v1,v2,v3) mid v2
			temp, res_ind = temp..","..val, res_ind - 1
		elseif val ~= "" then
			res[res_ind] = val
		end
	end
	res.len = res_ind
	return res
end
function fusf.Val_Cuts_Table_Process(_str, ...) -- "f(%1,,3)" -> {"f", vals[1], nil, "3"}
	local vals, res, st = {...}, { }, _str:find("%(")
	if st ~= 1 then res[1] = _str:sub(1, st - 1) end -- has f
	if st + 1 == #_str then return res end -- "f()" -> {"f"}
	_str = _str:sub(st + 1, #_str - 1)
	local res_ind = #res
	for _, val in ipairs(fusf.CutString(_str, ",", "Val_Cuts_Table_Process")) do
		res_ind = res_ind + 1
		if val:match("%%") then
			res[res_ind] = vals[tonumber(val:sub(2))]
		elseif val ~= "" then
			res[res_ind] = val
		end
	end
	res.len = res_ind
	return res
end
function fusf.ForTable(t, n)
	local i, max = 0, t.len or n
	return function()
		if i >= max then return nil end
		i = i + 1
		return i, t[i]
	end
end
--------------------------------------"Other Support function"
function fusf.GetFlag(val, cod, n1, n2)
	local typ, count = aux.GetValueType(val)
	if type(cod) == "string" then cod = tonumber(cod) end
	if cod < 19999999 then cod = cod + 20000000 end
	if typ == "Card" then count = val:GetFlagEffect(cod) end
	if typ == "Effect" then count = val:GetHandler():GetFlagEffect(cod) end
	if typ == "int" then count = Duel.GetFlagEffect(val, cod) end
	if not n1 then return n2 and (count == n2) or count end
	if type(n1) == "string" and n1:match("[%+%-]") then
		local Cal = {
			["+"] = count >= (n2 or math.abs(tonumber(n1))),
			["-"] = count <= (n2 or math.abs(tonumber(n1)))
		}
		return Cal[n1:match("[%+%-]")]
	end
end