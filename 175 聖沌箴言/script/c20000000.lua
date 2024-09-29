require("expansions/script/c20099997")
if fuef then return end
fuef = { } --2024/9/26
fuef.__index = fuef 
fuef.DebugMode = false
---------------------------------------------------------------- Standard Register
-- no cod 
for _,typ in ipairs(fusf.CutString("I,F+G", ",", "typ_reg_1")) do
	local name = typ:gsub("+", "")
	fuef[name] = function(_owner, _handler, _ignore)
		return fuef:Creat(typ, nil, _owner, _handler, _ignore)
	end
end
-- need cod
for _,typ in ipairs(fusf.CutString("S,F,E,S+C,F+C,E+C,F+TO,F+TF,S+TO,S+TF,X", ",", "typ_reg_2")) do
	local name = typ:gsub("+", "")
	fuef[name] = function(_owner, _cod, _handler, _ignore)
		if type(_owner) ~= "userdata" then _cod, _owner = _owner end -- _owner is cod (no owner
		return fuef:Creat(typ, _cod, _owner, _handler, _ignore)
	end
end
-- cod = cod or FC
for _,typ in ipairs(fusf.CutString("A,QO,QF", ",", "typ_reg_3")) do
	local name = typ:gsub("+", "")
	fuef[name] = function(_owner, _cod, _handler, _ignore)
		if type(_owner) ~= "userdata" then _cod, _owner = _owner end -- _owner is cod (no owner
		return fuef:Creat(typ, _cod or "FC", _owner, _handler, _ignore)
	end
end
---------------------------------------------------------------- fuef()
function fuef:__call(_cod, _handler, _ignore)
	-- _cod is owner (Creat and Register Noc
	if type(_cod) == "userdata" then
		local ori = self
		while ori.pre do
			ori = ori.pre
		end
		repeat
			self = fuef:Creat(ori.typ, ori.cod, _cod, _handler, _ignore)
			for _,_key in ipairs(fusf.CutString("des,cat,pro,ran,tran,res,lab,obj", ",", "__call_1")) do
				self[_key] = ori[_key] or nil
			end
			self:RCreat():SetKey("call Reg Noc"):Reg()
			if ori.ctl then self:CTL(table.unpack(ori.ctl)) end
			if ori.val then self:VAL(table.unpack(ori.val)) end
			if ori.con then self:CON(table.unpack(ori.con)) end
			if ori.cos then self:COS(table.unpack(ori.cos)) end
			if ori.tg then self:TG(table.unpack(ori.tg)) end
			if ori.op then self:OP(table.unpack(ori.op)) end
			local chk = ori.aft
			if chk then ori = ori.aft end
		until not chk
		return self
	end
	-- Creat and Clone all key
	local CE = setmetatable({ }, fuef)
	for _,_key in ipairs(fusf.CutString("typ,cod,des,cat,pro,ran,tran,ctl,val,con,cos,tg,op,res,lab,obj,handler", ",", "__call_2")) do
		CE[_key] = self[_key] or nil
	end
	if _cod then CE.cod = fusf.Get_Constant("cod", _cod) end
	-- Clone and return Clone
	if self.e then
		CE.e = self.e:Clone()
		-- Set Key and Register Effect
		return CE:SetKey("call Reg"):Reg(_handler, _ignore)
	end
	-- Clone Noc (
	CE.pre = self
	self.aft = CE
	return CE
end
--------------------------------------------------------------------------meta methods
-- fuef - : subtraction Key or Key detial
function fuef:__sub(_val)
	for _,key in ipairs(fusf.CutString(_val, ",", "__sub_1")) do
		-- subtraction Key
		if not key:match(":") then
			self[key:lower()] = nil
		-- subtraction Key detial (use in typ, cod, cat, pro
		else
			key = fusf.CutString(key, ":", "__sub_2")
			local Keytype = key[1]:lower()  -- typ, cod, cat, pro
			key = fusf.Get_Constant(Keytype == "typ" and "etyp" or Keytype, key[2])
			self[Keytype] = self[Keytype] - key
		end
	end
	-- Reload Effect
	return self:RCreat():SetKey("sub"):Reg()
end
function fuef:Sub(_val)
	return self - _val
end
--------------------------------------------------------------------------
-- Creat and Register Effect
function fuef:Creat(_typ, _cod, _owner, _handler, _ignore)
	local E = setmetatable({ }, fuef)
	-- Set type and code
	E.typ = fusf.Get_Constant("etyp", _typ)
	E.cod = fusf.Get_Constant("cod", _cod)
	-- use in no owner (Creat and Register in __call
	if not _owner then return E end
	-- Create Effect
	E.e = Effect.CreateEffect(fusf.GetCardTable(_owner)[1])
	-- Set type and code
	if _typ then E.e:SetType(E.typ) end
	if _cod then E.e:SetCode(E.cod) end
	-- Register Effect
	return E:Reg(_handler, _ignore)
end
-- Register Effect
function fuef:Reg(_handler, _ignore)
	local handler = self.handler or self.e:GetOwner()
	-- is force Register (use in Card.RegisterEffect
	local ignore = _ignore
	if _handler then 
		if _handler == true then		-- handler equal owner and ignore == true
			ignore = true
		elseif _handler == false then   -- not Reg (use in FG
			handler = nil
			return self
		else							-- handler not equal owner and handler is Card, Group or tp
			handler = _handler
		end
	end
	-- cover handler
	self.handler = handler
	-- Handler is player
	if type(handler) == "number" then
		Duel.RegisterEffect(self.e, handler)
	-- Handler is c or g 
	else
		-- get handler table
		handler = fusf.GetCardTable(handler)
		-- check and cover ignore
		if not type(ignore) == "boolean" then ignore = self.ignore or false end -- not input _ignore
		self.ignore = ignore
		-- Register First Card
		local fc = table.remove(handler)
		fc:RegisterEffect(self.e, ignore)
		-- Handler is group
		if #handler > 0 then
			-- Register other card
			self.gclo = { }
			for i = 1,#handler do
				local E = self.e:Clone()
				self.gclo[#self.gclo + 1] = E
				handler[i]:RegisterEffect(E, ignore)
			end
		end
	end
	return self
end
-- Reset and Creat Effect
function fuef:RCreat()
	-- chk self.e is effect
	if not aux.GetValueType(self.e) == "Effect" then return self end
	-- Reset self
	local _owner = self.e:GetOwner()
	self.e:Reset()
	self.e = Effect.CreateEffect(_owner)
	-- Reset if handler is group
	if self.gclo then 
		for _, gcloe in ipairs(self.gclo) do
			gcloe:Reset()
		end
		self.gclo = nil
	end
	return self
end
-- Set all Key
function fuef:SetKey(_from)
	if fuef.DebugMode then Debug.Message("SetKey <- ".._from) end
	if not self.e then return self end
	if self.typ then self.e:SetType(self.typ) end
	if self.cod then self.e:SetCode(self.cod) end
	if self.des then self.e:SetDescription(self.des) end
	if self.cat then self.e:SetCategory(self.cat) end
	if self.pro then self.e:SetProperty(self.pro) end
	if self.ran then self.e:SetRange(self.ran) end
	if self.tran then self.e:SetTargetRange(table.unpack(self.tran)) end
	if self.ctl then self.e:SetCountLimit(table.unpack(self.ctl)) end
	if self.val then self.e:SetValue(self.val) end
	if self.con then self.e:SetCondition(self.con) end
	if self.cos then self.e:SetCost(self.cos) end
	if self.tg then self.e:SetTarget(self.tg) end
	if self.op then self.e:SetOperation(self.op) end
	if self.res then self.e:SetReset(table.unpack(self.res)) end
	if self.lab then self.e:SetLabel(table.unpack(self.lab)) end
	if self.obj then self.e:SetLabelObject(self.obj) end
	return self
end
-- chk is Noc (use in every set key final return without CTL and Func_Model
function fuef:Reload(_from)
	if not self.e then return self end
	return self:RCreat():SetKey("Reload <- ".._from):Reg()
end
function fuef.NotNil(from, ...)
	local res = fusf.NotNil(...)
	if not res and fuef.DebugMode then Debug.Message("NotNil is nil <- "..from) end
	return res
end
----------------------------------------------------------------DES
function fuef:DES(_code, _id)
	if not fuef.NotNil("DES", _code, _id) then return self end  -- nil chk
	if _id then
		if type(_code) == "number" then
			_code = fusf.M_chk(_code)
		else	-- ("-1", 0)
			_code = self.e:GetOwner():GetOriginalCode() + tonumber(_code)
		end
	elseif type(_code) == "number" then
		if _code < 17 then  -- in cdb and code is owner card code
			_code, _id = self.e:GetOwner():GetOriginalCode(), _code
		else
			_code, _id = fusf.M_chk(_code), 0
		end
	elseif type(_code) == "string" then
		if tonumber(_code) then -- code = m +- _code
			_code, _id = self.e:GetOwner():GetOriginalCode() + tonumber(_code), 0
		else	-- in fucs.des
			_code, _id = 0,  fucs.des[_code]
		end
	end
	self.des = aux.Stringid(_code, _id)  -- _code*16 + _id
	return self:Reload("DES")
end
----------------------------------------------------------------TYP, COD, CAT and PRO
function fuef:Cons_Model(_key, _val)
	if not fuef.NotNil(_key:upper(), _val) then return self end  -- nil chk
	local _keytype = _key == "typ" and "etyp" or _key
	local val, des = fusf.Get_Constant(_keytype, _val)
	self[_key] = val
	if _key == "cod" and not self.des and fucs.des[des] then self.des = fucs.des[des] end
	return self:Reload("Cons_Model")
end
function fuef:TYP(_val)
	return self:Cons_Model("typ", _val)
end
function fuef:COD(_val)
	return self:Cons_Model("cod", _val)
end
function fuef:CAT(_val)
	return self:Cons_Model("cat", _val)
end
function fuef:PRO(_val)
	return self:Cons_Model("pro", _val)
end
----------------------------------------------------------------RAN and TRAN
function fuef:RAN(_val)
	if not fuef.NotNil("RAN", _val) then return self end  -- nil chk
	self.ran = fusf.Get_Loc(_val, nil, "fuef:RAN()")
	return self:Reload("RAN")
end
function fuef:TRAN(_val1, _val2)
	if not fuef.NotNil("TRAN", _val1, _val2) then return self end  -- nil chk
	self.tran = {fusf.Get_Loc(_val1, _val2, "fuef:TRAN()")}
	return self:Reload("TRAN")
end
----------------------------------------------------------------CTL
function fuef:CTL(_count, _code, _pro) --count, code, pro
	if not fuef.NotNil("CTL", _count, _code) then return self end  -- nil chk
	if not self.e then  --is Noc
		self.ctl = {_count, _code}
		return self 
	end
	local ctl = { _count } -- SetCountLimit
	-- (1, n), (1, "D"), (1, "n+D")
	if _code then
		if type(_code) == "string" then
			local _temp_code = 0
			for _,_val in ipairs(fusf.CutString(_code, "+", "CTL_1")) do
				if _val:match("m") then
					_temp_code = _temp_code + self.e:GetOwner():GetOriginalCode()
				elseif _val:match("[OD]") then
					_temp_code = _temp_code + self.e:GetOwner():GetOriginalCode() + fucs.ctl[_val]
				elseif _val:match("C") then
					_temp_code = _temp_code + EFFECT_COUNT_CODE_CHAIN 
				else -- is number
					_temp_code = _temp_code + fusf.M_chk(tonumber(_val))
				end
			end
			_code = _temp_code
		end
		ctl[2] = _code + (_pro and fucs.ctl[_val] or 0)
	-- (1), (m)
	elseif type(_count) == "number" then
		-- _count is code
		if _count > 99 then ctl = {1, fusf.M_chk(_count)} end
	-- ("n+D"), ("D")
	else
		_code = 0
		for _,_val in ipairs(fusf.CutString(_count, "+", "CTL_2")) do
			if _val:match("m") then
				_code = _code + self.e:GetOwner():GetOriginalCode()
			elseif _val:match("[OD]") then
				_code = _code + self.e:GetOwner():GetOriginalCode() + fucs.ctl[_val]
			elseif _val:match("C") then
				_code = _code + EFFECT_COUNT_CODE_CHAIN 
			else -- is number
				_code = _code + fusf.M_chk(tonumber(_val))
			end
		end
		ctl = {1, _code}
	end
	self.ctl = ctl
	return self:RCreat():SetKey("CTL"):Reg()
end
----------------------------------------------------------------VAL, CON, COS, TG and OP
function fuef:Func(_val, _func, ...)
	-- func = ("val,con,cos(v1,v2),tg,op") or ("con(v1,v2),op") or (val, "con,op(v1, v2)"), if v = %1~n then { ... } is value table
	if not fuef.NotNil("Func", _val, _func) then return self end  -- nil chk
	local vals = {...}
	if not (type(_val) == "string" and _val:match("%,")) then -- check _val is val
		local val = { _val }
		if type(_val) == "string" and _val:match("%%") then val = { _val , ... } end
		self.val = val 
	else	--  _val is _func
		vals, _func = {_func, ...}, _val
	end
	-- _val is func (func place, f(v1,v2,v3) chk and temp and set in Func
	local place, f_chk, f_temp, sets = 1, 0, "", { }
	for i, func in ipairs(fusf.CutString(_func, ",", "Func")) do
		local is_st = func:match("%(")
		local is_ed = func:match("%)")
		-- is f(v1)
		if is_st and is_ed then
			place = self:Set_Func_Val(i - f_chk, place, func, vals)
			sets[#sets + 1] = ({"val", "con", "cos", "tg", "op"})[place]
		-- is f(v1,v2,v3) st f(v1
		elseif is_st then
			f_temp, f_chk = func, f_chk + 1
		-- is f(v1,v2,v3) ed v3)
		elseif is_ed then
			place = self:Set_Func_Val(i - f_chk, place, f_temp..","..func, vals)
			sets[#sets + 1] = ({"val", "con", "cos", "tg", "op"})[place]
			f_temp = ""
		-- is f(v1,v2,v3) mid v2 
		elseif #f_temp > 0 then
			f_temp, f_chk = f_temp..","..func, f_chk + 1
		-- is normal func
		elseif fusf.NotNil(func) then
			place = self:Set_Func_Val(i - f_chk, place, func, vals)
			sets[#sets + 1] = ({"val", "con", "cos", "tg", "op"})[place]
		end
	end
	if not self.e then return self end
	-- self[key] = { func, ... }
	if sets[1] == "val" then
		table.remove(sets, 1)
		local val = self.val[1]
		if type(val) == "string" then
			self.val = tonumber(val) or fucs.val[val] or fusf.Get_Func(self.e:GetOwner(), table.unpack(self.val))
			if not self.val and fuef.DebugMode then Debug.Message("val Func value is nil") end
		else	-- number or function
			self.val = val
		end
	end
	for _,set in ipairs(sets) do
		local res = fusf.Get_Func(self.e:GetOwner(), table.unpack(self[set]))
		if not res and fuef.DebugMode then Debug.Message(set.." Func value is nil") end
		self[set] = res
	end
	return self:RCreat():SetKey("Func"):Reg()
end
function fuef:Set_Func_Val(cant_match, place, func, vals)
	-- "val,con,cos,tg,op" or "con,op" , if cant match then follow a sequence
	local seqs = {"val", "con", "cos", "tg", "op"}
	local match = 0
	for j = place, 5 do -- find can match seqs
		local temp = func:find(seqs[j])
		if temp and temp > match then
			place, match = j, temp
		end
	end
	if match == 0 then place = cant_match end -- cant match seqs
	local res = { func }
	if func:match("%%") then res = {func, table.unpack(vals)} end
	self[seqs[place] ] = res
	return place
end
function fuef:Func_Model(_key, _func, ...)
	if not fuef.NotNil(_key:upper(), _func) then return self end  -- nil chk
	if not self.e then  --is Noc
		self[_key] = {_func, ...}
		return self 
	end
	local val_chk = _key == "val" and (tonumber(_func) or fucs.val[_func]) or nil
	local vals = { ... }
	if #vals > 0 then
		_func = _func.."("
		for i = 1, #vals do
			_func = _func.."%"..i
			if i ~= #vals then _func = _func.."," end
		end
		_func = _func..")"
	end
	self[_key] = val_chk or fusf.Get_Func(self.e:GetOwner(), _func, ...)
	if not self[_key] and fuef.DebugMode then Debug.Message(_key.." Func value is nil") end
	return self:RCreat():SetKey(_key):Reg()
end
function fuef:VAL(_func, ...)
	return self:Func_Model("val", _func, ...)
end
function fuef:CON(_func, ...)
	return self:Func_Model("con", _func, ...)
end
function fuef:COS(_func, ...)
	return self:Func_Model("cos", _func, ...)
end
function fuef:TG(_func, ...)
	return self:Func_Model("tg", _func, ...)
end
function fuef:OP(_func, ...)
	return self:Func_Model("op", _func, ...)
end
----------------------------------------------------------------RES
function fuef:RES(_flag, _count)	-- _flag = a + b/b1/b2 + c
	if not fuef.NotNil("RES", _flag, _count) then return self end  -- nil chk
	if type(_flag) == "string" then
		local temp = 0
		for _,val in ipairs(fusf.CutString(_flag, "+", "RES_1")) do
			if val:match("PH") then
				temp = temp + RESET_PHASE 
				val = fusf.CutString(val, "/", "RES_2")
				for i = 2, #val do
					temp = temp + fucs.pha[val[i] ]
				end
			elseif val == "SELF" or val == "OPPO" or val == "CH" or val == "EV" then
				temp = temp + fucs.res[val]
			else -- add RESET_EVENT
				temp = (temp | RESET_EVENT) + fucs.res[val]
			end
		end
		_flag = temp
	end
	self.res = {_flag, _count}
	return self:Reload("RES")
end
----------------------------------------------------------------LAB
function fuef:LAB(...)
	if not fuef.NotNil("LAB", ...) then return self end  -- nil chk
	local _labs = {...}
	local labs = { }
	for _,_lab in ipairs(_labs) do
		if type(_lab) == "string" then 
			for _,lab in ipairs(fusf.CutString(_lab, "+", "LAB")) do
				labs[#labs + 1] = (lab == "m") and self.e:GetOwner():GetOriginalCode() or tonumber(lab)
			end
		else
			labs[#labs + 1] = _lab
		end
	end
	self.lab = labs
	return self:Reload("LAB")
end
----------------------------------------------------------------OBJ
function fuef:OBJ(_val)
	if not fuef.NotNil("OBJ", _val) then return self end  -- nil chk
	self.obj = _val
	return self:Reload("OBJ")
end
--------------------------------------------------------------------------"Support Effect function"
function fuef.initial(_lib, _glo, _ex_op)
	local cm, m = GetID()
	cm.pre = {}
	cm.initial_effect = function(c)
		if _lib then cm.lib = _lib end  -- set lib
		if _ex_op then _ex_op(c, cm) end	-- do ex_op
		-- do e1 ~ en
		local n = 1
		while cm["e"..n] do
			cm["e"..n](c)
			n = n + 1
		end
		-- do e1 ~ en in lib pre set
		n = 1
		while cm.pre["e"..n] do
			cm.pre["e"..n](c)
			n = n + 1
		end
		-- if cm[_glo] then do ge1 ~ gen
		if not (_glo and not cm[_glo]) then return end
		cm[_glo] = {0, 0}
		n = 1
		while cm["ge"..n] do
			cm["ge"..n](c, 1)
			n = n + 1
		end
	end
	return cm, m
end
function fuef.cos_in_tg(e,tp,eg,ep,ev,re,r,rp,chk)
	e:SetLabel(100)
	if chk==0 then return true end
end