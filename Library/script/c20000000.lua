require("expansions/script/c20099997")
if fuef then return end
fuef = { } --2024/10/7
fuef.__index = fuef 
fuef.DebugMode = false
---------------------------------------------------------------- Standard Register
-- no cod 
fuef.I = function(_owner, _handler, _ignore) return fuef:Creat("I", nil, _owner, _handler, _ignore) end
fuef.FG = function(_owner, _handler, _ignore) return fuef:Creat("F+G", nil, _owner, _handler, _ignore) end
-- need cod
for i,str in ipairs(fusf.CutString("S,F,E,S+C,F+C,E+C,F+TO,F+TF,S+TO,S+TF,X", "A,QO,QF", ",", "typ_reg_2")) do
	for _,typ in ipairs(fusf.CutString(str, ",", "typ_reg_2")) do
		local name = typ:gsub("+", "")
		fuef[name] = function(_owner, _cod, _handler, _ignore)
			if type(_owner) ~= "userdata" then _cod, _owner = _owner end -- _owner is cod (Noc
			if i == 2 then _cod = _cod or "FC" end -- A,QO,QF
			return fuef:Creat(typ, _cod, _owner, _handler, _ignore)
		end
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
			for _,_key in ipairs(fusf.CutString("cat,pro,ran,tran,res,lab,obj", ",", "__call_1")) do
				self[_key] = ori[_key] or nil
			end
			self:RCreat():SetKey("call Reg Noc"):Reg()
			for _,_key in ipairs(fusf.CutString("des,ctl,val,con,cos,tg,op", ",", "__call_2")) do
				if ori[_key] then fuef[_key:upper()](self, table.unpack(ori[_key])) end
			end
			local chk = ori.aft
			if chk then ori = ori.aft end
		until not chk
		return self
	end
	-- Creat and Clone all key
	local CE = setmetatable({ }, fuef)
	for _,_key in ipairs(fusf.CutString("typ,cod,des,cat,pro,ran,tran,ctl,val,con,cos,tg,op,res,lab,obj,handler", ",", "__call_3")) do
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
-- chk is Noc (use in every set key final return without cal need self.e
function fuef:Reload(_from)
	if not self.e then return self end
	return self:RCreat():SetKey("Reload <- ".._from):Reg()
end
function fuef.IsNil(from, ...)
	local res = fusf.IsNil(...)
	if res and fuef.DebugMode then Debug.Message("... IsNil <- "..from) end
	return res
end
function fuef:PreChk(from, ...)
	if not self.e then  --is Noc
		self[from:lower()] = {...}
		return false 
	end
	return not fuef.IsNil(from, ...)
end
----------------------------------------------------------------DES
function fuef:DES(_code, _id) -- (0), ("n"), (m), ("+1")
	if not self:PreChk("DES", _code, _id) then return self end
	self.des = fusf.GetDES(_code, _id, self.e:GetOwner():GetOriginalCode())
	return self:RCreat():SetKey("DES"):Reg()
end
----------------------------------------------------------------TYP, COD, CAT and PRO
function fuef:Cons_Model(_key, _val)
	if fuef.IsNil(_key:upper(), _val) then return self end  -- nil chk
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
function fuef:RAN(_loc)
	if fuef.IsNil("RAN", _loc) then return self end  -- nil chk
	self.ran = fusf.Get_Loc(_loc, nil, "fuef:RAN()")
	return self:Reload("RAN")
end
function fuef:TRAN(_loc1, _loc2)
	if fuef.IsNil("TRAN", _loc1, _loc2) then return self end  -- nil chk
	self.tran = {fusf.Get_Loc(_loc1, _loc2, "fuef:TRAN()")}
	return self:Reload("TRAN")
end
----------------------------------------------------------------CTL
function fuef:CTL(_count, _code, _pro) --count, code, pro
	if not self:PreChk("CTL", _count, _code, _pro) then return self end
	if type(_count) == "string" or _count > 99 then -- ("n+D") or (m) -> (1, "n+D") or (1, m)
		_count, _code, _pro = 1, _count, _code 
	end
	local res, ctl_val = {0, 0}, {
		O = EFFECT_COUNT_CODE_OATH,
		D = EFFECT_COUNT_CODE_DUEL,
		C = EFFECT_COUNT_CODE_CHAIN,
	}
	if _pro then -- (1, n, "D")
		res[1] = fusf.M_chk(_code) + ctl_val[_pro:match("[ODC]")]
	elseif type(_code) == "string" then -- (1, "n+D")
		res = fusf.CutString(_code, "+", "CTL_1") -- (n), (n, d), (d)
		if res[1]:match("[ODC]") then
			res = {0, res[1]}
		elseif res[1]:match("m") then
			res[1] = self.e:GetOwner():GetOriginalCode()
		else
			res[1] = fusf.M_chk(tonumber(res[1]))
		end
		if res[2] then res[2] = ctl_val[res[2]:match("[ODC]")] end
		if res[2] & 0x30000000 > 0 and res[1] == 0 then res[1] = self.e:GetOwner():GetOriginalCode() end -- is O or D
	end
	self.ctl = {_count, res[1] + res[2]}
	return self:RCreat():SetKey("CTL"):Reg()
end
----------------------------------------------------------------VAL, CON, COS, TG and OP
function fuef:Func(_val, _func, ...)
	-- func = ("val,con,cos(v1,v2),tg,op") or ("con(v1,v2),op") or (val, "con,op(v1, v2)"), if v = %1~n then { ... } is value table
	if fusf.IsNil(_val, _func) then   -- nil chk
		if fuef.DebugMode then Debug.Message("_val, _func IsNil <- Func"..from) end
		return self
	end
	local vals = {...}
	if not (type(_val) == "string" and _val:match("%,")) then -- check _val is val
		local val = { _val }
		if type(_val) == "string" and _val:match("%%") then val = { _val , ... } end
		self.val = val 
	else	--  _val is _func
		vals, _func = {_func, ...}, _val
	end
	-- "val,con,cos,tg,op" or "con,op" , if cant match then follow a sequence
	local seqs, place, sets = {"val", "con", "cos", "tg", "op"}, 1, { }
	 -- "f1,f2(%1,v2),f3" -> {"f1", {"f2", vals[1], "v2"}, "f3"}, ... use in vi = %i
	for i, func in fusf.ForTable(fusf.Val_Cuts(_func, table.unpack(vals))) do
		local fname, fval = func, func
		if type(fname) == "table" then
			fname = table.remove(fval,1)
		else 
			fval = nil
		end
		-- find fname can match seqs
		local match = 0
		for j = place, 5 do
			local temp = fname:find(seqs[j])
			if temp and temp > match then
				place, match = j, temp
			end
		end
		-- cant match seqs
		if match == 0 then place = i end
		-- set self[key] and save key
		func = seqs[place]
		self[func], sets[#sets + 1] = {fname, fval}, func -- self[func] = {name, {...}} or {name}
	end
	if not self.e then return self end
	-- self[key] = {func, ...}
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
function fuef:Func_Model(_key, _func, ...)
	if not self:PreChk(_key:upper(), _func, ...) then return self end
	local val_chk = _key == "val" and (tonumber(_func) or fucs.val[_func]) or nil
	local vals = select("#", ...) > 0 and { ... } or nil
	if vals and #vals == 1 and type(vals[1]) == "table" then
		vals = vals[1]  -- 若 vals 中只有一个表，则直接解包表
	end
	self[_key] = val_chk or fusf.Get_Func(self.e:GetOwner(), _func, vals)
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
function fuef:RES(_flag, _count)	-- _flag = a + b/b1/b2 + c | 1
	if fuef.IsNil("RES", _flag, _count) then return self end  -- nil chk
	self.res = fusf.GetRES(_flag, _count)
	return self:Reload("RES")
end
----------------------------------------------------------------LAB
function fuef:LAB(...)
	if fuef.IsNil("LAB", ...) then return self end  -- nil chk
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
	if fuef.IsNil("OBJ", _val) then return self end  -- nil chk
	self.obj = _val
	return self:Reload("OBJ")
end
--------------------------------------------------------------------------"Support Effect function"
function fuef.initial(_lib, _glo, _exop_func, ...)
	local cm, m = GetID()
	local exop_val = { ... }
	cm.pre = {}
	cm.initial_effect = function(c)
		if _lib then cm.lib = _lib end  -- set lib
		-- do ex_op
		if _exop_func then 
			local place = 1
			if type(_exop_func) ~= "table" then _exop_func = { _exop_func } end
			for _, exop_func in ipairs(_exop_func) do
				if type(exop_func) == "string" then 
					for _, func in ipairs(fusf.CutString(exop_func, ",", "fuef.initial")) do
						(fucf[func] or Card[func])(c, exop_val[place])
						place = place + 1
					end
				else
					exop_func(c, exop_val[place])
					place = place + 1
				end
			end
		end
		local dof = function(_tab, _name, _exval)
			local n = 1
			while _tab[_name..n] do
				_tab[_name..n](c, _exval)
				n = n + 1
			end
		end
		dof(cm, "e")		-- do e1 ~ en
		dof(cm.pre, "e")	-- do e1 ~ en in lib pre set
		-- if cm[_glo] then do ge1 ~ gen
		if not (_glo and not cm[_glo]) then return end
		cm[_glo] = {0, 0}
		dof(cm, "ge", 1)
	end
	return cm, m
end
function fuef.tg_is_cos(e,tp,eg,ep,ev,re,r,rp,chk)
	e:SetLabel(100)
	if chk==0 then return true end
end