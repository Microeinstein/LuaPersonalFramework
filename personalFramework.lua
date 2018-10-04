--Personal framework for Lua 5.3
--by Microeinstein

--[[
Personal lua notes:
	- Ind(ic|ex)es starts at 1... damn lua;
	- Semicolons are optionals, in a regular non-minified source (think about comments for ex.);
	- "\r" isn't necessary for new lines, even on Windows;
	- Does not support unicode strings... The table "utf8" is essential for this.
	- Command line arguments are available through "arg" table globally;
	- "#" measures length of strings, which are not indexed;
	- "#" measures amount of values with integral keys;
	- "table.concat" doesn't accept nil values;
	- "ipairs", "table.concat", "table.unpack" enumerate all values with integral keys;
	- "pairs" enumerates everything except keys with nil;
	- Table can contains every type of KEY and VALUE;
	- Integral keys:  {1,1,nil}=>2,  {1,1,nil,1}=>4
	- "and" and "or" works also for nil-check, but boolean-check can interfere;
	- In Lua 5.1:
		- Functions parameter lists (...) are accessed by "arg";
		- There is unpack();
	- In Lua 5.3:
		- Functions parameter lists (...) are accessed by "...";
		- There is table.unpack() and table can't be null;
]]--



if not _PERSONAL_FRAMEWORK_ then
	utf8._len = utf8.len
	utf8._codes_iter, _, _ = utf8.codes("")
	utf8._codes = utf8.codes
end
function utf8.len(str)
	--if i > 0 and j > 0 and i < j then
	--	return 0
	--elseif i < 0 and j < 0 and i > j then
	--	return 0
	--end
	local l = 0
	local p = 1
	local op = 0
	
	::finoallafine::
	local ul, ep = utf8._len(str, p)
	--print("UntilEnd:", ul, ep)
	if ul then
		goto fine
	end
	
	if ep == p then
		--print("FoundBad")
		l = l + 1
		p = p + 1
		goto finoallafine
	end
	--print("FoundGood")
	op = ep
	ep = ep - 1
	ul, ep = utf8._len(str, p, ep)
	--print("UntilChunk:", ul, ep)
	if not ul then
		error([[This can't happen, because last "ep" is used for current check...]])
	end
	l = l + ul
	p = op
	goto finoallafine
	
	::fine::
	l = l + ul
	--print("End:", l)
	return l
end
function utf8.codes(str)
	local ok, p, v
	return function(s, i)
		local ok2, p2, v2 = ok, p, v
		ok, p, v = pcall(utf8._codes_iter, s, i)
		if ok then
			--print("u", p)
			return p, v, true
		else
			if ok2 then
				i = i + #(utf8.char(v2))
			else
				i = i + 1
			end
			--print("a", i)
			return i, string.byte(s, i, i), false
		end
	end, str, 0
end
function utf8.sub(str, i, j)
	j = j or -1
	if i == 1 and j == -1 then
		return str
	end
	if type(str) ~= "string" or type(i) ~= "number" or type(j) ~= "number" then
		error("There are arguments with invalid types.")
	end
	local l = utf8.len(str)
	if string.len(str) == l then
		return string.sub(str, i, j)
	end
	if i < 0 then i = l + i + 1 end
	if j < 0 then j = l + j + 1 end
	local ret = ""
	local c = 0
	for _, v, u in utf8.codes(str) do
		c = c + 1
		if c <= j then
			if c >= i then
				ret = ret .. (u and utf8.char(v) or string.char(v))
			end
		else
			return ret
		end
	end
	return ret
end

term = {}
process = {}
path = {}
utils = {}

path.dirSep = utf8.sub(package.config, 1, 1)
process.isWindows = path.dirSep == "\\"
if process.isWindows then
	process.ansiSupported = os.getenv("ANSICON_DEF") ~= nil or os.getenv("ConEmuANSI") == "ON"
	process.clear_cmd     = "cls"
	process.nativehelp    = "winNativeHelper"
	process.random_cmd    = "cscript /nologo winRand.js"
	process.random_max    = 500000000
	process.libraryExt    = "dll"
	package.newLine = "\r\n"
	path.dir_cmd   = [[dir "%s" /b /ad]]
	path.file_cmd  = [[dir "%s" /b /a-d]]
	path.all_cmd   = [[dir "%s" /b]]
	path.dirr_cmd  = [[dir "%s" /b /ad /s]]
	path.filer_cmd = [[dir "%s" /b /a-d /s]]
	path.allr_cmd  = [[dir "%s" /b /s]]
	term.box1hv    = "\197"
	term.box1vr    = "\195"
	term.box1vl    = "\180"
	term.box1hu    = "\193"
	term.box1hd    = "\194"
	term.box1v     = "\179"
	term.box1h     = "\196"
	term.box1ur    = "\192"
	term.box1ul    = "\217"
	term.box1dr    = "\218"
	term.box1dl    = "\191"
	term.arrowr    = "\26"
	term.middledot = "\250"
else
	local t = os.getenv("TERM")
	process.ansiSupported = t ~= nil and t ~= "dumb"
	process.clear_cmd     = "clear"
	process.random_cmd    = "echo $RANDOM"
	process.random_max    = 32767
	process.libraryExt    = "so"
	package.newLine = "\n"
	path.dir_cmd   = [[find "%s" -maxdepth 1 -type d -printf '%%f\n']]
	path.file_cmd  = [[find "%s" -maxdepth 1 -type f -printf '%%f\n']]
	path.all_cmd   = [[find "%s" -maxdepth 1 -printf '%%f\n']]
	path.dirr_cmd  = [[find "%s" -type d -printf '%%P\n']]
	path.filer_cmd = [[find "%s" -type f -printf '%%P\n']]
	path.allr_cmd  = [[find "%s" -printf '%%P\n']]
	term.box1hv    = "\u{253C}"
	term.box1vr    = "\u{251C}"
	term.box1vl    = "\u{2524}"
	term.box1hu    = "\u{2534}"
	term.box1hd    = "\u{252C}"
	term.box1v     = "\u{2502}"
	term.box1h     = "\u{2500}"
	term.box1ur    = "\u{2514}"
	term.box1ul    = "\u{2518}"
	term.box1dr    = "\u{250C}"
	term.box1dl    = "\u{2510}"
	term.arrowr    = "\u{2192}"
	term.middledot = "\u{2E31}"
end
term.boxflag_u = 0x1
term.boxflag_r = 0x2
term.boxflag_d = 0x4
term.boxflag_l = 0x8
term.boxFlags = {
	{ term.box1hv, (term.boxflag_u | term.boxflag_r | term.boxflag_d | term.boxflag_l) },
	{ term.box1vr, (term.boxflag_u | term.boxflag_r | term.boxflag_d) },
	{ term.box1vl, (term.boxflag_u | term.boxflag_d | term.boxflag_l) },
	{ term.box1hu, (term.boxflag_u | term.boxflag_r | term.boxflag_l) },
	{ term.box1hd, (term.boxflag_r | term.boxflag_d | term.boxflag_l) },
	{ term.box1v,  (term.boxflag_u | term.boxflag_d) },
	{ term.box1h,  (term.boxflag_r | term.boxflag_l) },
	{ term.box1ur, (term.boxflag_u | term.boxflag_r) },
	{ term.box1ul, (term.boxflag_u | term.boxflag_l) },
	{ term.box1dr, (term.boxflag_r | term.boxflag_d) },
	{ term.box1dl, (term.boxflag_d | term.boxflag_l) },
}
term.lastInput = ""

utils.filtersParsed = {} --reserved for filters

if process.nativehelp then
	local lib64 = string.format("%s64.%s", process.nativehelp, process.libraryExt)
	local lib32 = string.format("%s32.%s", process.nativehelp, process.libraryExt)
	local libinit = package.loadlib(lib64, "libinit") or package.loadlib(lib32, "libinit")
	if libinit then
		libinit()
	end
end

--WARNING: On Windows please install ANSICON or ConEmu, otherwise stick with native helper
function term.moveCursor(dy, dx)
	dy = dy or 0
	dx = dx or 0
	if not process.ansiSupported then
		if nativeHelper then
			nativeHelper.moveCursor(false, dx, dy)
		end
		return
	end
	if dy < 0 then
		io.write(string.format("\27[%dA", -dy))
	elseif dy > 0 then
		io.write(string.format("\27[%dB", dy))
	end
	if dx < 0 then
		io.write(string.format("\27[%dD", -dx))
	elseif dx > 0 then
		io.write(string.format("\27[%dC", dx))
	end
end
function term.setCursor(y, x)
	y = y and y > 0 and y or 1
	x = x and x > 0 and x or 1
	if not process.ansiSupported then
		if nativeHelper then
			nativeHelper.moveCursor(true, x, y)
		end
		return
	end
	io.write(string.format("\27[%d;%dH", y, x))
end
function term.clear()
	if not process.ansiSupported then
		if nativeHelper then
			nativeHelper.clearAll()
		else
			os.execute(process.clear_cmd)
		end
		return
	end
	io.write("\27[3J")
	term.setCursor()
	io.write("\27[2J")
end
function term.toBoxFlags(glyph)
	local _, flags = table.first(term.boxFlags, function(k, v) return glyph == v[1] end)
	return flags and flags[2] or 0
end
function term.fromBoxFlags(flags)
	local _, glyph = table.first(term.boxFlags, function(k, v) return flags == v[2] end)
	return glyph and glyph[1] or ""
end

function process.pause()
	local msg = "[press enter]"
	print(msg)
	term.lastInput = io.read()
	if process.ansiSupported or nativeHelper then
		term.moveCursor(-2)
		print(string.rep(" ", #msg))
		term.moveCursor(-1)
	end
	return term.lastInput
end
function process.getOutput(cmd)
	local t, i = {}, 0
	local output = io.popen(cmd)
	for line in output:lines() do
		i = i + 1
		t[i] = line
	end
	output:close()
	return t
end

function path.combine(...)
	local args = {...}
	local ret = {}
	local iret = 1
	for i, v in ipairs(args) do
		if v then
			local ch = string.chars(v)
			local l = #ch
			local remA = ch[1] == path.dirSep
			local remB = ch[l] == path.dirSep
			if remA or remB then
				ret[iret] = table.concat(ch, "", remA and 2 or 1, remB and l - 1 or l)
			else
				ret[iret] = v
			end
			iret = iret + 1
		end
	end
	return table.concat(ret, path.dirSep)
end
function path.getDirs(dir, recursive)
	recursive = recursive or false
	local proc = string.format(recursive and path.dirr_cmd or path.dir_cmd, dir)
	return process.getOutput(proc)
end
function path.getFiles(dir, recursive)
	recursive = recursive or false
	local proc = string.format(recursive and path.filer_cmd or path.file_cmd, dir)
	return process.getOutput(proc)
end
function path.getAll(dir, recursive)
	recursive = recursive or false
	local proc = string.format(recursive and path.allr_cmd or path.all_cmd, dir)
	return process.getOutput(proc)
end

function math.round(num)
	return math.floor(num + .5)
end
function math.between(a, v, b)
	return math.max(a, math.min(v, b))
end
function math.isInteger(num)
	return num % 1 == 0
end
function math.nicerandom(a, b)
	local res
	if nativeHelper then
		res = nativeHelper.random()
	else
		local rnd = process.getOutput(process.random_cmd)[1]
		local rndm = process.random_max
		res = rnd / rndm
	end
	if a then
		if not b then
			b = a
			a = 1
		end
		if a == b then
			return a
		elseif a > b then
			a, b = b, a
		end
		res = a + (res * (b - a))
	end
	return math.floor(res)
end
math.randomseed(math.nicerandom(1, process.random_max))

function string.bytes(self)
	local ret = {}
	for i = 1, #self do
		ret[i] = string.byte(self, i, i)
	end
	return ret
end
function string.chars(self)
	local l = utf8.len(self)
	local ch = {}
	for i = 1, l do
		table.insert(ch, utf8.sub(self, i, i))
	end
	return ch
end
function string.join(sep, ...)
	return table.concat({...}, sep)
end
function string.split(self, sep, esc)
	if type(sep) == "number" then
		if self then
			local l = utf8.len(self)
			sep = math.between(0, sep, l)
			return utf8.sub(self, 1, sep), utf8.sub(self, sep + 1, l)
		else
			return "", ""
		end
	elseif type(sep) == "string" then
		local ret = {}
		local iret = 1
		if self then
			--[[Old sluggish code
			local l = utf8.len(self)
			local ls = utf8.len(sep)
			local res = {}
			local escstring = type(esc) == "string"
			local escape = false
			local sepA, sepB, last = 0, 0, 1
			for i = 1, l do
				local c = utf8.sub(self, i, i)
				sepB = sepA + 1
				if esc ~= nil and escstring and sepA == 0 and not escape and c == esc then
					escape = true
				elseif c == utf8.sub(sep, sepB, sepB) then
					sepA = sepB
					if sepA == ls then
						if not escape then
							local s = utf8.sub(self, last, i - ls)
							print(#res)
							table.insert(res, s)
							last = i + 1
						end
						escape = false
						sepA = 0
					end
				else
					escape = false
					sepA = 0
				end
			end
			local s = utf8.sub(self, last, l)
			table.insert(res, s)
			return res
			]]
			
			if sep == "" or self == "" then
				return { self }
			end
			local charsE, lenE
			if esc == "" then
				esc = nil
			end
			if esc then
				charsE = string.chars(esc)
				lenE = #charsE
			end
			local charsB = string.chars(sep)
			local lenB = #charsB
			local charsA = string.chars(self)
			local lenA = #charsA
			if lenA < lenB then
				return {}
			end
			local sliceSep = ""
			local sliceEsc = ""
			local nextTextAnchor = 1
			local escaped = false
			local nearEscaped = {}
			local inesc = 1
			for i, v in ipairs(charsA) do
				if i >= nextTextAnchor and v == charsB[1] then
					--io.write(i .. "?")
					sliceSep = table.concat(table.sub(charsA, i, i + lenB - 1))
					--io.write(sliceSep)
					if sliceSep == sep then --i is always at "Text Esc [S]ep"
						--io.write("~")
						escaped = false
						if esc and i > lenE and charsA[i - lenE] == charsE[1] then
							sliceEsc = table.concat(table.sub(charsA, i - lenE, i - 1))
							--io.write(sliceEsc)
							escaped = sliceEsc == esc
						end
						if not escaped then
							--io.write("!")
							ret[iret] = table.concat(nearEscaped) .. table.concat(table.sub(charsA, nextTextAnchor, i - 1))
							iret = iret + 1
							nearEscaped = {}
							inesc = 1
						else
							--io.write("x")
							nearEscaped[inesc] = table.concat(table.sub(charsA, nextTextAnchor, i - 1 - lenE)) .. sep
							inesc = inesc + 1
						end
						nextTextAnchor = i + lenB
					end
					--io.write("\n")
				end
			end
			ret[iret] = table.concat(table.sub(charsA, nextTextAnchor, lenA))
		end
		return ret
	end
end
function string.firstUpper(self)
	local ch = string.chars(self)
	ch[1] = string.upper(ch[1])
	return table.concat(ch)
end
function string.replace(self, from, to)
	if #from > #self then
		return self
	end
	return table.concat(string.split(self, from), to)
end
function string.startsWith(self, str)
	if self and str then
		if self == str then
			return true
		end
		local lenA = utf8.len(self)
		local lenB = utf8.len(str)
		local lenMin = lenA - lenB + 1
		if lenMin < 1 then
			return false
		end
		local slice = utf8.sub(self, 1, lenB)
		return slice == str
	else
		return false
	end
end
function string.contains(self, str)
	if self and str then
		if self == str then
			return true
		end
		local lenA = utf8.len(self)
		local lenB = utf8.len(str)
		local lenMin = lenA - lenB + 1
		if lenMin < 1 then
			return false
		end
		for i = 1, lenMin do
			local slice = utf8.sub(self, i, i + lenB - 1)
			if slice == str then
				return true
			end
		end
		return false
	else
		return false
	end
end
function string.padLeft(self, length, padder)
	if padder == nil then padder = ' ' end
	return self .. string.rep(padder, length - utf8.len(self)) 
end
function string.padRight(self, length, padder)
	if padder == nil then padder = ' ' end
	return string.rep(padder, length - utf8.len(self)) .. self
end
function string.lines(self)  --returns lines, max_width, max_height
	local lines = string.split(self, "\n")
	return lines, string.maxWidth(lines), #lines
end
function string.maxWidth(linesTable)
	return table.aggregate(table.select(linesTable, function(k,v) return utf8.len(v) end), function(k,v,r) return math.max(v,r) end)
end
--[[Useless code
function string.firstLine(self)
	return table.concat(table.takeWhile(self, function(k, v) return v ~= "\n" end))
end
function string.lastLine(self)
	return table.concat(table.reverse(table.takeWhile(table.reverse(self), function(k, v) return v ~= "\n" end)))
end]]

function utils.parseFilters(filters)
	local parsed = false
	filters = filters or {}
	if filters[utils.filtersParsed] == nil then
		parsed = true
		local cleaned = {}
		for _, v in pairs(filters) do
			if type(v) == "table" then
				cleaned[v] = true
			end
		end
		cleaned[utils.filtersParsed] = true
		filters = cleaned
	end
	return filters, parsed
end
--http://lua-users.org/wiki/BaseSixtyFour
utils._base64_chars_ = { [0] =
   'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
   'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
   'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
   'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/',
}
function utils.base64(s)
   local bs, byte, rep = utils._base64_chars_, string.byte, string.rep
   local pad = 2 - ((#s-1) % 3)
   s = (s..rep('\0', pad)):gsub("...", function(cs)
      local a, b, c = byte(cs, 1, 3)
      return bs[a>>2] .. bs[(a&3)<<4|b>>4] .. bs[(b&15)<<2|c>>6] .. bs[c&63]
   end)
   return s:sub(1, #s-pad) .. rep('=', pad)
end
function utils.num2hex(num, isFloat)
	return string.format("%x", isFloat and (num * 255) or math.floor(num))
end
function utils.hsv2hex(hue, saturation, value) --(0-360, 0-1, 0-1)
	local function s(n)
		local h = utils.num2hex(n, false)
		return #h < 2 and ("0"..h) or h
	end
	local h6 = hue / 60
	local fh6 = math.floor(h6)
	local hi = fh6 % 6
	local f = h6 - fh6
	
	value = value * 255
    local v = math.floor(value)
    local p = math.floor(value * (1 - saturation))
    local q = math.floor(value * (1 - f * saturation))
    local t = math.floor(value * (1 - (1 - f) * saturation))
	
	if hi == 0 then
		return s(v)..s(t)..s(p)
	elseif hi == 1 then
		return s(q)..s(v)..s(p)
	elseif hi == 2 then
		return s(p)..s(v)..s(t)
	elseif hi == 3 then
		return s(p)..s(q)..s(v)
	elseif hi == 4 then
		return s(t)..s(p)..s(v)
	else
		return s(v)..s(p)..s(q)
	end
end

function table.len(self, nils) --nils = count also nil values with an integral key
	nils = nils or true
	local c = 0
	local ma = 0
	for k, v in pairs(self) do
		if nils and type(k) == "number" and k >= 1 and math.isInteger(k) then
			ma = math.max(ma, k)
		else
			c = c + 1
		end
	end
	return c + ma
end
function table.rep(obj, amount)
	local ret = {}
	for i = 1, amount do
		ret[i] = obj
	end
	return ret
end
function table.reverse(self, onlyNumericalKeys)
	onlyNumericalKeys = onlyNumericalKeys or false
	local indexed, reversed = {}, {}
	local n = 0
	for k, v in pairs(self) do
		if onlyNumericalKeys or type(k) == "number" then
			n = n + 1
			indexed[n] = v
		else
			indexed[k] = v
		end
	end
	local l = #indexed
	for i, v in pairs(indexed) do
		if onlyNumericalKeys or type(i) == "number" then
			reversed[l - i + 1] = v
		else
			reversed[i] = v
		end
	end
	return reversed
end
function table.compact(self)
	local ret = {}
	local numKey = 1
	local numbersEnd = false
	for k, v in pairs(self) do
		if numbersEnd or type(k) ~= "number" then
			numbersEnd = true
			ret[k] = v
		else
			ret[numKey] = v
			numKey = numKey + 1
		end
	end
	return ret
end
function table.sub(self, from, to)
	to = to or -1
	if from == 1 and to == -1 then
		return self
	end
	local l = #self
	if l == 0 then
		return {}
	end
	if from < 0 then from = l + from + 1 end
	if to < 0 then to = l + to + 1 end
	if from > to then
		return {}
	elseif from == to then
		return { self[from] }
	end
	local ret = {}
	local iret = 1
	for i, v in ipairs(self) do
		if i <= to then
			if i >= from then
				ret[iret] = v
				iret = iret + 1
			end
		else
			return ret
		end
	end
	return ret
end
function table.copy(self, deep, filters, lambdaFilter) --from http://lua-users.org/wiki/CopyTable shallow + deep + personal mod
	deep = deep or false
	filters = utils.parseFilters(filters)
	local copy
	local function isFiltered(a)
		return filters[a] ~= nil or (lambdaFilter and lambdaFilter(a))
	end
	if type(self) == "table" then
		if isFiltered(self) then
			return self
		end
		copy = {}
		for k, v in pairs(self) do
			if deep then
				copy[table.copy(k, true, filters)] = table.copy(v, true, filters)
			else
				copy[k] = v
			end
		end
		if deep then
			setmetatable(copy, table.copy(getmetatable(self)))
		end
	else
		copy = self
	end
	return copy
end
function table.append(self, values, onlyNumericalKeys)
	onlyNumericalKeys = onlyNumericalKeys or false
	local l = #self
	if onlyNumericalKeys then
		for _, v in ipairs(values) do
			l = l + 1
			self[l] = v
		end
	else
		for _, v in pairs(values) do
			l = l + 1
			self[l] = v
		end
	end
end
function table.contains(self, value, onlyNumericalKeys, rec)
	onlyNumericalKeys = onlyNumericalKeys or false
	rec = rec or false
	local function check(v)
		if v == value then
			return true
		elseif rec and type(v) == "table" and table.contains(v, value, onlyNumericalKeys, rec) then
			return true
		end
		return false
	end
	if onlyNumericalKeys then
		for _, v in ipairs(self) do
			if check(v) then
				return true
			end
		end
	else
		for _, v in pairs(self) do
			if check(v) then
				return true
			end
		end
	end
	return false
end
function table.exists(self, lambda, ...)
	for k, v in pairs(self) do
		if lambda(k, v, ...) then
			return true
		end
	end
	return false
end
function table.trueForAll(self, lambda, ...)
	for k, v in pairs(self) do
		if not lambda(k, v, ...) then
			return false
		end
	end
	return true
end
function table.first(self, lambda, ...)
	for k, v in pairs(self) do
		if lambda(k, v, ...) then
			return k, v
		end
	end
end
function table.last(self, lambda, ...)
	local a, b
	for k, v in pairs(self) do
		if lambda(k, v, ...) then
			a, b = k, v
		end
	end
	return a, b
end
function table.takeWhile(self, lambda, ...)
	local ret = {}
	local iret = 1
	for k, v in pairs(self) do
		if lambda(k, v, ...) then
			ret[iret] = v
			iret = iret + 1
		else
			return ret
		end
	end
	return ret
end
function table.takeWhere(self, lambda, ...) --same as "where", but with numerical keys
	local ret = {}
	local iret = 1
	for k, v in pairs(self) do
		if lambda(k, v, ...) then
			ret[iret] = v
			iret = iret + 1
		end
	end
	return ret
end
function table.where(self, lambda, ...)
	local ret = {}
	for k, v in pairs(self) do
		if lambda(k, v, ...) then
			ret[k] = v
		end
	end
	return ret
end
function table.select(self, lambda, ...)
	local ret = {}
	for k, v in pairs(self) do
		local a, b = lambda(k, v, ...)
		local k2 = b == nil and k or a
		local v2 = b == nil and a or b
		ret[k2] = v2
	end
	return ret
end
function table.aggregate(self, lambda, ...)
	local ret = nil
	local skip1 = true
	for k, v in pairs(self) do
		if skip1 then
			ret = v
			skip1 = false
		else
			ret = lambda(k, v, ret, ...)
		end
	end
	return ret
end
function table.groupBy(self, lambda, ...)
	local ret = {}
	for k, v in pairs(self) do
		local g = lambda(k, v, ...)
		if g ~= nil then
			local gt = ret[g] or {}
			gt[#gt + 1] = v
			ret[g] = gt
		end
	end
	return ret
end
function table.allNumerical(self, distinct)
	distinct = distinct or false
	local ret = {}
	local iret = 1
	for _, v in pairs(self) do
		if not distinct or not table.contains(ret, v) then
			ret[iret] = v
			iret = iret + 1
		end
	end
	return ret
end
function table.allToString(self)
	if not self then
		return "<nil>"
	end
	for k, v in pairs(self) do
		self[k] = tostring(v)
	end
	return self
end
function table.makePrototype(self, skipCollection)
	local keysType = nil
	local length = 0
	local subKeys = {}
	skipCollection = skipCollection or false
	for k, _ in pairs(self) do
		length = length + 1
		local tk = type(k)
		if keysType == nil then
			keysType = tk
		elseif keysType ~= "mixed" then
			if keysType ~= tk then
				keysType = "mixed"
			end
		end
		if not skipCollection then
			local subType = subKeys[tk] or {}
			subType[#subType + 1] = k
			subKeys[tk] = subType
		end
	end
	return {
		type = keysType,
		length = length,
		keys = subKeys,
	}
end
function table.hasEqualPrototype(self, proto) --(table, prototype)
	local function dprint(...)
		if false then
			print(...)
		end
	end
	local keysType = nil
	local length = 0
	--local diffKeys = 0
	for k, _ in pairs(self) do
		length = length + 1
		if length > proto.length then
			dprint("more_elements")
			return false
		end
		local tk = type(k)
		if keysType == nil then
			keysType = tk
		elseif keysType ~= "mixed" then
			if keysType ~= tk then
				keysType = "mixed"
			end
		end
		if keysType == "mixed" and proto.type ~= "mixed" then
			dprint("diff_keys_seq")
			--do not edit
			return false
		end
		local ptk = proto.keys[tk] --prototype type-keys
		if ptk == nil or not table.contains(ptk, k) then
			--diffKeys = diffKeys + 1
			return false
		end
	end
	--if diffKeys > math.max(length, proto.length) * 0.5 then
	--	dprint("not_enough_keys")
	--	return false
	--end
	if keysType ~= proto.type then
		dprint("diff_keys_seq")
		return false
	end
	return true
end
function table.singleKey(self)
	local k1 = nil
	for k, v in pairs(self) do
		if k1 == nil then
			k1 = k
		else
			return nil
		end
	end
	return k1
end
function table.combine(list, distinct, stringKeysMode)
	distinct = distinct or false
	--[[stringKeysMode
		0 (default)	: Set keys to last values encountered
		1			: Set keys to first values encountered
	]]
	stringKeysMode = stringKeysMode or 0
	local ret = {}
	local numKey = 1
	local numbersEnd
	for _, t in pairs(list) do
		numbersEnd = false
		if type(t) == "table" then
			for k, v in pairs(t) do
				if numbersEnd or type(k) ~= "number" then
					numbersEnd = true
					if stringKeysMode == 0 or ret[k] == nil then
						ret[k] = v
					end
				elseif not distinct or not table.contains(ret, v, true) then
					ret[numKey] = v
					numKey = numKey + 1
				end
			end
		end
	end
	return ret
end
function table.blend(self, filters, lambdaFilter, lvl, prefix)
	--choice: blend string keys only at usercall, otherwise keep
	
	local result = {}
	local numbersEnd1 = false
	local numKey = 1
	lvl = lvl or 0
	local userCall = lvl == 0
	
	local function dprint(loc, ...)
		if debug.trigger1 then
			print(string.rep(" ", lvl + (loc and -1 or 0)) .. (loc and "+" or " ") .. table.concat({...}))
		end
	end
	local function isFiltered(a, s)
		return (s or type(a) == "table") and (filters[a] ~= nil or (lambdaFilter and lambdaFilter(a)))
	end
	local function setOrAdd(k, v)
		numbersEnd1 = numbersEnd1 or type(k) ~= "number"
		if not numbersEnd1 then
			k = numKey
			numKey = numKey + 1
		end
		local values = result[k]
		if values == nil then
			values = v
		elseif isFiltered(values) or type(values) ~= "table" then
			if values ~= v then
				values = { values, v }
			end
		elseif not table.contains(values, v, true) then
			values[#values + 1] = v
		end
		result[k] = values
	end
	local function removeFrom(a, b)
		for k, v in pairs(a) do
			if b[k] ~= nil then
				a[k] = nil
			end
		end
	end
	local function isGoodList(a, it)
		return (it or type(a) == "table")
		and not isFiltered(a)
		and table.makePrototype(a, true).type == "number"
	end
	
	if userCall then
		dprint(false, "")
		dprint(false, "usercall")
	end
	
	--(f) if usercall, transform list into a dictionary (from => to)
	filters = utils.parseFilters(filters)
	
	--debug print location
	prefix = prefix or "."
	dprint(true, prefix)
	--process.pause()
	
	--(t) only tables allowed
	if type(self) ~= "table" then
		dprint(false, "self_not_table")
		return self
	end
	
	--(d) duplicate to avoid mess in original tables
	if userCall then
		self = table.copy(self, true, filters, lambdaFilter)
	end
	
	--(c) check filter
	if isFiltered(self) then
		dprint(false, "self_filtered")
		return self
	else
		filters[self] = true
	end
	
	--(s) in case of single key, keep key
	local sk = table.singleKey(self)
	if sk ~= nil then
		local sv = self[sk]
		if isFiltered(sv) then
			dprint(false, "single_key_filtered")
			return self
		elseif type(sk) == "number" then
			dprint(false, "single_key_number")
			return sv
		else
			dprint(false, "single_key")
			result[sk] = table.blend(sv, filters, optimizer, lvl+1, sk)
			return result
		end
	end
	
	--(1) blend core
	dprint(false, "blend")
	for k1, v1 in pairs(self) do
		if type(v1) ~= "table" or isFiltered(v1, true) or (not userCall and type(k1) ~= "number") then
			dprint(false, " -"..k1)
			setOrAdd(k1, v1)
		else
			dprint(false, " +"..k1)
			for k2, v2 in pairs(v1) do
				dprint(false, "  -"..k2)
				setOrAdd(k2, v2)
			end
		end
	end
	
	--(2) merge sub-lists with main list
	for k1, v1 in pairs(result) do
		local equalIters = 0
		local vlen1 = 0
		local vlen2 = 0
		while equalIters < 5 and isGoodList(v1) do
			vlen2 = vlen1
			local sublists = table.where(v1, function(k, v) return isGoodList(v) end)
			if table.len(sublists) > 0 then
				dprint(false, "merge_lists "..k1)
				removeFrom(v1, sublists)
				v1 = table.compact(v1)
				for k2, v2 in pairs(sublists) do
					v1 = table.combine{v1, v2}
				end
				vlen1 = table.len(v1)
				result[k1] = v1
			end
			if vlen1 == vlen2 then
				equalIters = equalIters + 1
			else
				equalIters = 0
			end
		end
	end
	
	--(3) avoid infinite recursion
	local filters2 = {}
	for k, v in pairs(filters) do
		filters2[k] = v
	end
	for k, v in pairs(self) do
		if type(v) == "table" and not isFiltered(v, true) then
			filters2[v] = true
		end
	end
	
	--(4) repeat for sub-tables
	dprint(false, "loop")
	for k1, v1 in pairs(result) do
		if type(v1) == "table" and not isFiltered(v1, true) then
			result[k1] = table.blend(v1, filters2, lambdaFilter, lvl+1, k1)
		end
	end
	
	dprint(false, "end")
	return result
end
function table.extract(self, keys, target, delete)
	delete = delete or false
	target = target or {}
	if #keys == 0 then
		return target
	end
	for _, k in pairs(keys) do
		if self[k] ~= nil then
			target[k] = self[k]
			if delete then
				self[k] = nil
			end
		end
	end
	return target
end
function table.intersect(a, b)
	local ret = {}
	local numbersEnd = false
	local numKey = 1
	for k, v in pairs(a) do
		numbersEnd = numbersEnd or (type(k) ~= "number" or not math.isInteger(k))
		if numbersEnd then
			if v == b[k] then
				ret[k] = v
			end
		else
			if table.contains(b, v, true) then
				ret[numKey] = v
				numKey = numKey + 1
			end
		end
	end
	return ret
end


function rPrint(struct, maxdepth, name, pt, pv, prefix, idepth, last)   --Recursive Print
	maxdepth = maxdepth or 10
	idepth = idepth or maxdepth
	local ts = type(struct)
	name = name or ("["..ts.."]")
	prefix = prefix or ""
	if last == nil then last = true end
	if pt == nil   then pt = true   end
	if pv == nil   then pv = true   end
	if maxdepth < 0 then return end
	
	local glyphs = prefix .. (idepth == maxdepth and "" or ((last and term.box1ur or term.box1vr) .. term.arrowr))
	if ts ~= "table" then
		print(string.format('%s%s = %s', glyphs, name, tostring(struct)))
		local pp
		if pv then
			pp = process.pause()
		end
		return pp
	end
	
	print(glyphs .. name)
	if pt then
		local pp = process.pause()
		if pp == "q" or pp == "c" then
			return pp
		end
	end
	if maxdepth < 1 then return end
	
	prefix = prefix .. (last and --[[term.middledot]]" " or term.box1v) .. " "
	local remaining = table.len(table.where(struct, function(k,v) return v ~= struct end))
	for k, v in pairs(struct) do
		local pp
		if v ~= struct then
			remaining = remaining - 1
			pp = rPrint(v, maxdepth - 1, tostring(k), pt, pv, prefix, maxdepth, remaining == 0)
		end
		if pp == "c" then
			break
		end
	end
	print(prefix)
end
function makeRow(columns, align, sep)
	--[[rows is equal to
	{k° = {
		row = {"col1", "col2"},
		[align = {true, false}],
		[sep = {"_"}],
		},
		...
	}	
	]]
	local l = table.len(columns)
	return {
		row = columns,
		align = (type(align) == "boolean" and table.rep(align, l)) or align,
		sep = (type(sep) == "string" and table.rep(sep, l - 1)) or sep,
	}
end
function makeRows(rawrows, align, sep)
	return table.select(rawrows, function(k, v) return makeRow(type(v) == "table" and v or {tostring(k), v}, align, sep) end)
end
function buildText(rows, sep) --return lines
	if not rows then
		return ""
	end
	local ret = {}
	sep = sep or ""
	
	local entries = {}
	local cols = nil
	for ir, r in pairs(rows) do
		local l = #(r.row) 
		if cols == nil then
			cols = l
		elseif cols ~= l then
			error("Rows with different amount of columns")
		end
	end
	
	local maxWxC = {}
	for ir, r in pairs(rows) do
		for ic, c in pairs(r.row) do
			maxWxC[ic] = math.max(maxWxC[ic] or 0, #c)
		end
	end
	for ir, r in pairs(rows) do
		local line = nil
		for ic, c in pairs(r.row) do
			local alignright = false
			if r.align and r.align[ic] ~= nil then
				alignright = r.align[ic]
			end
			local txt = (alignright and string.padRight(c, maxWxC[ic]) or string.padLeft(c, maxWxC[ic]))
			if ic == 1 then
				line = txt
			else
				line = line .. (r.sep and r.sep[ic - 1] or sep) .. txt
			end
		end
		ret[#ret + 1] = line
	end
	return ret
end
function printBoxes(ignoreDetails, ...)
	ignoreDetails = ignoreDetails or false
	local strings = {...}
	if #strings == 0 then
		return ""
	end
	local blocks = {} --{{{"a","b"},w,h}, ...}
	for i, v in pairs(strings) do
		blocks[i] = type(v) == "string" and table.pack(string.lines(v)) or {v, string.maxWidth(v), #v}
	end
	local maxW = 0
	for i, t in pairs(blocks) do
		maxW = math.max(maxW, t[2])
	end
	
	local above, below, c_above, c_below
	below = blocks[1][1]
	c_below = string.chars(below[1])
	
	local function getLongLine()
		local ll = ""
		if ignoreDetails then
			ll = string.rep(term.box1h, maxW)
		else
			for i = 1, maxW do
				local cu = c_above and c_above[i] or ""
				local cd = c_below and c_below[i] or ""
				local uf = term.toBoxFlags(cu)
				local df = term.toBoxFlags(cd)
				local c = term.fromBoxFlags(term.toBoxFlags(term.box1h) | (uf & term.boxflag_u) | (df & term.boxflag_d))
				ll = ll .. c
			end
		end
		return ll
	end
	
	print(term.box1dr .. getLongLine() .. term.box1dl)
	for i = 1, #blocks do
		if i > 1 then
			below = blocks[i][1]
			c_below = string.chars(below[1])
			print(term.box1vr .. getLongLine() .. term.box1vl)
		end
		for _, l in pairs(below) do
			local marginL = term.box1v
			local marginR = term.box1v
			if not ignoreDetails then
				local cl = l[1]
				local cr = c_below[maxW]
				local lf = term.toBoxFlags(cl)
				local rf = term.toBoxFlags(cr)
				marginL = term.fromBoxFlags(term.toBoxFlags(term.box1v) | (lf & term.boxflag_l))
				marginR = term.fromBoxFlags(term.toBoxFlags(term.box1v) | (rf & term.boxflag_r))
			end
			print(marginL .. string.padLeft(l, maxW) .. marginR)
		end
		above = below
		c_above = string.chars(below[#below])
	end
	c_below = nil
	print(term.box1ur .. getLongLine() .. term.box1ul)
end


_PERSONAL_FRAMEWORK_ = 1
