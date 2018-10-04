local framework, loadError = loadfile("personalFramework.lua")
if loadError then
	error(loadError)
end
framework()

term.clear()
local table1 = {
	{a=1, b=2, c={1,2}},
	{a=2, b=3, c={5,7}},
	{a=2, b=5, c={8,9}},
}
local table2 = table.blend(table1)
rPrint(table2, 10, "Blended table")

local str1 = "This statement will not be shown, "
local str2 = "but this statement will."
print(str1..str2)
term.moveCursor(-1, 0)
print(string.rep(" ", #str1))
