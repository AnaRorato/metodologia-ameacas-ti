--------------------------------------------------------------
--          This file contains a Fill Cell Script           --
--               Compatible with LuccME 3.1                 --
--       Generated with Fill Cell Script Configurator       --
--               19/06/2025 at 15:13:26                     --
--------------------------------------------------------------

local x = os.clock()
import("gis")

local projFile = File("t3mp.tview")
if(projFile:exists()) then
	projFile:delete()
end

-- CREATING PROJECT --
print("-- Creating Project --\n")

proj = Project {
	file = "t3mp.tview",
	clean = true
}

-- ADDING LAYERS --
print("-- Adding Layers to the Project --")

l1 = Layer{
	project = proj,
	name = "limit",
	file = "C:\\Users\\ANA\\Dropbox\\Auditoria_TCU\\FAP_DF\\Dados\\buffer_zones_agrupamentos_indigenas\\buffer_0_5_km_agrupamentos_indigenas_BR_censo_2022_plus_POP_AMZL_new.shp"
}
print("Added Cellular Spaced: buffer_0_5_km_agrupamentos_indigenas_BR_censo_2022_plus_POP_AMZL_new.shp")

l2 = Layer{
	project = proj,
	name = "layer2",
	file = "D:\\Auditoria_TCU\\prodes_brasil_2023\\prodes_brasil_2023_policonic.tif"
}
print("Added Layer2: prodes_brasil_2023_policonic.tif")

-- Checking EPSGs --
print("\n-- Checking EPSGs--")
local epsgVector = {l1.epsg, l2.epsg}
local fileVector = {"buffer_0_5_km_agrupamentos_indigenas_BR_censo_2022_plus_POP_AMZL_new.shp", "prodes_brasil_2023_policonic.tif"}
local checkEPSG = true

for i = 1, #epsgVector, 1 do
	if (epsgVector[i] ~= l1.epsg) then
		print("Error: EPSG does not math - limit : "..l1.epsg.." "..fileVector[i].." : "..epsgVector[i])
		checkEPSG = false
	end

	if checkEPSG then print("EPSG - limit : "..l1.epsg.."\t"..fileVector[i]..": "..epsgVector[i]) end
end

if not checkEPSG then os.exit() end

-- Checking Shape Geometry --
print("\n-- Checking Geometries--")
l1:check()

-- OPENING CELLULAR SPACE --
print("\n-- Openning Cellular Space -- \n")
local cs = Layer{
	project = proj,
	name = l1.name,
}

-- FILLING CELLULAR SPACE --
print("Filling prodes_brasil_2023_policonic.tif into Cellular Space using coverage operation")
cs:fill{
	layer = "layer2",
	operation = "coverage",
	attribute = "def_",
	dummy = 88,
}

projFile = File("t3mp.tview")
if(projFile:exists()) then
	projFile:delete()
end

-- Calculating execution time --
local sTime = os.clock() - x
local days = math.floor(sTime / 86400)
local hours = math.floor(sTime % 86400 / 3600)
local minutes = math.floor(sTime % 3600 / 60)
local seconds = math.floor(sTime % 60)
if seconds < 59 then
	seconds = seconds + 1
end

print(string.format("\nElapsed time : %.2d : %.2d : %.2d hh : mm:ss", hours, minutes, seconds))
print("\nEnd of Script")

