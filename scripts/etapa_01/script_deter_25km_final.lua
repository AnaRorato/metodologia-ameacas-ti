--------------------------------------------------------------
--          This file contains a Fill Cell Script           --
--               Compatible with LuccME 3.1                 --
--       Generated with Fill Cell Script Configurator       --
--               18/06/2025 at 18:48:30                     --
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
	file = "C:\\Users\\ANA\\Dropbox\\Auditoria_TCU\\FAP_DF\\Dados\\buffer_zones_agrupamentos_indigenas\\buffer_0_25_km_agrupamentos_indigenas_BR_censo_2022_plus_POP_AMZL_new.shp"
}
print("Added Cellular Spaced: buffer_0_25_km_agrupamentos_indigenas_BR_censo_2022_plus_POP_AMZL_new.shp")

l2 = Layer{
	project = proj,
	name = "layer2",
	file = "D:\\Auditoria_TCU\\DETER\\degradacao_amzl_2018_polyconic.tif"
}
print("Added Layer2: degradacao_amzl_2018_polyconic.tif")

l3 = Layer{
	project = proj,
	name = "layer3",
	file = "D:\\Auditoria_TCU\\DETER\\degradacao_amzl_2019_polyconic.tif"
}
print("Added Layer3: degradacao_amzl_2019_polyconic.tif")

l4 = Layer{
	project = proj,
	name = "layer4",
	file = "D:\\Auditoria_TCU\\DETER\\degradacao_amzl_2020_polyconic.tif"
}
print("Added Layer4: degradacao_amzl_2020_polyconic.tif")

l5 = Layer{
	project = proj,
	name = "layer5",
	file = "D:\\Auditoria_TCU\\DETER\\degradacao_amzl_2021_polyconic.tif"
}
print("Added Layer5: degradacao_amzl_2021_polyconic.tif")

l6 = Layer{
	project = proj,
	name = "layer6",
	file = "D:\\Auditoria_TCU\\DETER\\degradacao_amzl_2022_polyconic.tif"
}
print("Added Layer6: degradacao_amzl_2022_polyconic.tif")

l7 = Layer{
	project = proj,
	name = "layer7",
	file = "D:\\Auditoria_TCU\\DETER\\degradacao_amzl_2023_polyconic.tif"
}
print("Added Layer7: degradacao_amzl_2023_polyconic.tif")

-- Checking EPSGs --
print("\n-- Checking EPSGs--")
local epsgVector = {l1.epsg, l2.epsg, l3.epsg, l4.epsg, l5.epsg, l6.epsg, l7.epsg}
local fileVector = {"buffer_0_25_km_agrupamentos_indigenas_BR_censo_2022_plus_POP_AMZL_new.shp", "degradacao_amzl_2018_polyconic.tif", "degradacao_amzl_2019_polyconic.tif", "degradacao_amzl_2020_polyconic.tif", "degradacao_amzl_2021_polyconic.tif", "degradacao_amzl_2022_polyconic.tif", "degradacao_amzl_2023_polyconic.tif"}
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
print("Filling degradacao_amzl_2018_polyconic.tif into Cellular Space using coverage operation")
cs:fill{
	layer = "layer2",
	operation = "coverage",
	attribute = "dg2018",
	dummy = 88,
}

print("Filling degradacao_amzl_2019_polyconic.tif into Cellular Space using coverage operation")
cs:fill{
	layer = "layer3",
	operation = "coverage",
	attribute = "dg2019",
	dummy = 88,
}

print("Filling degradacao_amzl_2020_polyconic.tif into Cellular Space using coverage operation")
cs:fill{
	layer = "layer4",
	operation = "coverage",
	attribute = "dg2020",
	dummy = 88,
}

print("Filling degradacao_amzl_2021_polyconic.tif into Cellular Space using coverage operation")
cs:fill{
	layer = "layer5",
	operation = "coverage",
	attribute = "dg2021",
	dummy = 88,
}

print("Filling degradacao_amzl_2022_polyconic.tif into Cellular Space using coverage operation")
cs:fill{
	layer = "layer6",
	operation = "coverage",
	attribute = "dg2022",
	dummy = 88,
}

print("Filling degradacao_amzl_2023_polyconic.tif into Cellular Space using coverage operation")
cs:fill{
	layer = "layer7",
	operation = "coverage",
	attribute = "dg2023",
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

