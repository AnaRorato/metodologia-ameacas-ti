--------------------------------------------------------------
--          This file contains a Fill Cell Script           --
--               Compatible with LuccME 3.1                 --
--       Generated with Fill Cell Script Configurator       --
--               19/06/2025 at 09:55:11                     --
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
	file = "C:\\Users\\ANA\\Dropbox\\Auditoria_TCU\\FAP_DF\\Dados\\buffer_zones_agrupamentos_indigenas\\buffer_0_25_km_agrupamentos_indigenas_BR_censo_2022_plus_POP_AMZL_new_4326.shp"
}
print("Added Cellular Spaced: buffer_0_25_km_agrupamentos_indigenas_BR_censo_2022_plus_POP_AMZL_new_4326.shp")

l2 = Layer{
	project = proj,
	name = "layer2",
	file = "C:\\Users\\ANA\\Dropbox\\Auditoria_TCU\\DADOS\\dados_ambientais\\BD_QUEIMADAS\\2018\\focos_qmd_inpe_2018-01-01_2018-12-31_06.575501\\focos_qmd_inpe_2018-01-01_2018-12-31_06.shp"
}
print("Added Layer2: focos_qmd_inpe_2018-01-01_2018-12-31_06.shp")

l3 = Layer{
	project = proj,
	name = "layer3",
	file = "C:\\Users\\ANA\\Dropbox\\Auditoria_TCU\\DADOS\\dados_ambientais\\BD_QUEIMADAS\\2019\\focos_qmd_inpe_2019-01-01_2019-12-31_20.215518\\focos_qmd_inpe_2019-01-01_2019-12-31_20.shp"
}
print("Added Layer3: focos_qmd_inpe_2019-01-01_2019-12-31_20.shp")

l4 = Layer{
	project = proj,
	name = "layer4",
	file = "C:\\Users\\ANA\\Dropbox\\Auditoria_TCU\\DADOS\\dados_ambientais\\BD_QUEIMADAS\\2020\\focos_qmd_inpe_2020-01-01_2020-12-31_12.163969\\focos_qmd_inpe_2020-01-01_2020-12-31_12.shp"
}
print("Added Layer4: focos_qmd_inpe_2020-01-01_2020-12-31_12.shp")

l5 = Layer{
	project = proj,
	name = "layer5",
	file = "C:\\Users\\ANA\\Dropbox\\Auditoria_TCU\\DADOS\\dados_ambientais\\BD_QUEIMADAS\\2021\\focos_qmd_inpe_2021-01-01_2021-12-31_27.155290\\focos_qmd_inpe_2021-01-01_2021-12-31_27.shp"
}
print("Added Layer5: focos_qmd_inpe_2021-01-01_2021-12-31_27.shp")

l6 = Layer{
	project = proj,
	name = "layer6",
	file = "C:\\Users\\ANA\\Dropbox\\Auditoria_TCU\\DADOS\\dados_ambientais\\BD_QUEIMADAS\\2022\\focos_qmd_inpe_2022-01-01_2022-12-31_39.966629\\focos_qmd_inpe_2022-01-01_2022-12-31_39.shp"
}
print("Added Layer6: focos_qmd_inpe_2022-01-01_2022-12-31_39.shp")

l7 = Layer{
	project = proj,
	name = "layer7",
	file = "C:\\Users\\ANA\\Dropbox\\Auditoria_TCU\\DADOS\\dados_ambientais\\BD_QUEIMADAS\\2023\\focos_qmd_inpe_2023-01-01_2023-12-31_12.800929\\focos_qmd_inpe_2023-01-01_2023-12-31_12.shp"
}
print("Added Layer7: focos_qmd_inpe_2023-01-01_2023-12-31_12.shp")

-- Checking EPSGs --
print("\n-- Checking EPSGs--")
local epsgVector = {l1.epsg, l2.epsg, l3.epsg, l4.epsg, l5.epsg, l6.epsg, l7.epsg}
local fileVector = {"buffer_0_25_km_agrupamentos_indigenas_BR_censo_2022_plus_POP_AMZL_new_4326.shp", "focos_qmd_inpe_2018-01-01_2018-12-31_06.shp", "focos_qmd_inpe_2019-01-01_2019-12-31_20.shp", "focos_qmd_inpe_2020-01-01_2020-12-31_12.shp", "focos_qmd_inpe_2021-01-01_2021-12-31_27.shp", "focos_qmd_inpe_2022-01-01_2022-12-31_39.shp", "focos_qmd_inpe_2023-01-01_2023-12-31_12.shp"}
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
l2:check()
l3:check()
l4:check()
l5:check()
l6:check()
l7:check()

-- OPENING CELLULAR SPACE --
print("\n-- Openning Cellular Space -- \n")
local cs = Layer{
	project = proj,
	name = l1.name,
}

-- FILLING CELLULAR SPACE --
print("Filling focos_qmd_inpe_2018-01-01_2018-12-31_06.shp into Cellular Space using count operation")
cs:fill{
	layer = "layer2",
	operation = "count",
	attribute = "fire_2018",
	dataType = "dot",
}

print("Filling focos_qmd_inpe_2019-01-01_2019-12-31_20.shp into Cellular Space using count operation")
cs:fill{
	layer = "layer3",
	operation = "count",
	attribute = "fire_2019",
	dataType = "dot",
}

print("Filling focos_qmd_inpe_2020-01-01_2020-12-31_12.shp into Cellular Space using count operation")
cs:fill{
	layer = "layer4",
	operation = "count",
	attribute = "fire_2020",
	dataType = "dot",
}

print("Filling focos_qmd_inpe_2021-01-01_2021-12-31_27.shp into Cellular Space using count operation")
cs:fill{
	layer = "layer5",
	operation = "count",
	attribute = "fire_2021",
	dataType = "dot",
}

print("Filling focos_qmd_inpe_2022-01-01_2022-12-31_39.shp into Cellular Space using count operation")
cs:fill{
	layer = "layer6",
	operation = "count",
	attribute = "fire_2022",
	dataType = "dot",
}

print("Filling focos_qmd_inpe_2023-01-01_2023-12-31_12.shp into Cellular Space using count operation")
cs:fill{
	layer = "layer7",
	operation = "count",
	attribute = "fire_2023",
	dataType = "dot",
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

