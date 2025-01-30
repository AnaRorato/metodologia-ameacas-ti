########################
#Projeto: Protegendo os Territórios Indígenas na Amazônia Brasileira: Uma Metodologia Espacial Baseada em Múltiplos Indicadores para Priorizar Áreas sob Pressão Ambiental
#Rorato et al., 2025.

#### Calculando os índices de sensibilidade ambiental
#### Componente de Sensibilidade
#### Ameaças no interior das TIS


# 1. Inicialização dos pacotes requeridos. 
#Caso o pacote ainda não esteja instalado: 1° install.packages("pacote_desejado") e 2º library("pacote_desejado")

library(openxlsx)
library(ggpmisc)
library(ggplot2)
library(ggpubr)
library(raster)
library(rgdal)
library(maptools)
library(spatstat)
library(rgeos)
library(ggmap)
library(sf)
library(ggspatial)
library(ggsci)
library(RColorBrewer)
library(maps)
library(grid)
library(cowplot)
library(MetBrewer)
library(raster)
library(tidyr)
library(dplyr)


#2. Carrega o diretório de trabalho
setwd("C:/Users/ANA/Dropbox/Auditoria_TCU/execucao/Resultados/Indice_sensibilidade")


#3.Carrega o shapefile com os dados das ameaças calculadas para as TIs

tis<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/DADOS/TIS/tis_amazonia_legal_poligonais/tis_amazonia_legal_poligonaisPolygon_polyconic_indicadores_nomes.shp")

#4. Verifica a estrutura do shapefile para entender como as colunas estão organizadas
head(tis) #mostra as 6 primeiras linhas do shapefile
colnames(tis)# mostra os nomes das colunas do shapefile

#5. Renomear as colunas que foram modificadas de acordo com os nomes pretendidos

colnames(tis)<-c("gid",  "terrai_cod" , "trr_nm_x", "etn_nm_x",  "mncp__x"  ,  "uf_sigla" ,   "superficie" ,   "fas_t_x" ,   "mdldd_x"  , 
                 "reestudo_t" ,   "cr_x"  ,     "undadm_cod" ,   "unddm_nm_x", "unddm_sg_x", "mp2010" ,   "mp2011" ,   "mp2012",    "mp2013" ,  
                 "mp2014" ,   "mp2015"  ,  "mp2016"  ,  "mp2017"  ,  "mp2018" ,   "mp2019" ,   "mp2020" ,   "mp2021" ,   "mp2022"  , 
                 "mp2023",    "dg2010" ,    "dg2011"  ,   "dg2012" ,    "dg2013" ,    "dg2014" ,    "dg2015" ,    "dg2016"  ,   "dg2017" ,   
                 "dg2018"  ,   "dg2019"  ,   "dg2020" ,    "dg2021" ,    "dg2022" ,    "dg2023" ,    "def_2007" ,     "def_2008"  ,    "def_2009"   ,  
                 "def_2010" ,    "def_2011" ,    "def_2012" ,    "def_2013" ,    "def_2014"  ,   "def_2015"  ,   "def_2016" ,    "def_2017"  ,   "def_2018"   , 
                 "def_2019" ,    "def_2020"  ,   "def_2021" ,    "def_2022" ,    "def_2023" ,    "fr_2010" ,   "fr_2011" ,   "fr_2012" ,   "fr_2013" ,  
                 "fr_2014" ,  "fr_2015" ,   "fr_2016" ,   "fr_2017"  ,  "fr_2018"  ,  "fr_2019"  ,  "fr_2020",    "fr_2021" ,   "fr_2022" ,  
                 "fr_2023" ,   "terrai_nom" ,  "etnia_nome" ,  "municipio" ,   "fase_ti",    "modalidade" ,   "cr" ,      "undadm_nom" ,"undadm_sig",
                 "geometry"  
)

colnames(tis)


#6. Cria um subconjunto com as colunas de interesse. 
# Neste caso, temos interesse nas colunas de identificação da TI e das variáveis de ameaças ambientais

colunas_desejadas<-c("gid", "terrai_cod",  "uf_sigla",   "superficie",
                     "mp2018" ,   "mp2019" ,   "mp2020" ,   "mp2021" ,   "mp2022"  , "mp2023",  
                     "dg2018"  ,   "dg2019"  ,   "dg2020" ,    "dg2021" ,    "dg2022" ,    "dg2023" , 
                     "def_2018"   , "def_2019" ,    "def_2020"  ,   "def_2021" ,    "def_2022" ,    "def_2023" ,   
                     "fr_2018"  ,  "fr_2019"  ,  "fr_2020",    "fr_2021" ,   "fr_2022" ,  "fr_2023" , 
                     "reestudo_t",  "undadm_cod", 
                     "terrai_nom", "etnia_nome", "municipio", "fase_ti", "modalidade", "cr", "undadm_nom", "undadm_sig", "geometry")


#7. Define o novo conjunto de dados apenas com as colunas desejadas 
ti <- tis[, colunas_desejadas]
colnames(ti)

#8. Cálculo dos indicadores de sensibilidade ambiental no período de 2018 a 2023. 
# As variáveis de % de área no shapefile foram calculados entre 0-1. (Já veio assim do FillCell - Etapa 01)

#9.Índice de sensibilidade à mineração (a varíavel calculada representa a % de área da TI com mineração no ano)
#### Calculando a % de área de mineração em 2023 

ti$se_mine<- c(ti$mp2023)

##### índice de sensibilidade à mineração normalizado entre 0-1

ti$se_minen <- c((ti$se_mine - min(ti$se_mine)) / (max(ti$se_mine) - min(ti$se_mine)))


#10.Índice de sensibilidade à degradação florestal 
#Soma das % de área com degradação florestal em cada ano

ti$se_degrad<-c(ti$dg2018 + ti$dg2019 + ti$dg2020 + ti$dg2021 + ti$dg2022 + ti$dg2023)


##### índice de sensibilidade à degradação florestal normalizado entre 0-1

ti$sedegradn <- c((ti$se_degrad - min(ti$se_degrad)) / (max(ti$se_degrad) - min(ti$se_degrad)))


#11. Índice de sensibilidade ao desmatamento 
#soma das % de área de incremento de desmatamento em cada ano

ti$se_defor<-c(ti$def_2018 + ti$def_2019 + ti$def_2020 + ti$def_2021 + ti$def_2022 + ti$def_2023)


##Índice de sensibilidade ao desmatamento normalizado entre 0-1

ti$sedeforn <- c((ti$se_defor - min(ti$se_defor)) / (max(ti$se_defor) - min(ti$se_defor)))


##12. Índice de sensibilidade ao fogo
#Calcular a densidade de focos de calor acumulados na TI no periodo de 2018 a 2023
# soma dos focos de calor no periodo/area da TI

ti$se_fogo<-c((ti$fr_2018 + ti$fr_2019 + ti$fr_2020 + ti$fr_2021 + ti$fr_2022 + ti$fr_2023)/ ti$superficie)

#### Índice de sensibilidade ao fogo normalizado entre 0 e 1

ti$sefogon <- c((ti$se_fogo - min(ti$se_fogo)) / (max(ti$se_fogo) - min(ti$se_fogo)))
               
  
########################### 13. Índice composto de sensibilidade ambiental 
### Soma dos produtos de cada indice de sensibilidade x o peso da ameaça atribuido por especialistas
### Pesos adaptados de Rorato et al, 2022

#### Pesos:
#defor = 0.282
#degrad = 0.261
#mine = 0.290
#fogo = 0.166

##### Índice composto de sensibilidade ambiental
ti$se_amb <- c((ti$sedeforn * 0.282) + (ti$sedegradn * 0.261)  + (ti$se_minen * 0.290) + (ti$sefogon * 0.166))

min(ti$se_amb)
max(ti$se_amb)

# 14. Para converter o dado em uma tabela e salvar como .XLSX
dados_tabela <- st_drop_geometry(ti)

write.xlsx(dados_tabela, "C:/Users/ANA/Dropbox/Auditoria_TCU/execucao/Resultados/Indice_sensibilidade/tis_amazonia_legal_indice_sensibilidade_ambiental.xlsx")



# 15.  Salvar o shapefile final com os indicadores de sensibilidade gerados
st_write(ti,"C:/Users/ANA/Dropbox/Auditoria_TCU/execucao/Resultados/Indice_sensibilidade/tis_amazonia_legal_indice_sensibilidade_ambiental.shp", layer_options = "ENCODING=UTF-8")



##############################################################################################################################
######################################## Análise dos resultados e produção de gráficos e tabelas dos rankings

### 1. Produção de mapas
# Produzindo os mapas com os índices de sensibilidade das TIS (índice composto e para cada ameaça)


### Carrega os layers dos mapas bases para o mapa (limites da Amozônia Legal e estados)

setwd("C:/Users/ANA/Dropbox/Auditoria_TCU/execucao/Resultados/Indice_sensibilidade")

amz<- st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/DADOS/limites_espaciais/amazlegal_sirgas_4326.shp")

states<- st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/DADOS/limites_espaciais/UFS_AMZLEG_Polyconic_p_mapa.shp")

hex11RdGn7<-  c('#67001f','#a50026','#d73027',  #red
                '#f46d43','#fdae61',            #orange
                '#ffe082', #fee090',  #yeloow
                '#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837', '#00441b') #greens



#### 2. Mapa do Índice composto de sensibilidade ambiental

p1<-  ggplot(data = ti) +
  geom_sf(aes(fill = se_amb), color = NA) +
  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Índice composto de sensibilidade ambiental (2018-2023)", trans = "sqrt",
#  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Composite Index of Environmental Sensitivity (2018–2023)", trans = "sqrt",
                       breaks = seq(min(ti$se_amb, na.rm = TRUE), max(ti$se_amb, na.rm = TRUE), length.out = 7),
                       labels = scales::label_number(decimal.mark = ".", accuracy = 0.01))  + # Formata as labels# Ajuste os valores conforme necessário) +
  guides(#fill = guide_colourbar(barwidth = unit( 2 , "in" ),
    #    ticks.colour = "black",
    #    ticks.linewidth = 1)) +
    fill = guide_colourbar(barwidth = unit( 3 , "in" ),
                           barheight = unit( 0.4 , "in" ),
                           ticks.colour = "black", 
                           ticks.linewidth = 0.8,
                           title.position = "bottom",
                           label.theme = element_text(angle = 0, size = 8),
                           title.theme = element_text(angle = 0, size = 12))) +
  geom_sf(data=states, fill = NA, color = 'gray30', size = 0.2)+
  geom_sf(data=amz, fill = NA, color = 'black', size = 0.3)+
  # addscalebar()+
  # annotation_scale(style = "ticks") +
  # annotation_north_arrow(location='tl', height = unit(1.0, "cm"), width = unit(1.0, "cm"), style = north_arrow_minimal) +
  annotation_scale(style = "ticks", location='bl', pad_x = unit(1, "cm"), pad_y = unit(0.50, "cm"), height = unit(0.4, "cm"), width = unit(1.8, "cm"), text_cex = 1 ) +
  annotation_north_arrow(location='tr',  height = unit(1.2, "cm"), width = unit(1.2, "cm"), style = north_arrow_minimal, pad_x = unit(1, "cm"), pad_y = unit(1, "cm")) +
  
  coord_sf(datum = NA) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = NA, fill=NA),
        legend.position="bottom"
  )

p1

ggsave("mapa_indice_sensibilidade_2018-2023_PORT.png", width = 6, height = 6, dpi = 400)



####3. Mapa do Índice de sensibilidade à mineração

p2<-  ggplot(data = ti) +
  geom_sf(aes(fill = se_minen), color = NA) +
#  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Índice de sensibilidade à mineração (2018-2023)", trans = "sqrt",
  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Sensitivity index to mining (2018-2023)", trans = "sqrt",
                       breaks = seq(min(ti$se_minen, na.rm = TRUE), max(ti$se_minen, na.rm = TRUE), length.out = 7),
                       labels = scales::label_number(decimal.mark = ".", accuracy = 0.01))  + # Formata as labels# Ajuste os valores conforme necessário) +
  guides(#fill = guide_colourbar(barwidth = unit( 2 , "in" ),
    #    ticks.colour = "black",
    #    ticks.linewidth = 1)) +
    fill = guide_colourbar(barwidth = unit( 3 , "in" ),
                           barheight = unit( 0.4 , "in" ),
                           ticks.colour = "black", 
                           ticks.linewidth = 0.8,
                           title.position = "bottom",
                           label.theme = element_text(angle = 0, size = 8),
                           title.theme = element_text(angle = 0, size = 12))) +
  geom_sf(data=states, fill = NA, color = 'gray30', size = 0.2)+
  geom_sf(data=amz, fill = NA, color = 'black', size = 0.3)+
  # addscalebar()+
  # annotation_scale(style = "ticks") +
  # annotation_north_arrow(location='tl', height = unit(1.0, "cm"), width = unit(1.0, "cm"), style = north_arrow_minimal) +
  annotation_scale(style = "ticks", location='bl', pad_x = unit(1, "cm"), pad_y = unit(0.50, "cm"), height = unit(0.4, "cm"), width = unit(1.8, "cm"), text_cex = 1 ) +
  annotation_north_arrow(location='tr',  height = unit(1.2, "cm"), width = unit(1.2, "cm"), style = north_arrow_minimal, pad_x = unit(1, "cm"), pad_y = unit(1, "cm")) +
  
  coord_sf(datum = NA) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = NA, fill=NA),
        legend.position="bottom"
  )

p2

ggsave("mapa_indice_sensibilidade_mineracao_2018-2023_ENG.png", width = 6, height = 6, dpi = 400)



####4. Mapa do Índice de sensibilidade ao desmatamento

ti$sedeforn
p3<-  ggplot(data = ti) +
  geom_sf(aes(fill = sedeforn), color = NA) +
#  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Índice de sensibilidade ao desmatamento (2018-2023)", trans = "sqrt",
  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Sensitivity index to deforestation (2018-2023)", trans = "sqrt",
                       breaks = seq(min(ti$sedeforn, na.rm = TRUE), max(ti$sedeforn, na.rm = TRUE), length.out = 7),
                       labels = scales::label_number(decimal.mark = ".", accuracy = 0.01))  + # Formata as labels# Ajuste os valores conforme necessário) +
  guides(#fill = guide_colourbar(barwidth = unit( 2 , "in" ),
    #    ticks.colour = "black",
    #    ticks.linewidth = 1)) +
    fill = guide_colourbar(barwidth = unit( 3 , "in" ),
                           barheight = unit( 0.4 , "in" ),
                           ticks.colour = "black", 
                           ticks.linewidth = 0.8,
                           title.position = "bottom",
                           label.theme = element_text(angle = 0, size = 8),
                           title.theme = element_text(angle = 0, size = 12))) +
  geom_sf(data=states, fill = NA, color = 'gray30', size = 0.2)+
  geom_sf(data=amz, fill = NA, color = 'black', size = 0.3)+
  # addscalebar()+
  # annotation_scale(style = "ticks") +
  # annotation_north_arrow(location='tl', height = unit(1.0, "cm"), width = unit(1.0, "cm"), style = north_arrow_minimal) +
  annotation_scale(style = "ticks", location='bl', pad_x = unit(1, "cm"), pad_y = unit(0.50, "cm"), height = unit(0.4, "cm"), width = unit(1.8, "cm"), text_cex = 1 ) +
  annotation_north_arrow(location='tr',  height = unit(1.2, "cm"), width = unit(1.2, "cm"), style = north_arrow_minimal, pad_x = unit(1, "cm"), pad_y = unit(1, "cm")) +
  
  coord_sf(datum = NA) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = NA, fill=NA),
        legend.position="bottom"
  )

p3

ggsave("mapa_indice_sensibilidade_desmatamento_2018-2023_ENG.png", width = 6, height = 6, dpi = 400)


####5. Mapa do Índice de sensibilidade à degradação florestal

ti$sedegradn
p4<-  ggplot(data = ti) +
  geom_sf(aes(fill = sedegradn), color = NA) +
#  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Índice de sensibilidade à degradação florestal (2018-2023)", trans = "sqrt",
  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Sensitivity index to forest degradation (2018-2023)", trans = "sqrt",
                       breaks = seq(min(ti$sedegradn, na.rm = TRUE), max(ti$sedegradn, na.rm = TRUE), length.out = 7),
                       labels = scales::label_number(decimal.mark = ".", accuracy = 0.01))  + # Formata as labels# Ajuste os valores conforme necessário) +
  guides(#fill = guide_colourbar(barwidth = unit( 2 , "in" ),
    #    ticks.colour = "black",
    #    ticks.linewidth = 1)) +
    fill = guide_colourbar(barwidth = unit( 3 , "in" ),
                           barheight = unit( 0.4 , "in" ),
                           ticks.colour = "black", 
                           ticks.linewidth = 0.8,
                           title.position = "bottom",
                           label.theme = element_text(angle = 0, size = 8),
                           title.theme = element_text(angle = 0, size = 12))) +
  geom_sf(data=states, fill = NA, color = 'gray30', size = 0.2)+
  geom_sf(data=amz, fill = NA, color = 'black', size = 0.3)+
  # addscalebar()+
  # annotation_scale(style = "ticks") +
  # annotation_north_arrow(location='tl', height = unit(1.0, "cm"), width = unit(1.0, "cm"), style = north_arrow_minimal) +
  annotation_scale(style = "ticks", location='bl', pad_x = unit(1, "cm"), pad_y = unit(0.50, "cm"), height = unit(0.4, "cm"), width = unit(1.8, "cm"), text_cex = 1 ) +
  annotation_north_arrow(location='tr',  height = unit(1.2, "cm"), width = unit(1.2, "cm"), style = north_arrow_minimal, pad_x = unit(1, "cm"), pad_y = unit(1, "cm")) +
  
  coord_sf(datum = NA) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = NA, fill=NA),
        legend.position="bottom"
  )

p4

ggsave("mapa_indice_sensibilidade_degradacao_2018-2023_ENG.png", width = 6, height = 6, dpi = 400)


####3. Mapa do Índice de sensibilidade ao fogo

ti$sefogon
p5<-  ggplot(data = ti) +
  geom_sf(aes(fill = sefogon), color = NA) +
#  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Índice de sensibilidade ao fogo (2018-2023)", trans = "sqrt",
  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Sensitivity index to fire (2018-2023)", trans = "sqrt",
                       breaks = seq(min(ti$sefogon, na.rm = TRUE), max(ti$sefogon, na.rm = TRUE), length.out = 7),
                       labels = scales::label_number(decimal.mark = ".", accuracy = 0.01))  + # Formata as labels# Ajuste os valores conforme necessário) +
  guides(#fill = guide_colourbar(barwidth = unit( 2 , "in" ),
    #    ticks.colour = "black",
    #    ticks.linewidth = 1)) +
    fill = guide_colourbar(barwidth = unit( 3 , "in" ),
                           barheight = unit( 0.4 , "in" ),
                           ticks.colour = "black", 
                           ticks.linewidth = 0.8,
                           title.position = "bottom",
                           label.theme = element_text(angle = 0, size = 8),
                           title.theme = element_text(angle = 0, size = 12))) +
  geom_sf(data=states, fill = NA, color = 'gray30', size = 0.2)+
  geom_sf(data=amz, fill = NA, color = 'black', size = 0.3)+
  # addscalebar()+
  # annotation_scale(style = "ticks") +
  # annotation_north_arrow(location='tl', height = unit(1.0, "cm"), width = unit(1.0, "cm"), style = north_arrow_minimal) +
  annotation_scale(style = "ticks", location='bl', pad_x = unit(1, "cm"), pad_y = unit(0.50, "cm"), height = unit(0.4, "cm"), width = unit(1.8, "cm"), text_cex = 1 ) +
  annotation_north_arrow(location='tr',  height = unit(1.2, "cm"), width = unit(1.2, "cm"), style = north_arrow_minimal, pad_x = unit(1, "cm"), pad_y = unit(1, "cm")) +
  
  coord_sf(datum = NA) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = NA, fill=NA),
        legend.position="bottom"
  )

p5

ggsave("mapa_indice_sensibilidade_fogo_2018-2023_ENG.png", width = 6, height = 6, dpi = 400)



#################################################################################################################
############## RANQUEAMENTO DAS CRS com base no índice composto de sensibilidade ambiental


#1. Agrupar os dados por CR e calcular a mediana do índice composto de sensibilidade ambiental por CR 
dados_cr_se <- ti %>%
  group_by(undadm_sig) %>%
  summarise(sensib = median(se_amb, na.rm = TRUE)) %>%   # usa a mediana do índice
  arrange(sensib)


###2. Grafico de barras

g_cr <- ggplot(dados_cr_se, aes(x = reorder(undadm_sig, -sensib), y = sensib)) +
  geom_bar(stat = "identity", fill = "#ea9999", color = "black", width = 0.7) +  # Barras com bordas e cor personalizada
  geom_text(aes(label = paste0(round(sensib, 2))), 
            vjust = -2, size = 2.5, color = "black") +  # Valores absolutos acima das barras
   labs(
    title =  "Composite Index of Environmental Sensitivity by CR (2018-2023)",
  #  title = "Índice composto de sensibilidade ambiental por CR (2018-2023)",
  #  x = "Coordenação Regional (CR)",
     x = "Regional Coordination (CR)",
  #  y = "I. composto de sensibilidade ambiental (mediana)"
     y = "Composite Index of Environmental Sensitivity (median)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),  # Centraliza e ajusta o título
    axis.title.x = element_text(size = 10, face = "bold"),  # Tamanho e negrito para o título do eixo X
    axis.title.y = element_text(size = 10, face = "bold"),  # Tamanho e negrito para o título do eixo Y
    axis.text.y = element_text(size = 8, hjust = 1),  # Ajusta o tamanho do texto no eixo Y
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Ajusta o tamanho do texto no eixo X com rotação
    panel.grid.major.y = element_line(color = "gray80"),  # Remove as linhas de grade horizontais principais
    panel.grid.major.x = element_line(),  # Mantém as linhas de grade verticais
    plot.background = element_blank(),  # Define o fundo como branco
    panel.background = element_blank()   # Define o fundo do painel como branco
  )

g_cr


ggsave("grafico_ranking_mediana_indice_sensibilidade_crs_ENG.png", plot = g_cr, width = 10, height = 8, dpi = 700, bg ="white")



####3.  Produzindo o mapa com base na mediana do índice composto de sensibilidade ambiental por CR 

library(MetBrewer)


p6<-  ggplot(data = dados_cr_se) +
  geom_sf(aes(fill = sensib), color = NA) +
#  scale_fill_gradientn(colours = rev(met.brewer("Hokusai1", n = 9)), name = "Composite Index of Environmental Sensitivity by CR (2018-2023)") +
  scale_fill_gradientn(colours = rev(met.brewer("Hokusai1", n = 9)), name = "Índice composto de sensibilidade ambiental por CR (2018-2023)") +
  guides(fill = guide_colourbar(barwidth = unit( 3 , "in" ),
                           barheight = unit( 0.4 , "in" ),
                           ticks.colour = "black", 
                           ticks.linewidth = 0.8,
                           title.position = "bottom",
                           label.theme = element_text(angle = 0, size = 8),
                           title.theme = element_text(angle = 0, size = 12))) +
  geom_sf(data=states, fill = NA, color = 'gray30', size = 0.2)+
  geom_sf(data=amz, fill = NA, color = 'black', size = 0.3)+
  annotation_scale(style = "ticks", location='bl', pad_x = unit(1, "cm"), pad_y = unit(0.50, "cm"), height = unit(0.4, "cm"), width = unit(1.8, "cm"), text_cex = 1 ) +
  annotation_north_arrow(location='tr',  height = unit(1.2, "cm"), width = unit(1.2, "cm"), style = north_arrow_minimal, pad_x = unit(1, "cm"), pad_y = unit(1, "cm")) +
  
  coord_sf(datum = NA) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = NA, fill=NA),
        legend.position="bottom"
  )

p6

ggsave("mapa_mediana_indice_sensibilidade_ambiental_2018-2023_PORT.png", width = 6, height = 6, dpi = 400)


#####################################################################################################
#### GERANDO TABELAS - RANKINGS DAS TIS

####### TABELAS dos rankings das TIS com base na mediana do indice de sensibilidade

# Carregar pacotes necessários
library(dplyr)

#SE

#1. Transformar o objeto 'sf' para um 'data.frame' removendo as colunas espaciais
tabela_cr_se <- st_drop_geometry(dados_cr_se)

#2. Criar a tabela de resumo e ordenar pela mediana do indice de sensibilidade
tabela_cr_se <- tabela_cr_se %>%
  select(undadm_sig, sensib) %>%  # Seleciona as colunas necessárias (undadm_sig é o identificador da CR)
  arrange(desc(sensib))  # Ordena pela mediana do indice de sensibilidade em ordem decrescente


library(openxlsx)
#3. Exportando a tabela para um arquivo CSV
write.xlsx(tabela_cr_se, "C:/Users/ANA/Dropbox/Auditoria_TCU/execucao/Resultados/Indice_sensibilidade/Tabela_ranking_CRs_mediana_indice_sensibilidade_ambiental_2018_2023.xlsx")

#############################################################################################################
################################################# Ranking das TIS de cada CR
### Um arquivo geral com a cada tabela de cada CR em uma planilha
# Carregar pacotes necessários
library(dplyr)
library(openxlsx)


#1. Gerar a tabela principal, agregando os dados
tabela_cr_se <- ti %>%
  st_drop_geometry() %>%  # Remove as informações geométricas
  group_by(undadm_sig, gid, terrai_nom) %>%  # Agrupa por CR, GID e Nome da Terra Indígena
  summarise(sensib = sum(se_amb, na.rm = TRUE)) %>%  # Soma o indice composto de sensibilidade por CR
  ungroup() %>%  # Remove o agrupamento
  group_by(undadm_sig) %>%  # Reagrupa apenas por CR para calcular os percentuais
  arrange(undadm_sig, desc(sensib))  # Ordena os dados por CR e pelo indice de sensibilidade decrescente

#2. Criar uma lista para armazenar tabelas por CR
lista_tabelas <- list()

#3. Iterar pelos CRs únicos e criar tabelas individuais
unique_cr <- unique(tabela_cr_se$undadm_sig)
for (cr in unique_cr) {
  tabela_cr <- tabela_cr_se %>%
    filter(undadm_sig == cr) %>%  # Filtrar dados para o CR específico
     arrange(desc(sensib))  # Ordenar por area desmatada
  
  # Adicionar a tabela à lista, usando o nome do CR como chave
  lista_tabelas[[cr]] <- tabela_cr
}

#4. Criar um arquivo Excel com todas as tabelas
caminho_arquivo <- "C:/Users/ANA/Dropbox/Auditoria_TCU/execucao/Resultados/Indice_sensibilidade/Tabelas_por_CR_indice_sensibilidade_das_TIs_2018-2023.xlsx"
wb <- createWorkbook()

#5. Adicionar cada tabela como uma aba no Excel
for (cr in names(lista_tabelas)) {
  addWorksheet(wb, sheetName = cr)  # Criar uma aba com o nome do CR
  writeData(wb, sheet = cr, lista_tabelas[[cr]])  # Adicionar os dados na aba
}

#6. Salvar o arquivo Excel
saveWorkbook(wb, file = caminho_arquivo, overwrite = TRUE)

#7. Mensagem final
cat("Todas as tabelas foram salvas no arquivo:", caminho_arquivo, "\n")

################################################################################################################
#########################Ranking geral das TIS 
#Tabela única com todas as TIS


#1. Criar a tabela geral com todas as TIs
tabela_completa_ti <- ti %>%
  st_drop_geometry() %>%  # Remove as informações geométricas
  group_by(gid, terrai_nom, undadm_sig) %>%  # Agrupa por GID, Nome da Terra Indígena e CR
  summarise(
    sensib = sum(se_amb, na.rm = TRUE),  # Soma do indice de sensbilidade
     ) %>% 
  ungroup() %>%  # Remove o agrupamento
    arrange(desc(sensib))  # Ordena pelo indice de sensibilidade em ordem decrescente

#2. Visualizar a tabela
print(tabela_completa_ti)

#3. Definir o nome do arquivo de saída
output_file <- "C:/Users/ANA/Dropbox/Auditoria_TCU/execucao/Resultados/Indice_sensibilidade/Tabela_Completa_por_TI_indice_sensibilidade_ambiental_2018_2023.xlsx"

#4. Salvar a tabela como arquivo Excel
write.xlsx(tabela_completa_ti, output_file)

#5. Mensagem de status
cat("Tabela completa salva em:", output_file, "\n")

#################################### FIM ##################################


