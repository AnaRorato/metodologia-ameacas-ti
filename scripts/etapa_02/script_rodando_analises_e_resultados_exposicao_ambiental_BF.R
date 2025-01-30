########################
#Projeto: Protegendo os Territórios Indígenas na Amazônia Brasileira: Uma Metodologia Espacial Baseada em Múltiplos Indicadores para Priorizar Áreas sob Pressão Ambiental
#Rorato et al., 2025.


#### ANALISANDO OS RESULTADOS
####Componente de Exposição
# Buffer zone (BF) de 10km no entorno das TIS 


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
library(tidyr)
library(dplyr)
library(sp)

#2. carrega o diretório de trabalho
setwd("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km")

#3.Carrega o shapefile com os dados.

tis<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km/resultados/buffer_tis_bt_10km_amazonia_legal_poligonais_policonic_analises_indicadores_exposicao.shp")


########################## Criando mapas classificados com os índices de exposição ambiental por TI

# Limite da Amazônia Legal
amz<- st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/DADOS/limites_espaciais/amazlegal_sirgas_4326.shp")

# Limites dos estados 
states<- st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/DADOS/limites_espaciais/UFS_AMZLEG_Polyconic_p_mapa.shp")

# Paleta de cores

hex11RdGn7<-  c('#67001f','#a50026','#d73027',  #red
                '#f46d43','#fdae61',            #orange
                '#ffe082', #fee090',  #yeloow
                '#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837', '#00441b') #greens



## FOGO

setwd("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km/resultados/fogo/")


# Indicador foi calculado da seguinte forma: Soma dos focos totais no BF no periodo / pela area do BF


p1<-  ggplot(data = ti) +
  geom_sf(aes(fill = exfiren), color = NA) +
#  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Índice de exposição ao fogo (2018-2023)",   trans = "sqrt") +
  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Exposure index to fire (2018-2023)",   trans = "sqrt") +
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
        legend.position="bottom", 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5)  # Reduz o tamanho do título do gráfico
  )


p1

ggsave("Indice_exposicao_fogo_2018-2023_ENG.png", width = 6, height = 6, dpi = 400)



## DESMATAMENTO

setwd("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km/resultados/desmatamento")

ti$exdeforn
p2<-  ggplot(data = ti) +
  geom_sf(aes(fill = exdeforn), color = NA) +
   scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Índice de exposição ao desmatamento (2018-2023)",   trans = "sqrt") +
 # scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Exposure index to deforestation (2018-2023)",   trans = "sqrt") +
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
        legend.position="bottom", 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5)  # Reduz o tamanho do título do gráfico
  )


p2

ggsave("Indice_exposicao_defor_2018-2023.png", width = 6, height = 6, dpi = 400)


## DEGRADAÇÃO FLORESTAL

setwd("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km/resultados/degradacao")

ti$exdegran
p3<-  ggplot(data = ti) +
  geom_sf(aes(fill = exdegran), color = NA) +
#  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Índice de exposição à degradação florestal (2018-2023)",   trans = "sqrt") +
  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Exposure index to forest degradation (2018-2023)",   trans = "sqrt") +
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
        legend.position="bottom", 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5)  # Reduz o tamanho do título do gráfico
  )


p3

ggsave("Indice_exposicao_degrad_2018-2023_ENG.png", width = 6, height = 6, dpi = 400)


## MINERAÇÃO

setwd("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km/resultados/mineracao")

ti$exminen
p4<-  ggplot(data = ti) +
  geom_sf(aes(fill = exminen), color = NA) +
    scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Índice de exposição à mineração (2018-2023)",   trans = "sqrt") +
 #scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Exposure index to mining (2018-2023)",   trans = "sqrt") +
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
        legend.position="bottom", 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5)  # Reduz o tamanho do título do gráfico
  )


p4

ggsave("Indice_exposicao_mining_2018-2023.png", width = 6, height = 6, dpi = 400)



###### Índice composto de exposição ambiental
### Soma dos produtos de cada indice de exposição x o peso da ameaça atribuído por especialistas

setwd("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km/resultados")

ti$ex_amb
p5<-  ggplot(data = ti) +
  geom_sf(aes(fill = ex_amb), color = NA) +
 # scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Índice composto de exposição ambiental (2018-2023)",   trans = "sqrt") +
  scale_fill_gradientn(colours = rev(hex11RdGn7), name = "Composite Index of Environmental Exposure (2018-2023)",   trans = "sqrt") +
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
        legend.position="bottom", 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5)  # Reduz o tamanho do título do gráfico
  )


p5

ggsave("Indice_exposicao_composto_2018-2023_ENG.png", width = 6, height = 6, dpi = 400)

####### FIM
