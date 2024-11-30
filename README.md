# Unidad 4 - Actividad 1 
Mapa de Muertes Accidentales y Violentas el 2023. Clase Sistemas de Informacion Geografica.
# Codigo
``` r 
#instalar paquetes 
install.packages("ggplot2") 
install.packages("sf") 
install.packages("readr")
install.packages("dplyr")
install.packages("ggspatial")

# Cargar la bibliotecas 
library(sf) 
library(ggplot2) 
library(readr)
library(dplyr)
library(ggspatial)
# Cargar datos estadisticos
#url <- "https://www.one.gob.do/catalogo-datos/MAV-SUICIDIO/2007-2023/BD_SUICIDIOS_2007-2023.csv"  # Reemplaza esta URL con la URL del archivo CSV
archivo <- "archivo.csv"  # Especifica el nombre del archivo de destino
#download.file(url, archivo, mode = "wb")
datos <- read.csv(archivo, header = TRUE, sep=",", stringsAsFactors = FALSE)

# Cargar archivo shapefile
ruta_archivo <- "./RD_PROV/RD_PROV.shx"
mapard <- st_read(ruta_archivo)


datosFiltrado <- datos %>% 
  filter(MESOCU == 2023) %>% 
  group_by(PROVINCIA)  %>%
  summarise( count = n())
 
mapard$PROV <- as.numeric(mapard$PROV)

mapard <- mapard %>%
  left_join(datosFiltrado, by = c("PROV" = "PROVINCIA"))

  
plot <- ggplot(data = mapard) +
  geom_sf(aes_string(fill = "count"), color = "black") +
  geom_sf_text(
    aes_string(label = "TOPONIMIA"), 
    size = 3, color = "black", 
    check_overlap = TRUE) +
  scale_fill_gradient(
    name = paste("count", "(RD$)"),
    labels = function(x) { 
      ifelse(x >= 1e9, 
          paste0(round(x / 1e9, 1), " Mil Millones"), 
          ifelse(x >= 1e6, paste0(round(x / 1e6, 1), " Millones"), x)) 
    }, 
  low = "grey", high = "gold", na.value = "black"
  ) + 
    labs(
      title = "title",
      subtitle = "subtitle",
      caption = "caption"
    ) + 
    theme_light()
# Añadir escala y flecha norte 
plot + 
  annotation_scale(location = "br", width_hint = 0.5) + 
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.75, "in"), 
                         style = north_arrow_fancy_orienteering)
```