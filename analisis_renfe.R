#PROCESAR

setwd("~/Cosas de estudiar/Cursos/Programación/Coursera/Google Data Analyst/Proyecto final")

#Librerias

install.packages("tidyverse")
library(tidyverse)
install.packages("janitor")
library(janitor)
install.packages("ggrepel")
library(ggrepel)

#Importar csv

listado_de_estaciones_con_servicio_de_atendo <- read_delim("datos/csv/listado-de-estaciones-con-servicio-de-atendo.csv", delim = ";", trim_ws = TRUE, col_types = cols(`Codigo de Estación` = col_integer())) %>%
  clean_names() %>%
  rename(codigo = codigo_de_estacion)
listado_de_estaciones_ave_ld_md <- read_delim("datos/csv/listado_completo_av_ld_md.csv", delim = ";", trim_ws = TRUE, locale = locale(decimal_mark = ","), col_types = cols(`CÓDIGO` = col_integer())) %>%
  clean_names()
estaciones <- read_delim("datos/csv/estaciones.csv", delim = ";", trim_ws = TRUE, col_types = cols(`CODIGO` = col_integer())) %>%
  clean_names()
distribucion_por_dicapacidad <- read_delim("datos/csv/distribucion-de-las-asistencias-por-tipo-de-discapacidad.csv", delim = ";", col_types = cols(`Persona mayor` = col_number(), `Persona con carrito de niño` = col_number(), `PMR miembros superiores/inferiores` = col_number(), Embarazadas = col_number(), `Discapacidad intelectual` = col_number(), `Discapacidad auditiva` = col_number(), `Dicapacidad visual` = col_number(), `Silla en plaza regular` = col_number(), `Silla en plaza H` = col_number(), `Personas con sordoceguera` = col_number(), `Otros PMR` = col_number()), locale = locale(decimal_mark = ","), trim_ws = TRUE) %>%
  clean_names()
indicadores_de_calidad_osp <- read_delim("datos/csv/indicadores-de-calidad-osp.csv", delim = ";", locale = locale(decimal_mark = ","), trim_ws = TRUE) %>%
  clean_names()
indicadores_de_calidad_sc <- read_delim("datos/csv/indicadores-de-calidad-servicios-comerciales.csv", delim = ";", locale = locale(decimal_mark = ","), trim_ws = TRUE) %>%
  clean_names()
numero_de_asistencias <- read_delim("datos/csv/numero-de-asistencias-del-servicio-atendo.csv", delim = ";", trim_ws = TRUE) %>%
  clean_names()
numero_de_empleados <- read_delim("datos/csv/numero-de-empleados.csv", delim = ";", trim_ws = TRUE) %>%
  clean_names() %>%
  rename(ano = anos)
indicadores_de_calidad_sc <- read_delim("datos/csv/indicadores-de-calidad-servicios-comerciales.csv", delim = ";", locale = locale(decimal_mark = ","), trim_ws = TRUE) %>%
  clean_names()
indicadores_economicos <- read_delim("datos/csv/principales-indicadores-economicos.csv", delim = ";", locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE) %>%
  clean_names()
indicadores_sociales <- read_delim("datos/csv/principales-indicadores-sociales.csv", delim = ";", locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE) %>%
  clean_names()

#Crear una sola tabla de datos anuales

datos_anuales <- full_join(numero_de_asistencias, numero_de_empleados, by = NULL, copy = FALSE) %>%
  full_join(distribucion_por_dicapacidad, by = NULL, copy = FALSE) %>%
  full_join(indicadores_de_calidad_osp, by = NULL, copy = FALSE) %>%
  full_join(indicadores_de_calidad_sc, by = NULL, copy = FALSE) %>%
  full_join(indicadores_economicos, by = NULL, copy = FALSE) %>%
  full_join(indicadores_sociales, by = NULL, copy = FALSE)

#ANALIZAR

#Crear tabla con tipo de estaciones atendo

estaciones_atendo <- left_join(listado_de_estaciones_con_servicio_de_atendo, listado_de_estaciones_ave_ld_md, by = NULL, copy = FALSE) %>%
  add_column("tipo_estacion" = "Regional") %>%
  clean_names() %>%
  rows_update(tibble(pais = "España", tipo_estacion = "Nacional"))

#Selección de datos para usar

datos_atendo_elegidos <- select(estaciones_atendo, c(codigo, nombre_de_la_estacion, tiempo_de_antelacion, tipo_estacion))
datos_anuales_elegidos <- select(datos_anuales, c(ano, no_de_asistencias_renfe_atendo, no_de_empleados, viajeros_millones, persona_mayor, persona_con_carrito_de_nino, pmr_miembros_superiores_inferiores, embarazadas, discapacidad_intelectual, discapacidad_auditiva, dicapacidad_visual, silla_en_plaza_regular, silla_en_plaza_h, personas_con_sordoceguera, otros_pmr)) %>%
  rename(n_asistencias = no_de_asistencias_renfe_atendo, n_empleados = no_de_empleados, n_viajeros = viajeros_millones, carrito = persona_con_carrito_de_nino, pmr_miembros = pmr_miembros_superiores_inferiores, disc_intelectual = discapacidad_intelectual, disc_auditiva = discapacidad_auditiva, disc_visual = dicapacidad_visual, sordoceguera = personas_con_sordoceguera) %>%
  mutate(tibble(n_viajeros = n_viajeros * 1e+06, persona_mayor = persona_mayor / 100, carrito = carrito / 100, pmr_miembros = pmr_miembros / 100, embarazadas = embarazadas /100, disc_intelectual = disc_intelectual / 100, disc_auditiva = disc_auditiva / 100, disc_visual = disc_visual / 100, silla_en_plaza_regular = silla_en_plaza_regular / 100, silla_en_plaza_h = silla_en_plaza_h / 100, sordoceguera = sordoceguera / 100, otros_pmr = otros_pmr / 100))

#Guardar estas tablas para el md
write.csv(datos_atendo_elegidos, file = "datos_atendo_elegidos.csv")
write.csv(datos_anuales_elegidos, file = "datos_anuales_elegidos.csv")

#Creación de los gráficos

#Mapa de estaciones

ggplot(listado_de_estaciones_ave_ld_md, aes(longitud, latitud)) + geom_point(colour = "grey50") + coord_quickmap() + labs(title = "Mapa estaciones Renfe en España")
ggsave("Mapa_estaciones_Renfe.png")

#Gráfico circular con proporción de estaciones

n_sin_atendo <- ((estaciones %>% count()) - (estaciones_atendo %>% count()))[1,1]
pie_chart <- datos_atendo_elegidos %>% count(tiempo_de_antelacion) %>%
  add_row(tiempo_de_antelacion = "Sin Atendo", n = n_sin_atendo)

ggplot(pie_chart, aes(x = "", y = n , fill = tiempo_de_antelacion)) +
  geom_bar(stat="identity", width = 2) +
  geom_label_repel(aes(label = n), min.segment.length = Inf) +
  guides(fill = guide_legend(override.aes = aes(label = "", color = NA, size = 0))) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Proporción de estaciones", fill = "Tiempo de antelación")
ggsave("proporcion_de_estaciones.png")

#Gráficos de barras por discapacidad

datos_graficos <- select(datos_anuales_elegidos, c(ano, n_asistencias, persona_mayor, carrito, pmr_miembros, embarazadas, disc_intelectual, disc_auditiva, disc_visual, silla_en_plaza_regular, silla_en_plaza_h, sordoceguera, otros_pmr)) %>% 
  mutate(tibble("persona mayor" = persona_mayor * n_asistencias, carrito = carrito * n_asistencias, "pmr miembros" = pmr_miembros * n_asistencias, embarazadas = embarazadas * n_asistencias, "disc. intelectual" = disc_intelectual * n_asistencias, "disc. auditiva" = disc_auditiva * n_asistencias, "disc. visual" = disc_visual * n_asistencias, "silla en plaza regular" = silla_en_plaza_regular * n_asistencias, "silla en plaza h" = silla_en_plaza_h * n_asistencias, sordoceguera = sordoceguera * n_asistencias, "otros pmr" = otros_pmr * n_asistencias))
  datos_graficos <- select(datos_graficos, c(ano, "persona mayor", carrito, "pmr miembros", embarazadas, "disc. intelectual", "disc. auditiva", "disc. visual", "silla en plaza regular", "silla en plaza h", sordoceguera, "otros pmr"))
datos_graficos %>% 
  pivot_longer(cols=-c(ano)) %>% 
  ggplot(aes(x=ano, y=value, fill=name, group=name)) +
  stat_summary(geom="col") +
  xlab("Años") + ylab("Asistencias") +
  facet_wrap(~name, scales = "free") +
  labs(title = "Cantidad de asistencias por discapacidad", fill = "Tipo de discapacidad")
ggsave("cantidad_de_asistencias_por_discapacidad.png")

#Gráfico de áreas

datos_graficos %>% 
  pivot_longer(cols=-c(ano)) %>% 
  ggplot(aes(x=ano, y=value, fill = name, label = name)) +
  geom_area(alpha = 0.6 , size = 0.5, colour="black") +
  xlab("Años") + ylab("Asistencias") +
  labs(title = "Tendencia de asistencias por discapacidad", fill = "Tipo de discapacidad")
ggsave("tendencia_de_asistencias_por_discapacidad.png")
