
###############################################################
# ANALISIS ESTADISTICO - PARASITOS GASTROINTESTINALES EN EQUINOS
# Tema: Infeccion por Strongylus spp. en equinos
#
# Este script fue preparado para que los estudiantes puedan:
# 1. Importar la base teorica en Excel.
# 2. Describir la poblacion equina evaluada.
# 3. Calcular la prevalencia de infeccion por Strongylus spp.
# 4. Generar graficos utiles para el informe.
# 5. Evaluar asociaciones entre factores de riesgo y la infeccion.
# 6. Calcular Odds Ratio crudos.
# 7. Construir un modelo de regresion logistica multivariada.
# 8. Exportar tablas y graficos en una carpeta de resultados.
#
# Articulo de referencia:
# Singh G, Singh NK, Singh H, Rath SS.
# Assessment of risk factors associated with prevalence of strongyle infection
# in equines from Central Plain Zone, Punjab.
#
# IMPORTANTE:
# La base de datos es teorica/simulada con fines docentes, pero fue construida
# siguiendo la estructura y hallazgos generales del articulo de referencia:
# - 311 equinos evaluados.
# - Variable principal: strongylus_positivo (Yes/No).
# - Prevalencia esperada cercana a 27.33%.
# - Mayor positividad en mulas que en caballos.
###############################################################

###############################################################
# 0. INSTALAR Y CARGAR PAQUETES
###############################################################

# Esta funcion instala paquetes solo si no estan instalados.
instalar_si_falta <- function(paquete) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete, dependencies = TRUE)
  }
}

paquetes <- c(
  "readxl",     # Leer archivos Excel
  "dplyr",      # Manipulacion de datos
  "ggplot2",    # Graficos
  "janitor",    # Tablas de frecuencia faciles
  "broom",      # Ordenar resultados de modelos estadisticos
  "writexl",    # Exportar tablas a Excel
  "forcats",    # Ordenar factores
  "stringr"     # Manejo de texto
)

invisible(lapply(paquetes, instalar_si_falta))
invisible(lapply(paquetes, library, character.only = TRUE))

###############################################################
# 1. DEFINIR ARCHIVOS Y CARPETAS DE TRABAJO
###############################################################

# Colocar este script en la misma carpeta que:
# base_parasitos_equinos_teorica.xlsx

archivo_excel <- "base_parasitos_equinos_teorica.xlsx"

# Carpeta donde se guardaran todos los resultados.
dir.create("resultados_parasitos_equinos", showWarnings = FALSE)
dir.create("resultados_parasitos_equinos/graficos", showWarnings = FALSE)
dir.create("resultados_parasitos_equinos/tablas", showWarnings = FALSE)

###############################################################
# 2. IMPORTAR LA BASE DE DATOS
###############################################################

datos <- read_excel(archivo_excel, sheet = "base_datos") %>%
  clean_names()

# Visualizar las primeras filas.
head(datos)

# Revisar estructura de la base.
str(datos)

###############################################################
# 3. PREPARACION DE VARIABLES
###############################################################

# Convertimos las variables categoricas en factores.
# Esto es importante porque R debe reconocerlas como categorias
# y no como texto suelto.

datos <- datos %>%
  mutate(
    especie = factor(especie, levels = c("Horse", "Mule")),
    sexo = factor(sexo, levels = c("Male", "Female")),
    grupo_edad = factor(grupo_edad, levels = c("1-4 years", "5-8 years", ">=8 years")),
    distrito = factor(distrito),
    estacion = factor(estacion, levels = c("Winter", "Summer", "Monsoon")),
    tipo_manejo = factor(tipo_manejo, levels = c("Organized", "Unorganized")),
    frecuencia_desparasitacion = factor(
      frecuencia_desparasitacion,
      levels = c("Regular", "Irregular", "Never")
    ),
    alojamiento = factor(alojamiento, levels = c("Stable", "Mixed", "Pasture")),
    condicion_corporal = factor(condicion_corporal, levels = c("Good", "Moderate", "Low")),
    strongylus_positivo = factor(strongylus_positivo, levels = c("No", "Yes")),
    intensidad_infeccion = factor(
      intensidad_infeccion,
      levels = c("Negative", "Rare", "Mild", "Moderate")
    ),
    tipo_strongylus = factor(tipo_strongylus)
  )

# Crear una variable numerica binaria para algunos analisis.
# 1 = positivo, 0 = negativo.
datos <- datos %>%
  mutate(strongylus_binario = ifelse(strongylus_positivo == "Yes", 1, 0))

###############################################################
# 4. OBJETIVO 1
# Caracterizar la poblacion equina evaluada
###############################################################

# Estadisticos descriptivos para edad y HPG.
descriptivos_numericos <- datos %>%
  summarise(
    n = n(),
    edad_media = mean(edad_anios, na.rm = TRUE),
    edad_sd = sd(edad_anios, na.rm = TRUE),
    edad_min = min(edad_anios, na.rm = TRUE),
    edad_max = max(edad_anios, na.rm = TRUE),
    hpg_media = mean(hpg, na.rm = TRUE),
    hpg_sd = sd(hpg, na.rm = TRUE),
    hpg_min = min(hpg, na.rm = TRUE),
    hpg_max = max(hpg, na.rm = TRUE)
  )

descriptivos_numericos

# Tablas de frecuencia para variables categoricas.
tabla_especie <- tabyl(datos, especie)
tabla_sexo <- tabyl(datos, sexo)
tabla_grupo_edad <- tabyl(datos, grupo_edad)
tabla_distrito <- tabyl(datos, distrito)
tabla_estacion <- tabyl(datos, estacion)
tabla_manejo <- tabyl(datos, tipo_manejo)
tabla_desparasitacion <- tabyl(datos, frecuencia_desparasitacion)
tabla_alojamiento <- tabyl(datos, alojamiento)
tabla_condicion <- tabyl(datos, condicion_corporal)

# Exportar descriptivos.
write_xlsx(
  list(
    descriptivos_numericos = descriptivos_numericos,
    especie = tabla_especie,
    sexo = tabla_sexo,
    grupo_edad = tabla_grupo_edad,
    distrito = tabla_distrito,
    estacion = tabla_estacion,
    manejo = tabla_manejo,
    desparasitacion = tabla_desparasitacion,
    alojamiento = tabla_alojamiento,
    condicion_corporal = tabla_condicion
  ),
  "resultados_parasitos_equinos/tablas/01_descriptivos.xlsx"
)

###############################################################
# GRAFICOS DEL OBJETIVO 1
###############################################################

# Grafico 1: distribucion de la edad.
g_edad <- ggplot(datos, aes(x = edad_anios)) +
  geom_histogram(bins = 15, color = "black") +
  labs(
    title = "Distribucion de la edad de los equinos",
    x = "Edad en anios",
    y = "Numero de equinos"
  ) +
  theme_minimal()

ggsave(
  "resultados_parasitos_equinos/graficos/01_histograma_edad.png",
  g_edad,
  width = 8,
  height = 5,
  dpi = 300
)

# Grafico 2: distribucion por especie.
g_especie <- ggplot(datos, aes(x = especie)) +
  geom_bar(color = "black") +
  labs(
    title = "Distribucion de equinos segun especie",
    x = "Especie",
    y = "Numero de animales"
  ) +
  theme_minimal()

ggsave(
  "resultados_parasitos_equinos/graficos/02_barras_especie.png",
  g_especie,
  width = 7,
  height = 5,
  dpi = 300
)

# Grafico 3: distribucion por estacion.
g_estacion <- ggplot(datos, aes(x = estacion)) +
  geom_bar(color = "black") +
  labs(
    title = "Distribucion de muestras segun estacion",
    x = "Estacion",
    y = "Numero de muestras"
  ) +
  theme_minimal()

ggsave(
  "resultados_parasitos_equinos/graficos/03_barras_estacion.png",
  g_estacion,
  width = 7,
  height = 5,
  dpi = 300
)

###############################################################
# 5. OBJETIVO 2
# Determinar la prevalencia general de infeccion por Strongylus spp.
###############################################################

tabla_prevalencia <- datos %>%
  summarise(
    total = n(),
    positivos = sum(strongylus_positivo == "Yes"),
    negativos = sum(strongylus_positivo == "No"),
    prevalencia = positivos / total,
    prevalencia_porcentaje = prevalencia * 100
  )

tabla_prevalencia

# Intervalo de confianza al 95% para la prevalencia.
# prop.test calcula un intervalo de confianza para una proporcion.
ic_prev <- prop.test(
  x = tabla_prevalencia$positivos,
  n = tabla_prevalencia$total
)

ic_prevalencia <- data.frame(
  prevalencia = tabla_prevalencia$prevalencia_porcentaje,
  ic95_inferior = ic_prev$conf.int[1] * 100,
  ic95_superior = ic_prev$conf.int[2] * 100,
  p_value = ic_prev$p.value
)

ic_prevalencia

write_xlsx(
  list(
    prevalencia = tabla_prevalencia,
    ic95_prevalencia = ic_prevalencia
  ),
  "resultados_parasitos_equinos/tablas/02_prevalencia_general.xlsx"
)

# Grafico 4: positivos y negativos.
g_prev <- ggplot(datos, aes(x = strongylus_positivo)) +
  geom_bar(color = "black") +
  labs(
    title = "Prevalencia de infeccion por Strongylus spp.",
    x = "Resultado coproparasitologico",
    y = "Numero de equinos"
  ) +
  theme_minimal()

ggsave(
  "resultados_parasitos_equinos/graficos/04_prevalencia_strongylus.png",
  g_prev,
  width = 7,
  height = 5,
  dpi = 300
)

###############################################################
# 6. FUNCIONES PARA ANALISIS BIVARIADO
###############################################################

# Esta funcion:
# - crea una tabla cruzada entre una variable explicativa y Strongylus.
# - calcula chi-cuadrado o Fisher segun corresponda.
# - entrega porcentajes por fila.
#
# Nota:
# Chi-cuadrado se usa cuando las frecuencias esperadas son adecuadas.
# Fisher se usa cuando hay celdas con frecuencias esperadas pequeñas.

analisis_categorico <- function(variable) {

  tabla <- table(datos[[variable]], datos$strongylus_positivo)

  prueba_chi <- suppressWarnings(chisq.test(tabla))

  if (any(prueba_chi$expected < 5)) {
    prueba <- fisher.test(tabla)
    metodo <- "Fisher exact test"
  } else {
    prueba <- prueba_chi
    metodo <- "Chi-square test"
  }

  tabla_porcentaje <- prop.table(tabla, margin = 1) * 100

  list(
    variable = variable,
    tabla = tabla,
    porcentaje_fila = round(tabla_porcentaje, 2),
    metodo = metodo,
    p_value = prueba$p.value
  )
}

# Funcion para Odds Ratio en variables binarias.
# La variable debe tener dos categorias.
# R interpreta la primera categoria como referencia.
calcular_or_binario <- function(variable) {

  tabla <- table(datos[[variable]], datos$strongylus_positivo)

  if (nrow(tabla) != 2 || ncol(tabla) != 2) {
    return(data.frame(
      variable = variable,
      mensaje = "No se calculo OR porque la variable no es binaria"
    ))
  }

  fisher <- fisher.test(tabla)

  data.frame(
    variable = variable,
    categoria_1 = rownames(tabla)[1],
    categoria_2 = rownames(tabla)[2],
    or_crudo = as.numeric(fisher$estimate),
    ic95_inferior = fisher$conf.int[1],
    ic95_superior = fisher$conf.int[2],
    p_value = fisher$p.value
  )
}

###############################################################
# 7. OBJETIVOS 3, 4 y 5
# Asociacion entre factores de riesgo y strongylus
###############################################################

variables_riesgo <- c(
  "especie",
  "sexo",
  "grupo_edad",
  "distrito",
  "estacion",
  "tipo_manejo",
  "frecuencia_desparasitacion",
  "alojamiento",
  "condicion_corporal"
)

resultados_bivariados <- lapply(variables_riesgo, analisis_categorico)

# Crear una tabla resumen con p-valores.
tabla_bivariada <- data.frame(
  variable = sapply(resultados_bivariados, function(x) x$variable),
  prueba = sapply(resultados_bivariados, function(x) x$metodo),
  p_value = sapply(resultados_bivariados, function(x) x$p_value)
) %>%
  mutate(
    significativo_005 = ifelse(p_value < 0.05, "Si", "No")
  )

tabla_bivariada

# OR crudos para variables binarias.
variables_binarias <- c("especie", "sexo", "tipo_manejo")

or_crudos <- bind_rows(
  lapply(variables_binarias, calcular_or_binario)
)

or_crudos

write_xlsx(
  list(
    tabla_bivariada = tabla_bivariada,
    or_crudos_binarios = or_crudos
  ),
  "resultados_parasitos_equinos/tablas/03_bivariado_or_crudos.xlsx"
)

###############################################################
# GRAFICOS DE ASOCIACION
###############################################################

# Grafico 5: prevalencia segun especie.
g_prev_especie <- ggplot(datos, aes(x = especie, fill = strongylus_positivo)) +
  geom_bar(position = "fill", color = "black") +
  labs(
    title = "Proporcion de infeccion segun especie",
    x = "Especie",
    y = "Proporcion",
    fill = "Strongylus"
  ) +
  theme_minimal()

ggsave(
  "resultados_parasitos_equinos/graficos/05_proporcion_especie.png",
  g_prev_especie,
  width = 7,
  height = 5,
  dpi = 300
)

# Grafico 6: prevalencia segun estacion.
g_prev_estacion <- ggplot(datos, aes(x = estacion, fill = strongylus_positivo)) +
  geom_bar(position = "fill", color = "black") +
  labs(
    title = "Proporcion de infeccion segun estacion",
    x = "Estacion",
    y = "Proporcion",
    fill = "Strongylus"
  ) +
  theme_minimal()

ggsave(
  "resultados_parasitos_equinos/graficos/06_proporcion_estacion.png",
  g_prev_estacion,
  width = 7,
  height = 5,
  dpi = 300
)

# Grafico 7: prevalencia segun frecuencia de desparasitacion.
g_prev_desparasitacion <- ggplot(datos, aes(x = frecuencia_desparasitacion, fill = strongylus_positivo)) +
  geom_bar(position = "fill", color = "black") +
  labs(
    title = "Proporcion de infeccion segun frecuencia de desparasitacion",
    x = "Frecuencia de desparasitacion",
    y = "Proporcion",
    fill = "Strongylus"
  ) +
  theme_minimal()

ggsave(
  "resultados_parasitos_equinos/graficos/07_proporcion_desparasitacion.png",
  g_prev_desparasitacion,
  width = 8,
  height = 5,
  dpi = 300
)

# Grafico 8: HPG segun especie.
g_hpg_especie <- ggplot(
  datos %>% filter(strongylus_positivo == "Yes"),
  aes(x = especie, y = hpg)
) +
  geom_boxplot() +
  labs(
    title = "Carga parasitaria HPG en equinos positivos segun especie",
    x = "Especie",
    y = "Huevos por gramo de heces (HPG)"
  ) +
  theme_minimal()

ggsave(
  "resultados_parasitos_equinos/graficos/08_boxplot_hpg_especie.png",
  g_hpg_especie,
  width = 7,
  height = 5,
  dpi = 300
)

###############################################################
# 8. OBJETIVO 6
# Regresion logistica multivariada
###############################################################

# La regresion logistica se usa porque la variable dependiente es binaria:
# strongylus_positivo = Yes / No.
#
# El modelo estima la probabilidad de ser positivo a Strongylus segun
# varios factores al mismo tiempo.
#
# En el modelo:
# - OR > 1 sugiere mayor probabilidad/riesgo de infeccion.
# - OR < 1 sugiere menor probabilidad/riesgo de infeccion.
# - p < 0.05 sugiere asociacion estadisticamente significativa.

modelo <- glm(
  strongylus_positivo ~ especie +
    sexo +
    grupo_edad +
    estacion +
    tipo_manejo +
    frecuencia_desparasitacion +
    alojamiento +
    condicion_corporal,
  data = datos,
  family = binomial
)

summary(modelo)

# Tabla de OR ajustados.
tabla_modelo <- tidy(modelo, exponentiate = TRUE, conf.int = TRUE) %>%
  rename(
    or_ajustado = estimate,
    ic95_inferior = conf.low,
    ic95_superior = conf.high,
    p_value = p.value
  ) %>%
  mutate(
    significativo_005 = ifelse(p_value < 0.05, "Si", "No")
  )

tabla_modelo

write_xlsx(
  list(
    regresion_logistica = tabla_modelo
  ),
  "resultados_parasitos_equinos/tablas/04_regresion_logistica.xlsx"
)

###############################################################
# FOREST PLOT DE OR AJUSTADOS
###############################################################

# Quitamos el intercepto porque no se interpreta como factor de riesgo.
tabla_forest <- tabla_modelo %>%
  filter(term != "(Intercept)") %>%
  mutate(term = str_replace_all(term, "_", " "))

g_forest <- ggplot(tabla_forest, aes(x = or_ajustado, y = forcats::fct_reorder(term, or_ajustado))) +
  geom_point() +
  geom_errorbarh(aes(xmin = ic95_inferior, xmax = ic95_superior), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10() +
  labs(
    title = "Odds Ratio ajustados para infeccion por Strongylus spp.",
    x = "OR ajustado, escala logaritmica",
    y = "Variable"
  ) +
  theme_minimal()

ggsave(
  "resultados_parasitos_equinos/graficos/09_forest_plot_or_ajustados.png",
  g_forest,
  width = 9,
  height = 6,
  dpi = 300
)

###############################################################
# 9. OBJETIVO 7
# Describir la carga parasitaria e intensidad de infeccion
###############################################################

positivos <- datos %>%
  filter(strongylus_positivo == "Yes")

resumen_hpg <- positivos %>%
  summarise(
    n_positivos = n(),
    hpg_media = mean(hpg),
    hpg_sd = sd(hpg),
    hpg_mediana = median(hpg),
    hpg_min = min(hpg),
    hpg_max = max(hpg)
  )

tabla_intensidad <- tabyl(datos, intensidad_infeccion)
tabla_tipo_strongylus <- tabyl(datos, tipo_strongylus)

write_xlsx(
  list(
    resumen_hpg_positivos = resumen_hpg,
    intensidad = tabla_intensidad,
    tipo_strongylus = tabla_tipo_strongylus
  ),
  "resultados_parasitos_equinos/tablas/05_carga_parasitaria_intensidad.xlsx"
)

# Grafico 10: distribucion de HPG en positivos.
g_hpg <- ggplot(positivos, aes(x = hpg)) +
  geom_histogram(bins = 12, color = "black") +
  labs(
    title = "Distribucion de HPG en equinos positivos",
    x = "Huevos por gramo de heces (HPG)",
    y = "Numero de equinos positivos"
  ) +
  theme_minimal()

ggsave(
  "resultados_parasitos_equinos/graficos/10_histograma_hpg_positivos.png",
  g_hpg,
  width = 8,
  height = 5,
  dpi = 300
)

# Grafico 11: tipo de strongylus identificado.
g_tipo <- ggplot(positivos, aes(x = tipo_strongylus)) +
  geom_bar(color = "black") +
  labs(
    title = "Tipos de Strongylus identificados en equinos positivos",
    x = "Tipo identificado",
    y = "Numero de equinos positivos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(
  "resultados_parasitos_equinos/graficos/11_tipo_strongylus.png",
  g_tipo,
  width = 8,
  height = 5,
  dpi = 300
)

###############################################################
# 10. RESPUESTAS AUTOMATICAS A LOS OBJETIVOS
###############################################################

# Esta seccion genera una tabla con textos base para el informe.
# Los estudiantes deben revisar e interpretar con sus propias palabras.

prev <- round(tabla_prevalencia$prevalencia_porcentaje, 2)
p_especie <- tabla_bivariada$p_value[tabla_bivariada$variable == "especie"]
p_estacion <- tabla_bivariada$p_value[tabla_bivariada$variable == "estacion"]
p_desparasitacion <- tabla_bivariada$p_value[tabla_bivariada$variable == "frecuencia_desparasitacion"]

respuesta_objetivos <- data.frame(
  objetivo = c(
    "Objetivo general",
    "Objetivo especifico 1",
    "Objetivo especifico 2",
    "Objetivo especifico 3",
    "Objetivo especifico 4",
    "Objetivo especifico 5",
    "Objetivo especifico 6",
    "Objetivo especifico 7"
  ),
  respuesta = c(
    "Se evaluaron factores asociados a la infeccion por Strongylus spp. mediante analisis descriptivo, pruebas bivariadas y regresion logistica.",
    paste0("La base incluyo ", nrow(datos), " equinos, caracterizados segun especie, sexo, edad, distrito, estacion y variables de manejo."),
    paste0("La prevalencia general de infeccion por Strongylus spp. fue de ", prev, "%."),
    paste0("La asociacion entre especie y positividad tuvo un p-valor de ", signif(p_especie, 3), ". Revisar la tabla bivariada y el OR crudo para interpretar la magnitud de la asociacion."),
    paste0("La asociacion entre estacion e infeccion tuvo un p-valor de ", signif(p_estacion, 3), "."),
    paste0("La frecuencia de desparasitacion tuvo un p-valor de ", signif(p_desparasitacion, 3), ". Tambien se evaluaron sexo, edad, manejo, alojamiento y condicion corporal."),
    "La regresion logistica permitio estimar OR ajustados, controlando el efecto simultaneo de las variables incluidas en el modelo.",
    "La carga parasitaria fue descrita mediante HPG, intensidad de infeccion y tipo de Strongylus identificado."
  )
)

write_xlsx(
  list(respuestas_objetivos = respuesta_objetivos),
  "resultados_parasitos_equinos/tablas/06_respuestas_objetivos.xlsx"
)

###############################################################
# 11. MENSAJE FINAL
###############################################################

cat("\n====================================================\n")
cat("ANALISIS FINALIZADO\n")
cat("Se crearon tablas en: resultados_parasitos_equinos/tablas\n")
cat("Se crearon graficos en: resultados_parasitos_equinos/graficos\n")
cat("Revise especialmente:\n")
cat("- 02_prevalencia_general.xlsx\n")
cat("- 03_bivariado_or_crudos.xlsx\n")
cat("- 04_regresion_logistica.xlsx\n")
cat("- 06_respuestas_objetivos.xlsx\n")
cat("====================================================\n")
