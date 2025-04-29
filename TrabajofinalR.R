###################R version 4.4.2##############################################

################################################################################
#################Variables que se utilizarán####################################
################################################################################

# Base de datos de personas
#id_per            / Identificador de persona
#fexp              / Factor de expansión
#area              / Área sociodemográfica
#region            / Región del Ecuador a la que pertenece
#prov              / Provincia 
#etnia             / Etnia 
#f1_s1_2           / Sexo
#f1_s1_3_1         / Años cumplidos
#f1_s1_8           / Carnet de discapacidad 
#f1_s1_15_1        / Nivel de instrucción de la madre
#quintil           / Quintiles por ingresos
#pobreza           / Pobreza por ingresos
#dcronica2_5       / Desnutrición crónica de 2 a menores de 5 años
#dglobal2_5        / Desnutrición global de 2 a menores de 5 años
#daguda2_5         / Desnutrición aguda de 2 a menores de 5 años

#Base de datos de Desarrollo infantil
#id_per             / Identificador de persona(usar para emparejar con f1_personas)
#f3_s6_609          / Disposición general del desarrollo del niño 
#fexp_di            / Factor de expansión 

################################################################################
########################### Paquetes y librerías ###############################
################################################################################

# Instalar paquetes necesarios
install.packages("tidyverse", dependencies = TRUE)
install.packages("readxl")
install.packages("janitor")
install.packages("knitr")
install.packages("openxlsx")
install.packages("kableExtra")
install.packages("survey")
install.packages("officer")
install.packages("flextable")
install.packages("forcats")
install.packages("logistf")

#Cargar librerías 
library(tidyverse)
library(readxl)
library(janitor)
library(ggplot2)
library(knitr)
library(openxlsx)
library(kableExtra)
library(survey)
library(officer)
library(flextable)
library(stringr)
library(broom)
library(forcats)
library(logistf)

################################################################################
########################## Subir bases##########################################
################################################################################

# Base personas
ENDI_per <- readRDS("C:/Users/User/Documents/YRF/trabajo final/BDD_ENDI_R2_rds/ENDI_personas.rds")

# Base desarrollo infantil
ENDI_desarrollo <- read.csv(
  "C:/Users/User/Documents/YRF/trabajo final/Base de datos ENDI diccionarios/BDD_ENDI_R2_f3_desarrollo/ENDI_desarrollo.csv",
  sep = ";",
  stringsAsFactors = FALSE,
  na.strings = c(".", "", "NA")
)

# Ver las primeras filas
head(ENDI_per)
head(ENDI_desarrollo)

#################### Seleccionar variables de cada base ########################

# Seleccionar variables de ENDI_per
ENDI_per_limpia <- ENDI_per %>%
  select(id_per, area, fexp, region, prov, etnia, f1_s1_2, f1_s1_3_1, f1_s1_8,
    f1_s1_15_1, quintil, pobreza, dcronica2_5, dglobal2_5,daguda2_5)

# Seleccionar variables de ENDI_desarrollo
ENDI_desarrollo_limpia <- ENDI_desarrollo %>%
  clean_names() %>%
  select(id_per, f3_s6_609, fexp_di) %>%
  mutate(
    id_per = as.character(id_per),
    f3_s6_609 = case_when(
      f3_s6_609 == "Buena" ~ 1,
      f3_s6_609 == "Regular" ~ 2,
      f3_s6_609 == "Malo" ~ 3,
      TRUE ~ NA_real_
    ),
    dis_gen_desarrollo = factor(f3_s6_609,
                                levels = c(1, 2, 3),
                                labels = c("Buena", "Regular", "Malo")))

############################## Conversión ######################################

# Convertir id_per a character en las dos bases
ENDI_per_limpia <- ENDI_per_limpia %>%
  mutate(id_per = as.character(id_per))

###################### Unir las bases de datos #################################
ENDI_completa <- ENDI_per_limpia %>%
  inner_join(ENDI_desarrollo_limpia, by = "id_per")

# Ver las primeras filas de la base de datos unida
head(ENDI_completa)

######################### Renombrar las variables ##############################
ENDI_completa <- ENDI_completa %>% 
  rename (sexo = f1_s1_2,
    edad = f1_s1_3_1,
    carnet_discapacidad = f1_s1_8,
    instruccion = f1_s1_15_1)

# Recodeo y conversión de variables según diccionario
ENDI_completa <- ENDI_completa %>%
  mutate(
    # Área
    area = factor(area, levels = c(1, 2), labels = c("Urbano", "Rural")),
    
    # Región
    region = factor(region, 
                    levels = c(1, 2, 3),
                    labels = c("Sierra", "Costa", "Amazonía")),
    
    # Etnia
    etnia = factor(etnia,
                   levels = c(1, 2, 3, 4, 5),
                   labels = c("Indígena", "Afroecuatoriana/o", "Montubia/o", "Mestiza/o", "Blanca/o u Otra/o")),
    
    # Sexo
    sexo = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    
    # Carnet de discapacidad
    carnet_discapacidad = factor(carnet_discapacidad, levels = c(1, 2), labels = c("Sí", "No")),
    
    # Nivel de instrucción de la madre (según diccionario)
    instruccion = factor(instruccion,
                         levels = c(1, 2, 3, 6), 
                         labels = c("Ninguno",
                                    "Centro de desarrollo/Creciendo con nuestros hijos/Guardería",
                                    "Educación Inicial/Preescolar/SAFPI",
                                    "Educación General Básica (EGB)")),
    # Provincia
    prov = factor(prov,
                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                             16, 17, 18, 19, 21, 22, 23, 24),
                  labels = c("Azuay", "Bolívar", "Cañar", "Carchi", "Cotopaxi",
                             "Chimborazo", "El Oro", "Esmeraldas", "Guayas", "Imbabura",
                             "Loja", "Los Ríos", "Manabí", "Morona Santiago", "Napo",
                             "Pastaza", "Pichincha", "Tungurahua", "Zamora Chinchipe",
                             "Sucumbíos", "Orellana", "Sto Domingo de los Tsáchilas",
                             "Santa Elena")),
    
    # Quintil de ingreso
    quintil = factor(quintil,
                     levels = c(1, 2, 3, 4, 5),
                     labels = c("Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5")),
    
    # Pobreza por ingreso
    pobreza = factor(pobreza, levels = c(0, 1), labels = c("No pobreza por ingresos", "Pobreza por ingresos")),
    
    # Desnutrición crónica (variable dependiente 1 para regresión)
    dcronica2_5 = factor(dcronica2_5, levels = c(0, 1), labels = c("Sin desnutrición", "Desnutrición crónica")),
    
    # Desnutrición global (variable dependiente 2 para regresión)
    dglobal2_5 = factor(dglobal2_5, levels = c(0, 1), labels = c("Sin desnutrición", "Desnutrición global")),
    
    #Desnutrición aguda (variable dependiente 3 para regresión)
    daguda2_5 = factor(daguda2_5, levels = c(0, 1), labels = c("Sin desnutrición", "Desnutrición aguda")))

# Validar la estructura final de la base
str(ENDI_completa)
# # Crear un factor de expansión específico 
ENDI_completa <- ENDI_completa %>%
  mutate(fexp_final = ifelse(is.na(fexp_di), fexp, fexp_di))

################################################################################
################### Limpieza de base completa #################################
################################################################################

#Codificar las variables
variables_completas <- c("edad", "area", "sexo", "region", "prov", "etnia", 
                         "carnet_discapacidad","instruccion", "quintil", 
                         "pobreza", "dcronica2_5", 
                         "dglobal2_5", "daguda2_5", "dis_gen_desarrollo", "fexp_final")

#Limpiar la base
ENDI_completa_limpia <- ENDI_completa %>%
  filter(if_all(all_of(variables_completas), ~ !is.na(.)))

# Crear variable binaria: dis_desarrollo_bin
ENDI_completa_limpia <- ENDI_completa_limpia %>%
  mutate(dis_desarrollo_bin = fct_collapse(dis_gen_desarrollo,
                                           "Óptimo" = "Buena",
                                           "No óptimo" = c("Regular", "Malo")))

################################################################################
########################### Análisis Estadístico ###############################
################################################################################

################# Estadísticos descriptivos ####################################
# Lista de variables categóricas a incluir
variables_categoricas <- c("area", "sexo", "prov" ,"region", "etnia", 
                           "carnet_discapacidad","instruccion", "quintil",
                           "pobreza", "dcronica2_5", 
                           "dglobal2_5", "daguda2_5", "dis_desarrollo_bin")

# Diseño muestral con factor de expansión combinado
design_apacat <- svydesign(ids = ~1, data = ENDI_completa_limpia, weights = ~fexp_final)

# Inicializar tabla final
tabla_final_categoricas <- data.frame()

# Bucle para calcular frecuencias y porcentajes ponderados
for (var in variables_categoricas) {
  tab <- svytable(as.formula(paste("~", var)), design_apacat)
  tab_df <- as.data.frame(tab)
  colnames(tab_df) <- c("Categoria", "Frecuencia")
  total <- sum(tab_df$Frecuencia)
  
  tab_df <- tab_df %>%
    mutate(
      Porcentaje = round((Frecuencia / total) * 100, 2),
      Variable = var
    ) %>%
    select(Variable, Categoria, Frecuencia, Porcentaje)
  
  tabla_final_categoricas <- bind_rows(tabla_final_categoricas, tab_df)
}

# Formato final
tabla_final_categoricas <- tabla_final_categoricas %>%
  arrange(Variable) %>%
  rename(`Frecuencia ponderada` = Frecuencia,
         `Porcentaje ponderado (%)` = Porcentaje)

# Exportar
write.xlsx(tabla_final_categoricas,
           file = "C:/Users/User/Documents/YRF/trabajo final/tabla_APA_categoricas.xlsx")

# Crear flextable para guardar en word
ft_cat <- flextable(tabla_final_categoricas)
ft_cat <- autofit(ft_cat)
ft_cat <- set_caption(ft_cat, caption = "Tabla 1\nFrecuencias y Porcentajes de Variables Categóricas")

# Crear documento Word
doc <- read_docx() %>%
  body_add_par("Tabla 1. Frecuencias y Porcentajes de Variables Categóricas", style = "heading 1") %>%
  body_add_flextable(ft_cat) %>%
  body_add_par("Nota: Elaboración propia a partir de la base ENDI.", style = "Normal")

# Guardar Word
print(doc, target = "C:/Users/User/Documents/arti/tabla_categoricas_APA.docx")

# Edad 
# Estadísticos descriptivos ponderados para la variable "edad"
# Calcular estadísticas ponderadas
edad_media <- svymean(~edad, design_apacat, na.rm = TRUE)
edad_mediana <- svyquantile(~edad, design_apacat, quantiles = 0.5, ci = TRUE)
edad_minmax <- svyquantile(~edad, design_apacat, quantiles = c(0, 1), ci = FALSE)

# Extraer valores numéricos con coef() en lugar de as.numeric()
media_valor <- coef(edad_media)[1]
mediana_valor <- coef(edad_mediana)[1]
minimo_valor <- coef(edad_minmax)[1]
maximo_valor <- coef(edad_minmax)[2]

# Crear tabla
tabla_edad <- data.frame(
  Estadístico = c("Media", "Mediana", "Mínimo", "Máximo"),
  Valor = round(c(media_valor, mediana_valor, minimo_valor, maximo_valor), 2)
)

#Ver tabla
print(tabla_edad)

# Crear flextable
ft_edad <- flextable(tabla_edad) %>%
  autofit() %>%
  set_caption(caption = "Tabla 2\nEstadísticos Descriptivos de Edad (ponderados)")

# Añadir al documento Word existente
doc <- doc %>%
  body_add_par("Tabla 2. Estadísticos Descriptivos de Edad (ponderados)", style = "heading 1") %>%
  body_add_flextable(ft_edad) %>%
  body_add_par("Nota: Elaboración propia a partir de la base ENDI.", style = "Normal")

# Guardar nuevamente el documento Word
print(doc, target = "C:/Users/User/Documents/arti/tabla_categoricas_APA.docx")


# Ver resultado en consola
print(tabla_edad)

########### tras limpiar la base se quedó un número de observaciones en edades de 3 a 4 años, se puede verificar a través de este código.
summary(ENDI_completa$edad)
summary(ENDI_completa_limpia$edad)


############# Gráficar las variables de desnutrición
# Función para gráfico de pastel estilo APA
grafico_pastel_desnutricion <- function(var_name, design, titulo, colores, nombre_archivo) {
  # Tabla ponderada
  tab <- svytable(as.formula(paste("~", var_name)), design)
  df <- as.data.frame(tab)
  colnames(df) <- c("Categoria", "Frecuencia")
  
  # Calcular porcentaje
  df <- df %>%
    mutate(
      Porcentaje = round((Frecuencia / sum(Frecuencia)) * 100, 1)
    )
  
  # Gráfico
  grafico <- ggplot(df, aes(x = "", y = Porcentaje, fill = Categoria)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    geom_text(aes(label = paste0(Porcentaje, "%")),
              position = position_stack(vjust = 0.5),
              color = "white", size = 5) +
    labs(
      title = titulo,
      fill = "Desnutrición"
    ) +
    scale_fill_manual(values = colores) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10)
    )
  
  # Guardar imagen
  ggsave(nombre_archivo, plot = grafico, width = 8, height = 6, dpi = 300)
  
  # Mostrar en consola
  print(grafico)
}

# Definir ruta absoluta para guardar las imágenes
ruta_cronica <- "C:/Users/User/Documents/YRF/trabajo final/grafico_dcronica_pastel.png"
ruta_global <- "C:/Users/User/Documents/YRF/trabajo final/grafico_dglobal_pastel.png"
ruta_aguda <- "C:/Users/User/Documents/YRF/trabajo final/grafico_daguda_pastel.png"

# Desnutrición crónica
grafico_pastel_desnutricion(
  var_name = "dcronica2_5",
  design = design_apacat,
  titulo = "Desnutrición crónica en niños de entre 2 a 5 años",
  colores = c("Sin desnutrición" = "deepskyblue", "Desnutrición crónica" = "coral2"),
  nombre_archivo = ruta_cronica
)

# Desnutrición global
grafico_pastel_desnutricion(
  var_name = "dglobal2_5",
  design = design_apacat,
  titulo = "Desnutrición global en niños de entre 2 a 5 años",
  colores = c("Sin desnutrición" = "mediumseagreen", "Desnutrición global" = "orange2"),
  nombre_archivo = ruta_global
)

# Desnutrición aguda
grafico_pastel_desnutricion(
  var_name = "daguda2_5",
  design = design_apacat,
  titulo = "Desnutrición aguda en niños de entre 2 a 5 años",
  colores = c("Sin desnutrición" = "skyblue3", "Desnutrición aguda" = "firebrick1"),
  nombre_archivo = ruta_aguda
)

################################################################################
############################## Regresión logística #############################
################################################################################
###Regresiones ponderadas con variable binaria de desarrollo                

# Definir niveles de referencia
ENDI_completa_limpia <- ENDI_completa_limpia %>%
  mutate(
    etnia = relevel(etnia, ref = "Mestiza/o"),
    pobreza = relevel(pobreza, ref = "No pobreza por ingresos"),
    dis_desarrollo_bin = relevel(dis_desarrollo_bin, ref = "Óptimo"),
    area = relevel(area, ref = "Urbano"),
    instruccion = relevel(instruccion, ref = "Educación Inicial/Preescolar/SAFPI")
  )

# Definir diseño muestral
design_bin <- svydesign(ids = ~1, data = ENDI_completa_limpia, weights = ~fexp_final)

# Modelos svyglm
modelo_cronica_bin <- svyglm(
  dcronica2_5 ~ dis_desarrollo_bin + etnia + pobreza + area + instruccion,
  design = design_bin, family = quasibinomial()
)

modelo_global_bin <- svyglm(
  dglobal2_5 ~ dis_desarrollo_bin + etnia + pobreza + area + instruccion,
  design = design_bin, family = quasibinomial()
)

# Crear función para tabla de resultados
tabla_resultados <- function(modelo, nombre_modelo){
  tidy(modelo) %>%
    mutate(
      OR = exp(estimate),
      IC_inf = exp(estimate - 1.96 * std.error),
      IC_sup = exp(estimate + 1.96 * std.error),
      p_stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE ~ ""
      ),
      p_value = format.pval(p.value, digits = 3, eps = 0.001),
      Modelo = nombre_modelo
    ) %>%
    select(Modelo, term, OR, IC_inf, IC_sup, p_value, p_stars)
}

# Tablas de resultados
tabla_cronica_bin <- tabla_resultados(modelo_cronica_bin, "Desnutrición Crónica")
tabla_global_bin  <- tabla_resultados(modelo_global_bin,  "Desnutrición Global")

# Crear flextables
ft_cronica <- flextable(tabla_cronica_bin) %>%
  set_caption("Tabla 1\nModelo Logístico – Desnutrición Crónica (ponderado)") %>%
  autofit()

ft_global <- flextable(tabla_global_bin) %>%
  set_caption("Tabla 2\nModelo Logístico – Desnutrición Global (ponderado)") %>%
  autofit()

################################################################################
### Modelo Firth para desnutrición aguda                                     ###
################################################################################

# Preparar base para logistf
ENDI_aguda_firth_df <- ENDI_completa_limpia %>%
  select(daguda2_5, dis_desarrollo_bin, etnia, pobreza, area, instruccion) %>%
  mutate(daguda2_5 = as.numeric(daguda2_5) - 1)

# Ajustar modelo
modelo_aguda_firth <- logistf(
  formula = daguda2_5 ~ dis_desarrollo_bin + etnia + pobreza + area + instruccion,
  data = ENDI_aguda_firth_df
)

# Extraer resultados
coef_vals <- modelo_aguda_firth[["coefficients"]]
ci_inf_vals <- modelo_aguda_firth[["ci.lower"]]
ci_sup_vals <- modelo_aguda_firth[["ci.upper"]]
p_vals <- modelo_aguda_firth[["prob"]]

# Crear tabla de resultados
tabla_aguda_firth <- data.frame(
  Modelo = rep("Desnutrición Aguda", length(coef_vals)),
  term = names(coef_vals),
  OR = round(exp(coef_vals), 4),
  IC_inf = round(exp(ci_inf_vals), 4),
  IC_sup = round(exp(ci_sup_vals), 4),
  p_value = format.pval(p_vals, digits = 3, eps = 0.001),
  p_stars = case_when(
    p_vals < 0.001 ~ "***",
    p_vals < 0.01  ~ "**",
    p_vals < 0.05  ~ "*",
    p_vals < 0.1   ~ ".",
    TRUE ~ ""
  )
)

# Flextable para Firth
ft_aguda <- flextable(tabla_aguda_firth) %>%
  set_caption("Tabla 3\nModelo Firth – Desnutrición Aguda") %>%
  autofit()

### Exportar a Word                                                          

doc <- read_docx() %>%
  body_add_par("Tabla 1. Modelo Logístico para Desnutrición Crónica (ponderado)", style = "heading 1") %>%
  body_add_flextable(ft_cronica) %>%
  body_add_par("Nota: OR = razón de odds; IC = intervalo de confianza del 95%.", style = "Normal") %>%
  
  body_add_par("Tabla 2. Modelo Logístico para Desnutrición Global (ponderado)", style = "heading 1") %>%
  body_add_flextable(ft_global) %>%
  body_add_par("Nota: OR = razón de odds; IC = intervalo de confianza del 95%.", style = "Normal") %>%
  
  body_add_par("Tabla 3. Modelo Firth para Desnutrición Aguda", style = "heading 1") %>%
  body_add_flextable(ft_aguda) %>%
  body_add_par("Nota: Regresión penalizada de Firth. OR = razón de odds; IC = intervalo de confianza del 95%.", style = "Normal")

# Guardar documento
print(doc, target = "modelo_binario_simplificado.docx")


################################################################################
####### Modelos con varible original(ordinal): disposición del desarrollo ######
################################################################################
################################################################################
### Regresiones ponderadas con variable ordinal de desarrollo               ###
################################################################################

# Reestablecer referencia para variable ordinal
ENDI_completa_limpia <- ENDI_completa_limpia %>%
  mutate(
    dis_gen_desarrollo = relevel(dis_gen_desarrollo, ref = "Buena"),
    etnia = relevel(etnia, ref = "Mestiza/o"),
    pobreza = relevel(pobreza, ref = "No pobreza por ingresos"),
    area = relevel(area, ref = "Urbano"),
    instruccion = relevel(instruccion, ref = "Educación Inicial/Preescolar/SAFPI")
  )

# Diseño muestral
design_ordinal <- svydesign(ids = ~1, data = ENDI_completa_limpia, weights = ~fexp_final)

# Modelos svyglm con variable ordinal
modelo_cronica_ord <- svyglm(
  dcronica2_5 ~ dis_gen_desarrollo + etnia + pobreza + area + instruccion,
  design = design_ordinal, family = quasibinomial()
)

modelo_global_ord <- svyglm(
  dglobal2_5 ~ dis_gen_desarrollo + etnia + pobreza + area + instruccion,
  design = design_ordinal, family = quasibinomial()
)

modelo_aguda_ord <- svyglm(
  daguda2_5 ~ dis_gen_desarrollo + etnia + pobreza + area + instruccion,
  design = design_ordinal, family = quasibinomial()
)

# Crear tabla de resultados
tabla_resultados <- function(modelo, nombre_modelo){
  tidy(modelo) %>%
    mutate(
      OR = exp(estimate),
      IC_inf = exp(estimate - 1.96 * std.error),
      IC_sup = exp(estimate + 1.96 * std.error),
      p_stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value < 0.1 ~ ".",
        TRUE ~ ""
      ),
      p_value = format.pval(p.value, digits = 3, eps = 0.001),
      Modelo = nombre_modelo
    ) %>%
    select(Modelo, term, OR, IC_inf, IC_sup, p_value, p_stars)
}

# Crear tablas de resultados
tabla_cronica_ord <- tabla_resultados(modelo_cronica_ord, "Desnutrición Crónica (ordinal)")
tabla_global_ord  <- tabla_resultados(modelo_global_ord, "Desnutrición Global (ordinal)")
tabla_aguda_ord   <- tabla_resultados(modelo_aguda_ord,  "Desnutrición Aguda (ordinal)")

# Crear flextables
ft_cronica_ord <- flextable(tabla_cronica_ord) %>%
  set_caption("Tabla 4\nModelo Ordinal – Desnutrición Crónica") %>%
  autofit()

ft_global_ord <- flextable(tabla_global_ord) %>%
  set_caption("Tabla 5\nModelo Ordinal – Desnutrición Global") %>%
  autofit()

ft_aguda_ord <- flextable(tabla_aguda_ord) %>%
  set_caption("Tabla 6\nModelo Ordinal – Desnutrición Aguda") %>%
  autofit()

### Exportar a Word                                                          

doc <- read_docx() %>%
  body_add_par("Tabla 4. Modelo Ordinal para Desnutrición Crónica", style = "heading 1") %>%
  body_add_flextable(ft_cronica_ord) %>%
  body_add_par("Nota. OR = razón de odds; IC = intervalo de confianza del 95%. Categoría de referencia: Buena.", style = "Normal") %>%
  
  body_add_par("Tabla 5. Modelo Ordinal para Desnutrición Global", style = "heading 1") %>%
  body_add_flextable(ft_global_ord) %>%
  body_add_par("Nota. OR = razón de odds; IC = intervalo de confianza del 95%. Categoría de referencia: Buena.", style = "Normal") %>%
  
  body_add_par("Tabla 6. Modelo Ordinal para Desnutrición Aguda", style = "heading 1") %>%
  body_add_flextable(ft_aguda_ord) %>%
  body_add_par("Nota. OR = razón de odds; IC = intervalo de confianza del 95%. Categoría de referencia: Buena.", style = "Normal")

# Guardar documento Word
print(doc, target = "modelo_ordinal_simplificado.docx")
