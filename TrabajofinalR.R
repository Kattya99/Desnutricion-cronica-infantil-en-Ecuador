###################R version 4.4.2##############################################

#################Variables que se utilizarán####################################

# Base de datos de personas
#id_per            / Identificador de persona
#fexp              / Factor de expansión
#area              / Área sociodemográfica
#region            / Región del Ecuador a la que pertenece
#prov              / Provincia 
#etnia             / etnia 
#f1_s1_2           / sexo
#f1_s1_3_1         / años cumplidos
#f1_s1_8           / Carnet de discapacidad 
#f1_s1_15_1        / nivel de instrucción de la madre
#quintil           / quintiles por ingresos
#pobreza           / pobreza 
#pobreza_nbi       / pobreza por necesidades básicas insatisfechas
#dcronica2_5       / Desnutrición crónica de 2 a menores de 5 años

#Base de datos de Desarrollo infantil
#id_per             / Identificador de persona (usar para emparejar con f1_personas)
#f3_s6_609          / disposición general del desarrollo del niño 


########################### Instalar paquetes necesarios########################
install.packages("tidyverse", dependencies = TRUE)
install.packages("readxl")
install.packages("janitor")
install.packages("knitr")
install.packages("openxlsx")
install.packages("kableExtra")
install.packages("survey")
install.packages("officer")
install.packages("flextable")

########################### Cargar librerías ###################################
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

################################################################################
####################### Subir bases#############################################
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

# Limpiar nombres de columnas
ENDI_desarrollo <- ENDI_desarrollo %>% clean_names()


# Ver las primeras filas
head(ENDI_per)
head(ENDI_desarrollo)

#################### Seleccionar variables de cada base ########################

# Seleccionar variables de ENDI_per
ENDI_per_limpia <- ENDI_per %>%
  select(
    id_per, area, fexp, region, prov, etnia, f1_s1_2, f1_s1_3_1, f1_s1_8,
    f1_s1_15_1, quintil, pobreza,
    nbi_1, dcronica2_5)

# Seleccionar variables de ENDI_desarrollo
ENDI_desarrollo_limpia <- ENDI_desarrollo %>%
  select(id_per, f3_s6_609)

############################## Conversión ######################################
# Convertir id_per a character en todas las bases
ENDI_per_limpia <- ENDI_per_limpia %>%
  mutate(id_per = as.character(id_per))

ENDI_desarrollo_limpia <- ENDI_desarrollo_limpia %>%
  mutate(id_per = as.character(id_per))

###################### Unir las bases de datos #################################
ENDI_completa <- ENDI_per_limpia %>%
  left_join(ENDI_desarrollo_limpia, by = "id_per") 

# Ver las primeras filas de la base de datos unida
head(ENDI_completa)

######################### Renombrar las variables ##############################
ENDI_completa <- ENDI_completa %>% 
  rename (
    sexo = f1_s1_2,
    edad = f1_s1_3_1,
    carnet_discapacidad = f1_s1_8,
    instruccion = f1_s1_15_1,
    dis_gen_desarrollo = f3_s6_609,
  )

# Revisar variables únicas
table_variables <- lapply(ENDI_completa, unique)

# Ver la estructura de la base de datos
str(ENDI_completa)

############################## LIMPIEZA ########################################

# Filtración de registros con información en dcronica2_5
ENDI_completa <- ENDI_completa %>%
  filter(!is.na(dcronica2_5), !is.na(fexp))

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
    
    # Quintil de ingreso
    quintil = factor(quintil,
                     levels = c(1, 2, 3, 4, 5),
                     labels = c("Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5")),
    
    # Pobreza por ingreso
    pobreza = factor(pobreza, levels = c(0, 1), labels = c("No pobreza por ingresos", "Pobreza por ingresos")),
    
    # Pobreza por necesidades básicas insatisfechas
    nbi_1 = factor(nbi_1, levels = c(0, 1), labels = c("No pobreza NBI", "Pobreza NBI")),
    
    # Desnutrición crónica (variable dependiente)
    dcronica2_5 = factor(dcronica2_5, levels = c(0, 1), labels = c("Sin desnutrición", "Desnutrición crónica")),
    
    # Desarrollo general del niño/a
    dis_gen_desarrollo = factor(dis_gen_desarrollo, levels = c("Buena", "Regular", "Malo"))
  )

# Validar la estructura final de la base
str(ENDI_completa)

# Guardar la base limpia y recodificada
saveRDS(ENDI_completa, "C:/Users/User/Documents/YRF/trabajo final/ENDI_limpia_recodificada.rds")

################# Definir variables categóricas ################################
variables_categoricas <- c(
  "area",
  "region",
  "etnia",
  "sexo",
  "carnet_discapacidad",
  "instruccion",
  "quintil",
  "pobreza",
  "nbi_1",
  "dcronica2_5",
  "dis_gen_desarrollo"
)

#################### Diseño de la encuesta #####################################
ENDI_design <- svydesign(ids = ~1, data = ENDI_completa, weights = ~fexp)

################# Estadísticos descriptivos ####################################
tabla_final <- data.frame()

for (var in variables_categoricas) {
  
  # Tabla ponderada
  tab <- svytable(as.formula(paste("~", var)), ENDI_design)
  tab_df <- as.data.frame(tab)
  colnames(tab_df) <- c("Categoria", "Frecuencia")
  
  # Calcular total de la variable
  total <- sum(tab_df$Frecuencia)
  
  # Añadir porcentaje
  tab_df <- tab_df %>%
    mutate(Porcentaje = round((Frecuencia / total) * 100, 2)) %>%
    mutate(Variable = var) %>%
    select(Variable, Categoria, Frecuencia, Porcentaje)
  
  # Unir con la tabla final
  tabla_final <- bind_rows(tabla_final, tab_df)
}

# Visualizar la tabla final
head(tabla_final)


########################## Ordenar y formatear #################################
tabla_final <- tabla_final %>%
  arrange(Variable) %>%
  rename(`Frecuencia ponderada` = Frecuencia,
         `Porcentaje ponderado (%)` = Porcentaje)

########################## Exportar como Excel (para dar formato APA) ##########
write.xlsx(tabla_final, file = "C:/Users/User/Documents/YRF/trabajo final/Descriptivos_APA_ENDI.xlsx")

########################## Mostrar primera parte de la tabla ###################
print(head(tabla_final, 10))

cat("✅ Tabla única en formato APA 7 creada y exportada a Excel.\n")

# Crear un documento Word en blanco
doc <- read_docx()

# Convertir la tabla final a flextable
tabla_flex <- flextable(tabla_final) %>%
  autofit() %>%
  theme_booktabs() %>%   # Estilo más limpio
  set_table_properties(width = 1, layout = "autofit") %>%
  fontsize(size = 11, part = "all") %>%       # Tamaño de letra APA
  font(fontname = "Times New Roman", part = "all") %>%  # Fuente APA
  align(align = "center", part = "all")       # Centrado como sugiere APA

# Agregar un título (opcional)
doc <- doc %>%
  body_add_par("Tabla 1\nFrecuencias y Porcentajes de Variables Categóricas", style = "heading 1") %>%
  body_add_flextable(tabla_flex) %>%
  body_add_par("Nota: Elaboración propia a partir de la base ENDI.", style = "Normal")

# Guardar el documento
print(doc, target = "tabla_final_APA.docx")


################ desnutricion cronica############################
# Tabla ponderada solo para desnutrición crónica
tabla_dcronica <- svytable(~ dcronica2_5, ENDI_design)

# Convertir a dataframe para graficar
df_dcronica <- as.data.frame(tabla_dcronica) %>%
  mutate(Porcentaje = round(Freq / sum(Freq) * 100, 2))


ggsave("grafico_dcronica_pastel.png", width = 10, height = 8, dpi = 300)

ggplot(df_dcronica, aes(x = "", y = Porcentaje, fill = dcronica2_5)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 5) +
  labs(
    title = "Desnutrición crónica en niños de entre 2 a 5 años",
    fill = "Desnutrición"
  ) +
  scale_fill_manual(values = c("Sin desnutrición" = "deepskyblue",
                               "Desnutrición crónica" = "coral2")) +
  theme_void() +
  theme(
    plot.title.position = "plot",           
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )


#################### Regresión ################################################
# Diseño con el factor de expansión
ENDI_design <- svydesign(ids = ~1, data = ENDI_completa, weights = ~fexp)

modelo_logit <- svyglm(dcronica2_5 ~ etnia + dis_gen_desarrollo + pobreza + nbi_1 + edad + sexo + region,
                       design = ENDI_design,
                       family = quasibinomial())

summary(modelo_logit)

# Obtener los coeficientes
coefs <- summary(modelo_logit)$coefficients

# Calcular OR y sus IC95
OR <- exp(coefs[, "Estimate"])
IC_lower <- exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"])
IC_upper <- exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"])
p_values <- coefs[, "Pr(>|t|)"]

# Crear tabla en data frame
tabla_regresion <- data.frame(
  Variable = rownames(coefs),
  OR = round(OR, 2),
  IC95 = paste0(round(IC_lower, 2), " - ", round(IC_upper, 2)),
  p_value = ifelse(p_values < 0.001, "<0.001", round(p_values, 3))
)

# Formato final para APA
tabla_regresion <- tabla_regresion %>%
  mutate(Interpretacion = paste0(OR, " (", IC95, ")"))

# Mostrar tabla
print(tabla_regresion)

write.xlsx(tabla_regresion, "C:/Users/User/Documents/YRF/trabajo final/Regresion_Logistica_APA.xlsx")

# Crear documento Word
doc <- read_docx()

# Convertir la tabla de regresión a flextable con formato APA
tabla_flex <- flextable(tabla_regresion) %>%
  theme_booktabs() %>%  # Estilo limpio y formal
  autofit() %>%
  fontsize(size = 11, part = "all") %>%  # APA: Times New Roman 11
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "center", part = "all") %>%
  set_table_properties(width = 1, layout = "autofit")

# Añadir título de tabla y la flextable
doc <- doc %>%
  body_add_par("Tabla 2\nRegresión logística: Factores asociados a la desnutrición crónica", style = "heading 1") %>%
  body_add_flextable(tabla_flex) %>%
  body_add_par("Nota: OR = Odds Ratio; IC95% = Intervalo de confianza al 95%. Elaboración propia.", style = "Normal")

# Guardar el documento Word
print(doc, target = "regresion_logistica_APA.docx")
