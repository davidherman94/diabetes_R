
rm(list=ls()) 

#Riesgo de infarto 

#Instalación de paquetes
library(tidyverse)
library(haven)
library(gtsummary)
library(broom)
library(LogisticDx)
library(here)
library(dplyr)
library(meta) 
library(metafor) 
library(AICcmodavg)
library(readr)
#library(Rcmdr)

#Cargar base de datos
setwd("C:/Users/david/Desktop/Database_diabetes")
datos_ENHS <- read.csv("EESEadulto_2020.csv", sep =  ";", header = TRUE)
#setwd("~/R")
#EESEadulto_2020 <- read_delim("ESHS_2020/datos_2020_individual/CSV/EESEadulto_2020.csv", 
#                                +     delim = "\t", escape_double = FALSE, 
 #                               +     trim_ws = TRUE)
view(datos_ENHS)

#Eleccion variables


### REVISAR ESTO DE CONVERTIR NO SABE NO RESPONDE Y NA
datos_ENHS[datos_ENHS == ''] <- NA
datos_ENHS[datos_ENHS == '98'] <- NA
datos_ENHS[datos_ENHS == '99'] <- NA


                                      ### RECATEGORIZACION DE LOS GRUPOS ###

## GRUPO EDAD
datos_ENHS <- datos_ENHS %>% 
  mutate(
    EDADa = case_when(
      EDADa >= 15 & EDADa <= 59 ~ "< 60 años",
      EDADa >= 60               ~ "> 60 años") %>% as.factor())

## Años residiendo
datos_ENHS <- datos_ENHS %>% 
  mutate(
    E3 = case_when(
      E3 >= 0 & E3 <= 9 ~ "< 10 años",
      E3 >= 10           ~ "> 10 años") %>% as.factor())

##Severidad depresiva
datos_ENHS <- within(datos_ENHS, {
  SEVERIDAD_DEPRESIVA <- Recode(SEVERIDAD_DEPRESIVA, 
                                '1 = "Ninguna"; 2 = "Leve"; 3 = "Moderada"; 4 = "Moderadamente Grave"; 5 = "Grave"; 9 = "NA"',
                                as.factor=TRUE, to.value="=", interval=":", separator=";")
})

##IMC

datos_ENHS <- within(datos_ENHS, {
  IMC <- Recode(IMC, 
                '1 = "Peso insuficiente"; 2 = "Normopeso"; 3 = "Sobrepeso"; 4 = "Obesidad"; 9 = "NA"',
                as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## CLASE SOCIAL
datos_ENHS <- within(datos_ENHS, {
  CLASE_PR <- Recode(CLASE_PR, 
                     '1 = "Directores/as y gerentes de establecimientos de 10 o más asalariados/as y profesionales tradicionalmente asociados/as a las licenciaturas universitarias"; 2 = "Directores/as y gerentes de establecimientos de menos de 10 asalariados/ as y profesionales tradicionalmente asociados/as a diplomaturas universitarias y otros/as profesionales de apoyo técnico. Deportistas y artistas"; 3 = "Ocupaciones intermedias y trabajadores/as por cuenta propia"; 4 = "Supervisores/as y trabajadores/as en ocupaciones técnicas cualificadas"; 5 = "Trabajadores/as cualificados/as del sector primario y otros/as trabajadores/as semi-cualificados/as"; 6 = "Trabajadores/as no cualificados/as"; 8 = "No sabe"; 9 = "No contesta"',
                     as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## APOYO SOCIAL: ¿En qué medida se interesan otras personas por lo que a usted le pasa?
datos_ENHS <- within(datos_ENHS, {
  X131 <- Recode(X131, 
                 '1 = "Mucho"; 2 = "Algo"; 3 = "Ni mucho ni poco"; 4 = "Poco"; 5 = "Nada"; 8 = "No sabe"; 9 = "No contesta"',
                 as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## CONSUMO DE ALCOHOL
datos_ENHS <- within(datos_ENHS, {
  W127 <- Recode(W127, 
                 '01 = "A diario o casi a diario"; 02 = "5-6 días por semana"; 03 = "3-4 días por semana"; 04 = "1-2 días por semana"; 05 = "2-3 días en un mes"; 06 = "Una vez al mes"; 07 = "Menos de una vez al mes"; 08 = "No en los últimos 12 meses, he dejado de tomar alcohol"; 09 = "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida"',
                 as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## FUMAR: ¿Fuma actualmente?
datos_ENHS <- within(datos_ENHS, {
  V121 <- Recode(V121, 
                 '1 = "Sí, fumo a diario"; 2 = "Sí fumo, pero no a diario"; 3 = "No fumo actualmente, pero he fumado antes"; 4 = "No fumo ni he fumado nunca de manera habitual"; 8 = "No sabe"; 9 = "No contesta"',
                 as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## EJERCICIO: Frecuencia con la que realiza alguna actividad física en su tiempo libre
datos_ENHS <- within(datos_ENHS, {
  T112 <- Recode(T112, 
                 '1 = "No hago ejercicio. El tiempo libre lo ocupo de forma casi completamente sedentaria"; 2 = "Hago alguna actividad física o deportiva ocasional"; 3 = "Hago actividad física varias veces al mes"; 4 = "Hago entrenamiento deportivo o físico varias veces a la semana"; 8 = "No sabe"; 9 = "No contesta"',
                 as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## ATENCION SANITARIA: Falta de atención sanitaria debido a problemas económicos en los últimos 12 meses: Atención médica
datos_ENHS <- within(datos_ENHS, {
  R108_1 <- Recode(R108_1, 
                   '1 = "Sí"; 2 = "No"; 3 = "No lo he necesitado"; 8 = "No sabe"; 9 = "No contesta"',
                   as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## ATENCION SANITARIA: Falta de asistencia médica debido a problemas de transporte en los últimos 12 meses
datos_ENHS <- within(datos_ENHS, {
  R106 <- Recode(R106, 
                 '1 = "Sí"; 2 = "No"; 3 = "No he necesitado asistencia médica"; 8 = "No sabe"; 9 = "No contesta"',
                 as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## ATENCION SANITARIA: Falta de asistencia médica debido a listas de espera en los últimos 12 meses
datos_ENHS <- within(datos_ENHS, {
  R107 <- Recode(R107, 
                 '1 = "Sí"; 2 = "No"; 3 = "No he necesitado asistencia médica"; 8 = "No sabe"; 9 = "No contesta"',
                 as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## MEDICINAS: Medicamentos para la diabetes consumidos
datos_ENHS <- within(datos_ENHS, {
  P87_19a <- Recode(P87_19a, 
                    '1 = "Sí"; 2 = "No"; 8 = "No sabe"; 9 = "No contesta"', as.factor=TRUE, 
                    to.value="=", interval=":", separator=";")
})

## MEDICINAS: Medicinas para el catarro, gripe, garganta, bronquios consumidas
datos_ENHS <- within(datos_ENHS, {
  P87_1a <- Recode(P87_1a, 
                    '1 = "Sí"; 2 = "No"; 8 = "No sabe"; 9 = "No contesta"', as.factor=TRUE, 
                    to.value="=", interval=":", separator=";")
})

## URGENCIAS: Utilización del servicio de urgencias en los últimos 12 meses
datos_ENHS <- within(datos_ENHS, {
  O78 <- Recode(O78, '1 = "Sí"; 2 = "No";', as.factor=TRUE, to.value="=", 
                interval=":", separator=";")
})

## INGRESO: Ingreso en hospital en los últimos 12 meses, excluyendo parto o cesárea (doble enunciado)
datos_ENHS <- within(datos_ENHS, {
  O66 <- Recode(O66, '1 = "Sí"; 2 = "No";', as.factor=TRUE, to.value="=", 
                interval=":", separator=";")
})

## TIEMPO CONSULTA: Tiempo desde la última consulta al médico general o de familia
datos_ENHS <- within(datos_ENHS, {
  N48 <- Recode(N48, 
                '1 = "En las últimas 4 semanas"; 2 = "Entre 4 semanas y 12 meses"; 3 = "Hace 12 meses o más"; 4 = "Nunca"; ;',
                as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## SALUD PERCIBIDA: Estado de salud percibido en los últimos 12 meses
datos_ENHS <- within(datos_ENHS, {
  G21 <- Recode(G21, 
                '1 = "Muy bueno"; 2 = "Bueno"; 3 = "Regular"; 4 = "Malo"; 5 = "Muy malo"', 
                as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## Persona que generó la pensión: Situación profesional en la ocupación que desempeñó
datos_ENHS <- within(datos_ENHS, {
  F10 <- Recode(F10, 
                '1 = "Asalariado/a (a sueldo, comisión, jornal...)"; 2 = "Empresario/a o profesional con asalariados"; 3 = "Empresario/a sin asalariados o trabajador/a independiente"; 4 = "Ayuda familiar (sin remuneración reglamentada en la empresa o negocio de un familiar)"; 5 = "Miembro de una cooperativa"; 6 = "Otra situación"; 8 = "No sabe"; 9 = "No contesta";',
                as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## ESTUDIOS: Nivel de estudios del adulto seleccionado
datos_ENHS <- within(datos_ENHS, {
  ESTUDIOS <- Recode(ESTUDIOS, 
                     '01 = "No procede, es menor de 10 años"; 02 = "No sabe leer o escribir"; 03 = "Educación Primaria incompleta (Ha asistido menos de 5 años a la escuela)"; 04 = "Educación Primaria completa"; 05 = "Primera etapa de Enseñanza Secundaria, con o sin título (2º ESO aprobado, EGB, Bachillerato Elemental)"; 06 = "Estudios de Bachillerato"; 07 = "Enseñanzas profesionales de grado medio o equivalentes"; 08 = "Enseñanzas profesionales de grado superior o equivalentes"; 09 = "Estudios universitarios o equivalentes"',
                     as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## Estado civil
datos_ENHS <- within(datos_ENHS, {
  E4b <- Recode(E4b, 
                '1 = "Soltero/a"; 2 = "Casado/a"; 3 = "Viudo/a"; 4 = "Separado/a legalmente"; 5 = "Divorciado/a"; 8 = "No sabe"; 9 = "No contesta"',
                as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## Convivencia en pareja
datos_ENHS <- within(datos_ENHS, {
  E4 <- Recode(E4, 
               '1 = "Conviviendo con su cónyuge"; 2 = "Conviviendo con una pareja de hecho"; 3 = "No conviviendo en pareja"; 8 = "No sabe"; 9 = "No contesta"',
               as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## Nacionalidad: Española
datos_ENHS <- within(datos_ENHS, {
  E2_1a <- Recode(E2_1a, '1 = "Sí"; 2 = "No";', as.factor=TRUE, to.value="=", 
                interval=":", separator=";")
})

# Sexo
datos_ENHS <- within(datos_ENHS, {
  SEXOa <- Recode(SEXOa, '1 = "Hombre"; 2 = "Mujer";', as.factor=TRUE, 
                  to.value="=", interval=":", separator=";")
})

## Comunidad autonoma
datos_ENHS <- within(datos_ENHS, {
  CCAA <- Recode(CCAA, 
                 '01 = "Andalucía"; 02 = "Aragón"; 03 = "Asturias, Principado de"; 04 = "Balears, Illes"; 05 = "Canarias"; 06 = "Cantabria"; 07 = "Castilla y León"; 08 = "Castilla - La Mancha"; 09 = "Cataluña"; 10 = "Comunitat Valenciana"; 11 = "Extremadura"; 12 = "Galicia"; 13 = "Madrid, Comunidad de"; 14 = "Murcia, Región de"; 15 = "Navarra, Comunidad Foral de"; 16 = "País Vasco"; 17 = "Rioja, La"; 18 = "Ceuta"; 19 = "Melilla";',
                 as.factor=TRUE, to.value="=", interval=":", separator=";")
})

## Seguro público o privado
datos_ENHS <- datos_ENHS %>%
  mutate(Seguro = case_when(
    O84_1 == 1 ~ "Publico",
    O84_2 == 1 | O84_3 == 1 | O84_4 == 1 | O84_5 == 1 ~ "Privado",
    O84_6 == 1 ~ "No seguro",
    TRUE ~ "Otro" # Para cubrir cualquier otro caso no especificado
  ))


## Pseudoterapia si o no
datos_ENHS <- datos_ENHS %>%
  mutate(Pseudo_si_no = case_when(
    N60a_1 == 1 | N60a_2 == 1 | N60a_3 == 1 | N60a_4 == 1 ~ "Pseudoterapia",
    TRUE ~ "Nunca" # Para cubrir cualquier otro caso no especificado
  ))

## Recodificacion de las diferentes pseudoterapias

datos_ENHS <- within(datos_ENHS, {
  N60a_1 <- Recode(N60a_1, '1 = "Sí"; 2 = "No";', as.factor=TRUE, to.value="=", 
                interval=":", separator=";")
})
datos_ENHS <- within(datos_ENHS, {
  N60a_2 <- Recode(N60a_2, '1 = "Sí"; 2 = "No";', as.factor=TRUE, to.value="=", 
                interval=":", separator=";")
})
datos_ENHS <- within(datos_ENHS, {
  N60a_3 <- Recode(N60a_3, '1 = "Sí"; 2 = "No";', as.factor=TRUE, to.value="=", 
                interval=":", separator=";")
})
datos_ENHS <- within(datos_ENHS, {
  N60a_4 <- Recode(N60a_4, '1 = "Sí"; 2 = "No";', as.factor=TRUE, to.value="=", 
                interval=":", separator=";")
})

## Gente que necesita ayuda (combinación de L43-L44)
# ¿Dispone habitualmente de ayuda para realizarlas?
# ¿Necesitaría ayuda o más ayuda de la que dispone? (doble enunciado)

datos_ENHS <- datos_ENHS %>%
  mutate(Nec_ay = case_when(
    L43 == 1 | L44 == 1 ~ "Nec_ayuda",
    L43 == 2 | L44 == 2 ~ "No_ayuda",
    TRUE ~ "NA" # Para cubrir cualquier otro caso no especificado
  ))


## Creacion de la variable Barthel
### Despues de revisar las variables no disponemos de las necesarias ni para el calculo de Barthel ni Lawton y Brody


#Selección de variables y renombrados}

# datos_ENHS <- datos_ENHS %>%
#   dplyr::filter(E4 <= 3, E4b <=5, ESTUDIOS <= 9,F10 <= 6,G24 <= 6, G25a_12 <= 2,G25b_12 <= 2,
#                 G25c_12 <= 2, N58_3 <= 2,N60a_1 <= 2,N60a_2 <= 2,N60a_3 <= 2, N60a_4 <= 2, P87_1a <= 2, P87_19a <= 2, R106 <= 3, R107 <= 3, R108_1 <= 3, T112 <= 4,V121 <= 4, W127 <= 9,
#                 X131 <=5, CLASE_PR <=6,IMC <=4,SEVERIDAD_DEPRESIVA <=5)
datos_ENHS <- datos_ENHS %>%
  dplyr::select(SEXOa,EDADa,E2_1a,E3,E4,E4b,ESTUDIOS,F10,G21,G23,G24,G25c_1,G25a_12,G25b_12,
                G25c_12,N48,N58_3,N60a_1,N60a_2,N60a_3,N60a_4,O66,O78,O84_1,O84_2,O84_3,
                O84_4,O84_5,O84_6,P87_1a, P87_19a,Q88,R106, R107, R108_1, T112,V121,W127,
                X131, CLASE_PR,IMC,SEVERIDAD_DEPRESIVA)  %>% 
  dplyr::rename(
    "Sexo" = "SEXOa",
    "Edad" = "EDADa",
    "Nacionalidad" = "E2_1a",
    "Años de residencia" = "E3",
    "Convivencia en pareja" = "E4",
    "Estado civil" = "E4b",
    "Nivel de estudios" = "ESTUDIOS",
    "Situación profesional" = "F10",
    "Salud_percibida" = "G21",
    "alguna vez diabetes" = "G25a_12",
    "ultimo 12 meses diabetes" = "G25b_12",
    "diagnostico medico diabetes" = "G25c_12",
    "Tiempo ultima visita médica" = "N48",
    "Visita_enfermero/matrona" = "N58_3",
    "Visita_homeopata" = "N60a_1",
    "Visita_acupuntor" = "N60a_2",
    "Visita_naturista" = "N60a_3",
    "Visita_otro med. alternativa" = "N60a_4",
    "Ingreso_hospitalario" = "O78",
    "Serv_emergencias" = "O78",
    "Seguro publico" = "O84_1",
    "Seguro mutual" = "O84_2",
    "Seguro privado" = "O84_4",
    "Medicamentos catarro" = "P87_1a",
    "Medicamentos diabetes" = "P87_19a",
    "Vacuna_gripe" = "Q88",
    "No atencion - Lista de espera" = "R106",
    "No atencion - transporte" = "R107",
    "No atencion  - atencion medica" = "R108_1", 
    "act_fisica" = "T112",
    "Tabaco" = "V121",
    "Bebida_alcohólica" = "W127",
    "Interés de otras personas" = "X131",
    "Clase social" = "CLASE_PR", 
    "IMC_factor" = "IMC",
    "Severidad depresiva" = "SEVERIDAD_DEPRESIVA")

glimpse(datos_ENHS)###similar a str pero muestra aún mas información


#pasar a factor
datos_ENHS[sapply(datos_ENHS, is.integer)] <- lapply(datos_ENHS[sapply(datos_ENHS, is.integer)], 
                                                     as.factor)

# ##renombrar factores
levels(datos_ENHS$Vacuna_gripe) <- c('si',"no")
datos_ENSE$Visita_médica <- with(datos_ENSE, factor(Visita_médica, levels=c('ultimo_mes',"ultimo_ano","mas_1_ano")))
datos_ENSE$Vacuna_gripe <- with(datos_ENSE, factor(Vacuna_gripe, levels=c("si","no")))

#Descriptivos por status
datos_ENHS %>%
  tbl_summary(by = Vacuna_gripe, percent = "row", missing = "no") %>%
  add_overall() %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(label = "**Vacuna influenza**") %>%
  #modify_caption("**Table 1. Patient Characteristics**") %>%
  bold_labels() %>%
  as_gt()
  

# export tbl_summary as CSV
write_csv(as.data.frame(tbl_summary), "tbl_summary.csv")





##################################################################################
##################################################################################################
##################################################################################
#Modelo múltiple
###############
##################################################################################
###################################################################################
ENE_md_multivariante <- glm(
  Vacuna_gripe ~ Visita_médica + Sexo + Grupo_edad + País_nacimiento + IMC_factor + Bebida_alcohólica +
    act_fisica + Estado_laboral + serv_emergencias + Salud_percibida + Tabaco, 
  data = datos_ENSE, na.action = na.exclude, family = binomial(link = 'logit'))

ENSE_md_multivariante %>% summary

#pendientes estimadas
log_odds_multivariante <- tidy(ENSE_md_multivariante, 
                               conf.int = TRUE)
log_odds_multivariante

#Odds Ratios

odds_ratio_multivariante <- tidy(ENSE_md_multivariante,
                                 exponentiate = TRUE,  
                                 conf.int = TRUE)
odds_ratio_multivariante

##Para realizar la interpretración de forma clara podemos reorganizar la tabla como sigue

tab_logistic_multivariante <- bind_cols(log_odds_multivariante, odds_ratio_multivariante)

tab_logistic_multivariante %>%
  dplyr::select(term...1, estimate...2, std.error...3,
                estimate...9, conf.low...13, conf.high...14 ,p.value...5) %>%
  dplyr::rename(covariate = term...1,
                log_odds = estimate...2,
                SE = std.error...3,
                odds_ratio = estimate...9,
                lower_OR = conf.low...13,
                upper_OR = conf.high...14,
                p.val = p.value...5)


#modelo 2 sin emergencias, IMC 
ENSE_md_multivariante_2<- glm(
  Vacuna_gripe ~ Sexo + Grupo_edad + Visita_médica + Estado_laboral + País_nacimiento + 
    Salud_percibida + Bebida_alcohólica + Tabaco,
  data = datos_ENSE, family = binomial(link = 'logit'), na.action = na.exclude)

ENSE_md_multivariante_2 %>% summary

#pendientes estimadas
log_odds_multivariante_2 <- tidy(ENSE_md_multivariante_2, 
                                 conf.int = TRUE)
log_odds_multivariante_2

#Odds Ratios

odds_ratio_multivariante_2 <- tidy(ENSE_md_multivariante_2,
                                   exponentiate = TRUE,  
                                   conf.int = TRUE)
odds_ratio_multivariante_2

##Para realizar la interpretración de forma clara podemos reorganizar la tabla como sigue

tab_logistic_multivariante_2 <- bind_cols(log_odds_multivariante_2, odds_ratio_multivariante_2)

tab_logistic_multivariante_2 %>%
  dplyr::select(term...1, estimate...2, std.error...3,
                estimate...9, conf.low...13, conf.high...14 ,p.value...5) %>%
  dplyr::rename(covariate = term...1,
                log_odds = estimate...2,
                SE = std.error...3,
                odds_ratio = estimate...9,
                lower_OR = conf.low...13,
                upper_OR = conf.high...14,
                p.val = p.value...5)



#Comparación de modelos:
anova(ENSE_md_multivariante, ENSE_md_multivariante_2, test = 'Chisq')
###ANOVA NO FUNCIONA PERO LOS AIC son similares entre los dos modelos, para simplificar me quedaria con el 2.
## TAMBIEN SE PUEDE COMPARAR CON AKAIKE.
#definir lista
modelos<-list(ENSE_md_multivariante,ENSE_md_multivariante_2)
#colocarle nombres
mod.names<-c('ENSE_md_multivariante', 'ENSE_md_multivariante_2')
#calcular AIC
aictab(cand.set=modelos,modnames=mod.names)

############
#Interacción
#############
ENSE_md_multivariante_int <- 
  glm(Vacuna_gripe ~  Visita_médica + Sexo + Grupo_edad + Sexo:Grupo_edad + #act_fisica +
        Salud_percibida + Salud_percibida:Grupo_edad + Grupo_edad:Estado_laboral + Tabaco:Bebida_alcohólica + Tabaco + Bebida_alcohólica + Estado_laboral, 
      data = datos_ENSE, 
      family = binomial(link = 'logit'))
tidy(ENSE_md_multivariante_int)
summary(ENSE_md_multivariante_int)
### de las interacciones evaluadas ninguna es significativa ni epidmeiologicamente relevante.

#Colinealidad Correalciones y VIF
library(rms)

vif(ENSE_md_multivariante_2)
##la unica variable con VIF mayor a 5 es actividad fisica, ver que hacemos.
#Para un VIF de 1 no hay correlación entre las variables predictoras para un VIF entre 1 y 5 hay poco y a
#partir de 5 ya habría colinealidad.

##para unir tablas aunque en este caso no se puede por largos distintos
models_summary <-
  tbl_merge(
    tbls = list(ENSE_md_multivariante %>% tbl_regression(exponentiate = TRUE), 
                ENSE_md_multivariante_2 %>% tbl_regression(exponentiate = TRUE)),
    tab_spanner = c("**OR not adjusted**", "**OR adjusted**")
  ) %>%
  as_gt()
#https://www.danieldsjoberg.com/gtsummary/reference/tbl_merge.html


##################
###PLOTEO modelo 1
install.packages("sjPlot",dependencies = T)
library(sjPlot)
plot_model(ENSE_md_multivariante, type = "est", size = 4, sort.est = FALSE, title = NULL, colors =c("blue","#ED1C24FF"),
           text.size = 12, dot.size = 3, line.size = 0.5, show.data = TRUE, show.values = TRUE, auto.label = FALSE, grid = F, value.offset = .3, 
           vline.color = "red", bpe.color = NULL,ci.style = "whisker")
#, group.terms = c(1, 1, 2, 3, 4, 5, 5, 6, 7, 7, 7, 8, 8, 9, 10, 10, 11, 11)) 

#ploteo modelo 2
plot_model(ENSE_md_multivariante_2, type = "est", size = 4, axis.title = "OR", axis.lim=c(0.2,8) , sort.est = FALSE, title = NULL, colors =c("blue","#ED1C24FF"),
           text.size = 12, dot.size = 3, line.size = 0.5, show.data = TRUE, show.values = TRUE, auto.label = FALSE, grid = F, value.offset = .3, 
           vline.color = "red", bpe.color = NULL,ci.style = "whisker")


#Comparación de modelos:## TAMBIEN SE PUEDE COMPARAR CON AKAIKE.
anova(sedentarismo_data_md1,sedentarismo_data_mm1, test = 'Chisq')

#Interacción
ENSE_md_multivariante_int <- 
  glm(Sedentarismo ~ Sexo + Edad + Sexo:Edad + Edad:Ecronica, 
      data = sedentarismo_data, 
      family = binomial(link = 'logit'))
tidy(sedentarismo_mm1_ia)


#guardar base arreglada
write.csv(datos_ENSE, file = "datos_ENSE_rec.csv") ##como CSV
save(datos_ENSE,file="datos_ENSE_rec.Rdata") ## como R.data



#################################
#Modelo simple binario no ajustado
##visitamedica
ENSE_md_visita <- glm(Vacuna_gripe ~ Visita_médica,  data = datos_ENSE, 
                      family = binomial(link = 'logit'))

ENSE_md_visita %>% summary()
tidy(ENSE_md_visita, conf.int = TRUE)
log_odds_not_adj_visita <- tidy(ENSE_md_visita, conf.int = TRUE)
odds_ratio_not_adj_visita <- tidy(ENSE_md_visita, exponentiate = TRUE, conf.int = TRUE)
tab_logistic_not_adj_visita <- bind_cols(log_odds_not_adj_visita, odds_ratio_not_adj_visita)
tab_logistic_not_adj_visita %>%
  dplyr::select(term...1, estimate...2, std.error...3,
                estimate...9, conf.low...13, conf.high...14 ,p.value...5) %>%
  dplyr::rename(covariate = term...1,
                log_odds = estimate...2,
                SE = std.error...3,
                odds_ratio = estimate...9,
                lower_OR = conf.low...13,
                upper_OR = conf.high...14,
                p.val = p.value...5)
###sexo
ENSE_md_sexo <- glm(Vacuna_gripe ~ Sexo,  data = datos_ENSE, 
                    family = binomial(link = 'logit'))

ENSE_md_sexo %>% summary()
tidy(ENSE_md_sexo, conf.int = TRUE)
log_odds_not_adj_sexo <- tidy(ENSE_md_sexo, conf.int = TRUE)
odds_ratio_not_adj_sexo <- tidy(ENSE_md_sexo, exponentiate = TRUE, conf.int = TRUE)
tab_logistic_not_adj_sexo <- bind_cols(log_odds_not_adj_sexo, odds_ratio_not_adj_sexo)
tab_logistic_not_adj_sexo %>%
  dplyr::select(term...1, estimate...2, std.error...3,
                estimate...9, conf.low...13, conf.high...14 ,p.value...5) %>%
  dplyr::rename(covariate = term...1,
                log_odds = estimate...2,
                SE = std.error...3,
                odds_ratio = estimate...9,
                lower_OR = conf.low...13,
                upper_OR = conf.high...14,
                p.val = p.value...5)

###edad
ENSE_md_edad <- glm(Vacuna_gripe ~ Grupo_edad,  data = datos_ENSE, 
                    family = binomial(link = 'logit'))

ENSE_md_edad %>% summary()
tidy(ENSE_md_edad, conf.int = TRUE)
log_odds_not_adj_edad <- tidy(ENSE_md_edad, conf.int = TRUE)
odds_ratio_not_adj_edad <- tidy(ENSE_md_edad, exponentiate = TRUE, conf.int = TRUE)
tab_logistic_not_adj_edad <- bind_cols(log_odds_not_adj_edad, odds_ratio_not_adj_edad)
tab_logistic_not_adj_edad %>%
  dplyr::select(term...1, estimate...2, std.error...3,
                estimate...9, conf.low...13, conf.high...14 ,p.value...5) %>%
  dplyr::rename(covariate = term...1,
                log_odds = estimate...2,
                SE = std.error...3,
                odds_ratio = estimate...9,
                lower_OR = conf.low...13,
                upper_OR = conf.high...14,
                p.val = p.value...5)

##hola jose bb