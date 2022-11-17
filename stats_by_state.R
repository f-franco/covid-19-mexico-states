library(dplyr)
library(zoo)
library(ggplot2)

dir <- "C:/Users/santa/Desktop/COVID"
setwd(dir)

proy_pob <- read.csv("proyeccion_pob_municipios.csv")

datos <- "201018COVID19MEXICO.csv"
covid19 <- read.csv(datos, stringsAsFactors = FALSE)
fecha_rec <- as.Date(paste("2020", substr(datos, 3, 4), substr(datos, 5, 6), sep = "-"))
estados <- read.csv("Estados Mexico.csv", encoding = "UTF-8")

covid19$FECHA <- as.Date(covid19$FECHA_INGRESO)
covid19$ENTIDAD_RES <- as.factor((covid19$ENTIDAD_RES))
proy_pob$CLAVE_ENT <- as.factor(proy_pob$CLAVE_ENT)
estados$CLAVE <- as.factor(estados$X.U.FEFF.CLAVE)

pob_2020 <- proy_pob %>%
  filter(AÑO == 2020) %>%
  group_by(CLAVE_ENT) %>%
  summarise(POB_TOT_2020  = sum(POB))

plot_data <- covid19 %>%
  filter(FECHA >= "2020-02-28", FECHA < fecha_rec, ENTIDAD_RES %in% c(1:32)) %>%
  group_by(ENTIDAD_RES, FECHA) %>%
  summarise(CASOS_N = sum(RESULTADO_LAB == 1), 
            FALL_N = sum(FECHA_DEF != "9999-99-99")) %>%
  mutate(CASOS_A = do.call(c, tapply(CASOS_N, ENTIDAD_RES, FUN = cumsum)),
         FALL_A = do.call(c, tapply(FALL_N, ENTIDAD_RES, FUN = cumsum)),
         RM_CASOS = rollmean(CASOS_A, k = 7, fill = NA),
         RM_FALL = rollmean(FALL_A, k = 7, fill = NA)) %>%
  left_join(pob_2020, by = c("ENTIDAD_RES" = "CLAVE_ENT")) %>%
  mutate(CASOS_100K = (RM_CASOS / POB_TOT_2020)*100000,
         FALL_100K = (RM_FALL / POB_TOT_2020)*100000) %>%
  left_join(estados, by = c("ENTIDAD_RES" = "CLAVE"))

ggplot(plot_data, aes(FECHA, CASOS_100K, color = REGION)) +
  geom_path(alpha = 0.75) +
  geom_dl(aes(label = X3D, color = REGION), method = list("last.points", rot = 30), alpha = 0.5)

ggplot(plot_data, aes(FECHA, FALL_100K, color = REGION)) +
  geom_path(alpha = 0.75) +
  geom_dl(aes(label = X3D, color = REGION), method = list("last.points", rot = 30), alpha = 0.5)