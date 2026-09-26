# Paso1: cargamos las librerias quq vamos a necesitar 
library(ggplot2)
library(tibble)
library(dplyr)
library(patchwork)

# Paso2: Cargamos todas nuestras funciones hechas anteriormente 
source(here::here("R", "00-lectura.R"))
source(here::here("R", "01-graficos.R"))
source(here::here("R", "02-metodos.R"))
source(here::here("R", "03-evaluacion.R"))

# creamos una seilla para reproducibilidad 
set.seed(123) 


# Ejemplo #1: Media simple

#Paso 1: descripcion de la serie
help(Nile) ##  con las funcion de NILE tenemos 100 observaciones
datos_nile <- leer_serie(Nile, fuente = "R datasets (Nile)", unidad = "10^8 m^3")
datos_nile
attr(datos_nile, "frecuencia")

# Paso 2: creamos los grafico y  el correlograma para ver el comportamiento de la serie
grafico_nile <- graficar_serie(datos_nile, "Caudal anual del rio Nilo")
grafico_nile

correlograma(datos_nile)

# Paso 3: con esta funcion calculamos las   estimacion/validacion 
n_nile <- nrow(datos_nile)
h_nile <- min(12, floor(0.2 * n_nile))  # horizonte de validacion
corte_nile <- n_nile - h_nile

y_estim_nile <- datos_nile$y[1:corte_nile] # extrae la columna del dataframe 
y_valid_nile <- datos_nile$y[(corte_nile+1):n_nile]

ajuste_nile <- ajustar_media(y_estim_nile) # hacemos un ajuste con parametros elegidos

# Paso 5:  creamos una funcion para calcular la medidas de error
medidas_estim_nile <- medidas(y_estim_nile, 
                              ajuste_nile$yhat)
medidas_estim_nile

# pronosticos extramuestrales, contra validacion
## ajuste_nile es el modelo ajustado 
pron_nile <- ajuste_nile$pronosticar(h_nile)
# aqui calculo las métricas de error(lo real vs. lo predicho)
medidas_valid_nile <- medidas(y_valid_nile, 
                              pron_nile, 
                              y_entrenamiento = y_estim_nile)
medidas_valid_nile

# Paso 6: validacion de errores de un paso 
errores_nile <- y_estim_nile - ajuste_nile$yhat #residuo del error 
errores_nile_validos <- errores_nile[!is.na(errores_nile)]

validacion_nile <- validar_errores(
  errores_nile_validos, 
  m = min(floor(length(errores_nile_validos)/4), 24), 
  p = 0) # p = número de parámetros del modelo
# m = número de rezagos 
# a m se le aplicamos las pruebas (Ljung-Box, autocorrelación)

validacion_nile$prueba_t # test t
validacion_nile$ljung_box # test de ljung_Box
validacion_nile$jarque_bera # test jarque_bera
validacion_nile$durbin_watson # estadistico de durbin_watson
validacion_nile$grafico_errores

# Paso 7: referente ingenuo
pron_ingenuo_nile <- rep(y_estim_nile[corte_nile], h_nile) # el vector de pronósticos ingenuos
# con esta funcion calculo las mismas métricas de error
medidas_ingenuo_nile <- medidas(y_valid_nile, 
                                pron_ingenuo_nile, 
                                y_entrenamiento = y_estim_nile)
medidas_ingenuo_nile

# Ljung-Box sobre la serie misma (paso 2 del protocolo)
r_nile <- numeric(min(floor(nrow(datos_nile)/4), 24))
y_n <- datos_nile$y
media_n <- mean(y_n)
Tn <- length(y_n)
for (h in 1:length(r_nile)) {
  r_nile[h] <- (sum((y_n[1:(Tn-h)] - media_n) * (y_n[(1+h):Tn] - media_n)) / Tn) / (sum((y_n-media_n)^2)/Tn)
}
ljung_serie_nile <- ljung_box(r_nile, Tn, length(r_nile), 0)
ljung_serie_nile

# Prueba t, Jarque-Bera, Durbin-Watson sobre los errores (ya calculados en validar_errores)
validacion_nile$prueba_t
validacion_nile$jarque_bera
validacion_nile$durbin_watson


### EJEMPLO 2: Media movil -- LakeHuron

# Paso 1: descripcion de la serie
help(LakeHuron) # esto lo determino la rubrica del trabajo 
datos_lake <- leer_serie(LakeHuron, fuente = "R datasets (LakeHuron)", unidad = "pies")
datos_lake
attr(datos_lake, "frecuencia")

# Paso 2: creamos los grafico y  el correlograma para ver el comportamiento de la serie
grafico_lake <- graficar_serie(datos_lake, "Nivel anual del lago Huron")
grafico_lake
correlograma(datos_lake)

# Paso 3: particion estimacion/validacion 
n_lake <- nrow(datos_lake)
h_lake <- min(12, floor(0.2 * n_lake))
corte_lake <- n_lake - h_lake

y_estim_lake <- datos_lake$y[1:corte_lake]
y_valid_lake <- datos_lake$y[(corte_lake+1):n_lake]

# Paso 4: optimizar k, y ajustar con el optimo 
opt_lake <- optimizar(y_estim_lake, metodo = "mm", rejilla = 2:12)
opt_lake$rejilla_completa
opt_lake$optimo

k_optimo_lake <- opt_lake$optimo$parametro
ajuste_lake <- ajustar_mm(y_estim_lake, 
                          k = k_optimo_lake)

# grafico de la curva de MSE vs k
# k = orden de la media movil
ggplot2::ggplot(opt_lake$rejilla_completa, ggplot2::aes(x = parametro, y = MSE)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = opt_lake$optimo, ggplot2::aes(x = parametro, y = MSE), color = "red", size = 3) +
  ggplot2::labs(title = "MSE vs k (media movil)", x = "k", y = "MSE")

# Paso 5: medidas de error
medidas_estim_lake <- medidas(y_estim_lake, ajuste_lake$yhat)
medidas_estim_lake

pron_lake <- ajuste_lake$pronosticar(h_lake)
medidas_valid_lake <- medidas(y_valid_lake, 
                              pron_lake, 
                              y_entrenamiento = y_estim_lake)
medidas_valid_lake

# Paso 6: validacion de errores de un paso
errores_lake <- y_estim_lake - ajuste_lake$yhat
errores_lake_validos <- errores_lake[!is.na(errores_lake)]
validacion_lake <- validar_errores(errores_lake_validos, 
                                   m = min(floor(length(errores_lake_validos)/4), 24), 
                                   p = 1) # p = numero de parametros del modelo que fueron estimados
validacion_lake$prueba_t
validacion_lake$ljung_box
validacion_lake$jarque_bera
validacion_lake$durbin_watson

# Paso 7: referente ingenuo (comparar contra el modelo)
# creamos un vector de predicciones constantes 
pron_ingenuo_lake <- rep(y_estim_lake[corte_lake], h_lake)
medidas_ingenuo_lake <- medidas(y_valid_lake, 
                                pron_ingenuo_lake, 
                                y_entrenamiento = y_estim_lake)
medidas_ingenuo_lake

# Ljung-Box sobre la serie misma
#calculamos la autocorrelación
r_lake <- numeric(min(floor(nrow(datos_lake)/4), 24))
y_l <- datos_lake$y
media_l <- mean(y_l)
Tl <- length(y_l)
for (h in 1:length(r_lake)) {
  r_lake[h] <- (sum((y_l[1:(Tl-h)] - media_l) * (y_l[(1+h):Tl] - media_l)) / Tl) / (sum((y_l-media_l)^2)/Tl)
}
ljung_serie_lake <- ljung_box(r_lake, Tl, length(r_lake), 0)
ljung_serie_lake



### EJEMPLO 3: SES -- discoveries


# Paso 1: descripcion de la serie 
help(discoveries)
datos_disc <- leer_serie(discoveries, fuente = "R datasets (discoveries)", unidad = "numero de inventos")
datos_disc
attr(datos_disc, "frecuencia")

# Paso 2: grafico y correlograma
grafico_disc <- graficar_serie(datos_disc, "Grandes inventos por año")
grafico_disc
correlograma(datos_disc)

# Paso 3: particion estimacion/validacion
n_disc <- nrow(datos_disc)
h_disc <- min(12, floor(0.2 * n_disc))
corte_disc <- n_disc - h_disc

y_estim_disc <- datos_disc$y[1:corte_disc]
y_valid_disc <- datos_disc$y[(corte_disc+1):n_disc]

# Paso 4: optimizar alpha, y ajustar con el optimo

opt_disc <- optimizar(y_estim_disc, metodo = "ses", rejilla = seq(0.02, 0.98, by = 0.02))
opt_disc$optimo
# esta funcion es para elegir el mejor valor del parámetro alpha
alpha_optimo_disc <- opt_disc$optimo$parametro
ajuste_disc <- ajustar_ses(y_estim_disc, 
                           alpha = alpha_optimo_disc)

# curva de MSE vs alpha
ggplot2::ggplot(opt_disc$rejilla_completa, ggplot2::aes(x = parametro, y = MSE)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = opt_disc$optimo, ggplot2::aes(x = parametro, y = MSE), color = "red", size = 3) +
  ggplot2::labs(title = "MSE vs alpha (SES)", x = "alpha", y = "MSE")
# el punto rojo es para saber ¿cuál es el mejor alpha?

# Paso 5: medidas de error
medidas_estim_disc <- medidas(y_estim_disc, 
                              ajuste_disc$yhat)
medidas_estim_disc

pron_disc <- ajuste_disc$pronosticar(h_disc)
medidas_valid_disc <- medidas(y_valid_disc, 
                              pron_disc, 
                              y_entrenamiento = y_estim_disc)
medidas_valid_disc

# Paso 6: validacion de errores de un paso
errores_disc <- y_estim_disc - ajuste_disc$yhat
errores_disc_validos <- errores_disc[!is.na(errores_disc)]
validacion_disc <- validar_errores(errores_disc_validos, 
                                   m = min(floor(length(errores_disc_validos)/4), 24), 
                                   p = 1)
validacion_disc$prueba_t
validacion_disc$ljung_box
validacion_disc$jarque_bera
validacion_disc$durbin_watson

# Paso 7: referente ingenuo
pron_ingenuo_disc <- rep(y_estim_disc[corte_disc], h_disc)
medidas_ingenuo_disc <- medidas(y_valid_disc, 
                                pron_ingenuo_disc, 
                                y_entrenamiento = y_estim_disc)
medidas_ingenuo_disc

# Ljung-Box sobre la serie misma
#calculamos la autocorrelación
r_disc <- numeric(min(floor(nrow(datos_disc)/4), 24))
y_d <- datos_disc$y
media_d <- mean(y_d)
Td <- length(y_d)
for (h in 1:length(r_disc)) {
  r_disc[h] <- (sum((y_d[1:(Td-h)] - media_d) * (y_d[(1+h):Td] - media_d)) / Td) / (sum((y_d-media_d)^2)/Td)
}
ljung_serie_disc <- ljung_box(r_disc, Td, length(r_disc), 0)
ljung_serie_disc


### EJEMPLO 4: Doble media movil -- airmiles

# Paso 1: descripcion de la serie
help(airmiles)
datos_air <- leer_serie(airmiles, fuente = "R datasets (airmiles)", unidad = "millas")
datos_air
attr(datos_air, "frecuencia")

# Paso 2: grafico y correlograma
grafico_air <- graficar_serie(datos_air, "Millas voladas por aerolineas EE.UU.")
grafico_air
correlograma(datos_air)

# Paso 3: particion estimacion/validacion
n_air <- nrow(datos_air)
h_air <- min(12, floor(0.2 * n_air))
corte_air <- n_air - h_air

y_estim_air <- datos_air$y[1:corte_air]
y_valid_air <- datos_air$y[(corte_air+1):n_air]

# Paso 4: optimizar k, y ajustar con el optimo
opt_air <- optimizar(y_estim_air, 
                     metodo = "dmm", 
                     rejilla = 2:10)
opt_air$optimo

k_optimo_air <- opt_air$optimo$parametro
ajuste_air <- ajustar_dmm(y_estim_air, 
                          k = k_optimo_air)

# curva de MSE vs k
ggplot2::ggplot(opt_air$rejilla_completa, 
                ggplot2::aes(x = parametro, y = MSE)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = opt_air$optimo, ggplot2::aes(x = parametro, y = MSE), color = "red", size = 3) +
  ggplot2::labs(title = "MSE vs k (doble media movil)", x = "k", y = "MSE")

# Paso 5: medidas de error
medidas_estim_air <- medidas(y_estim_air, ajuste_air$yhat)
medidas_estim_air

pron_air <- ajuste_air$pronosticar(h_air)
medidas_valid_air <- medidas(y_valid_air, 
                             pron_air, 
                             y_entrenamiento = y_estim_air)
medidas_valid_air

# Paso 6: validacion de errores de un paso
errores_air <- y_estim_air - ajuste_air$yhat
errores_air_validos <- errores_air[!is.na(errores_air)]
validacion_air <- validar_errores(errores_air_validos, m = min(floor(length(errores_air_validos)/4), 24), p = 2)
validacion_air$prueba_t
validacion_air$ljung_box
validacion_air$jarque_bera
validacion_air$durbin_watson

# Paso 7: referente ingenuo
pron_ingenuo_air <- rep(y_estim_air[corte_air], h_air)
medidas_ingenuo_air <- medidas(y_valid_air, 
                               pron_ingenuo_air, 
                               y_entrenamiento = y_estim_air)
medidas_ingenuo_air


### EJEMPLO 5: Tendencia lineal -- austres

# Paso 1: descripcion de la seri
help(austres)
datos_aus <- leer_serie(austres, fuente = "R datasets (austres)", unidad = "miles de personas")
datos_aus
attr(datos_aus, "frecuencia")

# Paso 2: grafico y correlograma
grafico_aus <- graficar_serie(datos_aus, "Poblacion trimestral de Australia")
grafico_aus
correlograma(datos_aus)

# Paso 3: particion estimacion/validacion
n_aus <- nrow(datos_aus)
h_aus <- min(12, floor(0.2 * n_aus))
corte_aus <- n_aus - h_aus

y_estim_aus <- datos_aus$y[1:corte_aus]
y_valid_aus <- datos_aus$y[(corte_aus+1):n_aus]

# Paso 4: ajuste (tendencia lineal no tiene parametro que optimizar con rejilla)
ajuste_aus <- ajustar_tendencia(y_estim_aus, "lineal")
ajuste_aus$parametros$tabla

# Paso 5: medidas de error
medidas_estim_aus <- medidas(y_estim_aus, ajuste_aus$yhat)
medidas_estim_aus

pron_aus <- ajuste_aus$pronosticar(h_aus)
medidas_valid_aus <- medidas(y_valid_aus, 
                             pron_aus, 
                             y_entrenamiento = y_estim_aus)
medidas_valid_aus

# Paso 6: validacion de errores
errores_aus <- y_estim_aus - ajuste_aus$yhat
validacion_aus <- validar_errores(errores_aus, m = min(floor(length(errores_aus)/4), 24), p = 2)
validacion_aus$prueba_t
validacion_aus$ljung_box
validacion_aus$jarque_bera
validacion_aus$durbin_watson

# Paso 7: referente ingenuo
pron_ingenuo_aus <- rep(y_estim_aus[corte_aus], h_aus)
medidas_ingenuo_aus <- medidas(y_valid_aus, 
                               pron_ingenuo_aus, 
                               y_entrenamiento = y_estim_aus)
medidas_ingenuo_aus


# EJEMPLO 6: Tendencia cuadratica -- co2

# Paso 1: descripcion de la serie
help(co2)
datos_co2 <- leer_serie(co2, fuente = "R datasets (co2, Mauna Loa)", unidad = "ppm")
datos_co2
attr(datos_co2, "frecuencia")

# Paso 2: grafico y correlograma
grafico_co2 <- graficar_serie(datos_co2, "CO2 atmosferico, Mauna Loa")
grafico_co2
correlograma(datos_co2)

# Paso 3: particion estimacion/validacion
n_co2 <- nrow(datos_co2)
h_co2 <- min(12, floor(0.2 * n_co2))
corte_co2 <- n_co2 - h_co2

y_estim_co2 <- datos_co2$y[1:corte_co2]
y_valid_co2 <- datos_co2$y[(corte_co2+1):n_co2]

# Paso 4: ajuste
ajuste_co2 <- ajustar_tendencia(y_estim_co2, "cuadratica")
ajuste_co2$parametros$tabla

# Paso 5: medidas de error
medidas_estim_co2 <- medidas(y_estim_co2, ajuste_co2$yhat)
medidas_estim_co2

pron_co2 <- ajuste_co2$pronosticar(h_co2)
medidas_valid_co2 <- medidas(y_valid_co2, pron_co2, y_entrenamiento = y_estim_co2)
medidas_valid_co2

# Paso 6: validacion de errores
errores_co2 <- y_estim_co2 - ajuste_co2$yhat
validacion_co2 <- validar_errores(errores_co2, 
                                  m = min(floor(length(errores_co2)/4), 24), 
                                  p = 3)
validacion_co2$prueba_t
validacion_co2$ljung_box
validacion_co2$jarque_bera
validacion_co2$durbin_watson

# Paso 7: referente ingenuo
pron_ingenuo_co2 <- rep(y_estim_co2[corte_co2], h_co2)
medidas_ingenuo_co2 <- medidas(y_valid_co2, pron_ingenuo_co2, y_entrenamiento = y_estim_co2)
medidas_ingenuo_co2


### EJEMPLO 7: Tendencia exponencial -- JohnsonJohnson

# Paso 1: descripcion de la serie
help(JohnsonJohnson)
datos_jj <- leer_serie(JohnsonJohnson, fuente = "R datasets (JohnsonJohnson)", unidad = "USD por accion")
datos_jj
attr(datos_jj, "frecuencia")

# Paso 2: grafico y correlograma
grafico_jj <- graficar_serie(datos_jj, "Ganancias trimestrales Johnson & Johnson")
grafico_jj
correlograma(datos_jj)

# Paso 3: particion estimacion/validacion
n_jj <- nrow(datos_jj)
h_jj <- min(12, floor(0.2 * n_jj))
corte_jj <- n_jj - h_jj

y_estim_jj <- datos_jj$y[1:corte_jj]
y_valid_jj <- datos_jj$y[(corte_jj+1):n_jj]

# Paso 4: ajuste
ajuste_jj <- ajustar_tendencia(y_estim_jj, "exponencial")
ajuste_jj$parametros$tabla

# Paso 5: medidas de error
medidas_estim_jj <- medidas(y_estim_jj, ajuste_jj$yhat)
medidas_estim_jj

pron_jj <- ajuste_jj$pronosticar(h_jj)
medidas_valid_jj <- medidas(y_valid_jj, pron_jj, y_entrenamiento = y_estim_jj)
medidas_valid_jj

# Paso 6: validacion de errores
errores_jj <- y_estim_jj - ajuste_jj$yhat
validacion_jj <- validar_errores(errores_jj, m = min(floor(length(errores_jj)/4), 24), p = 2)
validacion_jj$prueba_t
validacion_jj$ljung_box
validacion_jj$jarque_bera
validacion_jj$durbin_watson

# Paso 7: referente ingenuo
pron_ingenuo_jj <- rep(y_estim_jj[corte_jj], h_jj)
medidas_ingenuo_jj <- medidas(y_valid_jj, 
                              pron_ingenuo_jj, 
                              y_entrenamiento = y_estim_jj)
medidas_ingenuo_jj



# EJEMPLO 8: Holt lineal -- WWWusage

# Paso 1: descripcion de la serie
help(WWWusage)
datos_www <- leer_serie(WWWusage, fuente = "R datasets (WWWusage)", unidad = "usuarios conectados")
datos_www
attr(datos_www, "frecuencia")

# Paso 2: grafico y correlograma
grafico_www <- graficar_serie(datos_www, "Uso de internet, minuto a minuto")
grafico_www
correlograma(datos_www)

# Paso 3: particion estimacion/validacion
n_www <- nrow(datos_www)
h_www <- min(12, floor(0.2 * n_www))
corte_www <- n_www - h_www

y_estim_www <- datos_www$y[1:corte_www]
y_valid_www <- datos_www$y[(corte_www+1):n_www]

# Paso 4: optimizar (alpha, beta), y ajustar con el optimo
opt_www <- optimizar(y_estim_www, 
                     metodo = "holt", 
                     rejilla = seq(0.05, 0.95, 
                                   by = 0.05))
opt_www$optimo

alpha_opt_www <- opt_www$optimo$alpha
beta_opt_www <- opt_www$optimo$beta
ajuste_www <- ajustar_holt(y_estim_www, 
                           alpha = alpha_opt_www, 
                           beta = beta_opt_www)

# mapa de calor de MSE sobre (alpha, beta)
ggplot2::ggplot(opt_www$rejilla_completa, ggplot2::aes(x = alpha, y = beta, fill = MSE)) +
  ggplot2::geom_tile() +
  ggplot2::geom_point(data = opt_www$optimo, ggplot2::aes(x = alpha, y = beta), color = "red", size = 3, inherit.aes = FALSE) +
  ggplot2::labs(title = "MSE sobre la rejilla (alpha, beta) - Holt", x = "alpha", y = "beta")

# Paso 5: medidas de error
medidas_estim_www <- medidas(y_estim_www, ajuste_www$yhat)
medidas_estim_www

pron_www <- ajuste_www$pronosticar(h_www)
medidas_valid_www <- medidas(y_valid_www, pron_www, y_entrenamiento = y_estim_www)
medidas_valid_www

# Paso 6: validacion de errores
errores_www <- y_estim_www - ajuste_www$yhat
errores_www_validos <- errores_www[!is.na(errores_www)]
validacion_www <- validar_errores(errores_www_validos, m = min(floor(length(errores_www_validos)/4), 24), p = 2)
validacion_www$prueba_t
validacion_www$ljung_box
validacion_www$jarque_bera
validacion_www$durbin_watson

# Paso 7: referente ingenuo
pron_ingenuo_www <- rep(y_estim_www[corte_www], h_www)
medidas_ingenuo_www <- medidas(y_valid_www, pron_ingenuo_www, y_entrenamiento = y_estim_www)
medidas_ingenuo_www


### EJEMPLO 9 (CONTRAEJEMPLO): Media simple -- AirPassengers

# Paso 1: descripcion (ya cargada como 'resultado')
resultado
attr(resultado, "frecuencia")

# Paso 3: particion estimacion/validacion
n_cx <- nrow(resultado)
h_cx <- min(12, floor(0.2 * n_cx))
corte_cx <- n_cx - h_cx

y_estim_cx <- resultado$y[1:corte_cx]
y_valid_cx <- resultado$y[(corte_cx+1):n_cx]

# Paso 4: ajuste (media simple no tiene parametros)
ajuste_cx <- ajustar_media(y_estim_cx)

# Paso 5: medidas de error
medidas_estim_cx <- medidas(y_estim_cx, ajuste_cx$yhat)
medidas_estim_cx

pron_cx <- ajuste_cx$pronosticar(h_cx)
medidas_valid_cx <- medidas(y_valid_cx, pron_cx, y_entrenamiento = y_estim_cx)
medidas_valid_cx

# Paso 6: validacion de errores
errores_cx <- y_estim_cx - ajuste_cx$yhat
errores_cx_validos <- errores_cx[!is.na(errores_cx)]
validacion_cx <- validar_errores(errores_cx_validos, m = min(floor(length(errores_cx_validos)/4), 24), p = 0)
validacion_cx$prueba_t
validacion_cx$ljung_box
validacion_cx$jarque_bera
validacion_cx$durbin_watson
validacion_cx$grafico_correlograma  # correlograma de los errores

# Paso 7: referente ingenuo
pron_ingenuo_cx <- rep(y_estim_cx[corte_cx], h_cx)
medidas_ingenuo_cx <- medidas(y_valid_cx, pron_ingenuo_cx, y_entrenamiento = y_estim_cx)
medidas_ingenuo_cx

# ============================================================
# GUARDAR TODAS LAS FIGURAS EN figs/
# ============================================================
dir.create("figs", showWarnings = FALSE)

# Series y correlogramas
ggsave("figs/01_nile_serie.png",         grafico_nile, width = 8, height = 4)
ggsave("figs/01_nile_correlograma.png", correlograma(datos_nile), width = 8, height = 6)
ggsave("figs/02_lake_serie.png",         grafico_lake, width = 8, height = 4)
ggsave("figs/02_lake_correlograma.png", correlograma(datos_lake), width = 8, height = 6)
ggsave("figs/03_disc_serie.png",         grafico_disc, width = 8, height = 4)
ggsave("figs/03_disc_correlograma.png", correlograma(datos_disc), width = 8, height = 6)
ggsave("figs/04_air_serie.png",          grafico_air,  width = 8, height = 4)
ggsave("figs/04_air_correlograma.png",  correlograma(datos_air),  width = 8, height = 6)
ggsave("figs/05_aus_serie.png",          grafico_aus,  width = 8, height = 4)
ggsave("figs/05_aus_correlograma.png",  correlograma(datos_aus),  width = 8, height = 6)
ggsave("figs/06_co2_serie.png",          grafico_co2,  width = 8, height = 4)
ggsave("figs/06_co2_correlograma.png",  correlograma(datos_co2),  width = 8, height = 6)
ggsave("figs/07_jj_serie.png",           grafico_jj,   width = 8, height = 4)
ggsave("figs/07_jj_correlograma.png",   correlograma(datos_jj),   width = 8, height = 6)
ggsave("figs/08_www_serie.png",          grafico_www,  width = 8, height = 4)
ggsave("figs/08_www_correlograma.png",  correlograma(datos_www),  width = 8, height = 6)

# Errores y correlograma de errores (de validar_errores)
ggsave("figs/01_nile_errores.png", validacion_nile$grafico_errores, width = 8, height = 4)
ggsave("figs/02_lake_errores.png", validacion_lake$grafico_errores, width = 8, height = 4)
ggsave("figs/03_disc_errores.png", validacion_disc$grafico_errores, width = 8, height = 4)
ggsave("figs/04_air_errores.png",  validacion_air$grafico_errores,  width = 8, height = 4)
ggsave("figs/05_aus_errores.png",  validacion_aus$grafico_errores,  width = 8, height = 4)
ggsave("figs/06_co2_errores.png",  validacion_co2$grafico_errores,  width = 8, height = 4)
ggsave("figs/07_jj_errores.png",   validacion_jj$grafico_errores,   width = 8, height = 4)
ggsave("figs/08_www_errores.png",  validacion_www$grafico_errores,  width = 8, height = 4)
ggsave("figs/09_cx_errores.png",   validacion_cx$grafico_errores,   width = 8, height = 4)
