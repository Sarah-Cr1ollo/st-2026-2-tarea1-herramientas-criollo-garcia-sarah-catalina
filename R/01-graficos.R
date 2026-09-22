#instalamos los paquete necesarios para la creación de las graficas 
library(ggplot2)
library(tibble)
library(dplyr)
library(patchwork)

#creamos un grafico donde vemos el comportamiento de los numero de pasajeros usan esa areolinea por año
graficar_serie <- function(datos, titulo) {
  # funcion para guardar todo lo que vamos a  hacer 
  unidad <- attr(datos, "unidad")
  fuente <- attr(datos, "fuente")
  n <- nrow(datos)
  #usamos la funcion ggplot  para la creacion de el grafico
  grafico <- ggplot2::ggplot(datos, aes(x = fecha, y = y)) +
    ggplot2::geom_line() + # creacion de barras verticales 
    ggplot2::labs(
      title = titulo,
      x = "Fecha",
      y = unidad,
      caption = paste("Fuente:", fuente, ". n =", n, "observaciones")
    ) +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "2 years")
  
  return(grafico)
}

# miraremos el comportamiento de estos datos por medio del correlograma 

# tenemos que calcular la autocorrelacion a mano
correlograma <- function(datos, m = min(floor(nrow(datos)/4), 24)) {
  # en la teoria trabajada en clase tenemos m
  y <- datos$y
  T_obs <- length(y)
  #calculamos la media 
  y_media <- mean(y)
  
  #vamos a hacer la formula por partes 
  #parte 1: codigo del denominador de la ecuación Rh
  denominador <- sum((y - y_media)^2) / T_obs # media
  
  #parte 2: codigo para el numerador
  r <- numeric(m)
  for (h in 1:m) {
    numerador <- sum((y[1:(T_obs - h)] - y_media) * (y[(1 + h):T_obs] - y_media)) / T_obs
    #paso 3: terminamos de calcular  la autocorrelación
    r[h] <- numerador / denominador
  }
  
  #comparamos contra la función acf, para verificar que el cálculo a mano esté bien
  acf_verificacion <- stats::acf(y, plot = FALSE, lag.max = m)$acf[-1]
  diferencia_max <- max(abs(r - acf_verificacion))
  
  pacf_valores <- stats::pacf(y, plot = FALSE, lag.max = m)$acf
  banda <- stats::qnorm(0.975) / sqrt(T_obs)
  # contruimos un tibble con los rezagos 
  tabla_acf <- tibble::tibble(h = 1:m, valor = r)
  tabla_pacf <- tibble::tibble(h = 1:m, valor = pacf_valores)
  
  # creacion de grafico de ACF
  grafico_acf <- ggplot2::ggplot(tabla_acf, ggplot2::aes(x = h, y = valor)) +
    ggplot2::geom_col(width = 0.1) + # width crea lineas verticales 
    ggplot2::geom_hline(yintercept = c(-banda, banda), linetype = "dashed", color = "blue") +
    ggplot2::labs(title = "ACF", x = "Rezago", y = "ACF")
  
  # creacion de grafico de PACF
  grafico_pacf <- ggplot2::ggplot(tabla_pacf, ggplot2::aes(x = h, y = valor)) +
    ggplot2::geom_col(width = 0.1) + # width crea lineas verticales 
    ggplot2::geom_hline(yintercept = c(-banda, banda), linetype = "dashed", color = "blue") +
    ggplot2::labs(title = "PACF", x = "Rezago", y = "PACF")
  #combinamos los graficos 
  panel <- grafico_acf / grafico_pacf
  
  
  return(panel)
}
