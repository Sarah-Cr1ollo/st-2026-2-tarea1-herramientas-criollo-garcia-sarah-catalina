#  buscamos la media simple (recursiva) 
ajustar_media <- function(y) {
  #usamos is.numeric(y) para convierte a y en un vector
  # necesitamos que !any(is.na(y) para que no aparezca ningun NA
  stopifnot(is.numeric(y), !any(is.na(y)), length(y) >= 2)
  # serie con al menos 2 obser.
  n <- length(y)
  yhat <- rep(NA_real_, n)
  yhat[2] <- y[1]
  # esta funcion la exigue el enuenciado 
  ybarra <- y[1]
  if (n >= 3) {
    for (t in 2:(n - 1)) {
      ybarra <- ybarra + (y[t] - ybarra) / t
      yhat[t + 1] <- ybarra
    }
  }
  # ybarra final = media de toda la muestra de estimacion
  ybarra_final <- mean(y)
  # con este bloque vamos a calcular la media  de toda la serie
  pronosticar <- function(h) {
    stopifnot(h >= 1)
    rep(ybarra_final, h)
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(media_final = ybarra_final)
  )
}
