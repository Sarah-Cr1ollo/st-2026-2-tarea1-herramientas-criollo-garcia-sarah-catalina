##  buscamos la media simple (recursiva) 

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

## vamos a trabajar la funcion de la media movil de orden k

ajustar_mm <- function(y, k) {
  stopifnot(is.numeric(y), !any(is.na(y)), k >= 2, k <= length(y))
  
  n <- length(y)
  yhat <- rep(NA_real_, n)
  
  # creamos la suma de los primeros k valores
  suma_movil <- sum(y[1:k])
  yhat[k + 1] <- suma_movil / k
  
  # calculamos yhat[t+1] = MM_t(k), valido para t >= k
  if (n > k + 1) {
    for (t in (k + 1):(n - 1)) {
      suma_movil <- suma_movil - y[t - k] + y[t]
      yhat[t + 1] <- suma_movil / k
    }
  }
  
  mm_final <- mean(y[(n - k + 1):n])
  
  pronosticar <- function(h) {
    stopifnot(h >= 1)
    rep(mm_final, h) # repite un valor especifico varias veces
    
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(k = k, mm_final = mm_final)
  )
}

## vamos a crear la función para el suavizamiento exponencial 

ajustar_ses <- function(y, alpha) {
  stopifnot(is.numeric(y), !any(is.na(y)), alpha > 0, alpha < 1, length(y) >= 2)
  n <- length(y)
  
  # --- Calculo principal: forma de correccion de error ---
  # yhat_{t+1} = yhat_t + alpha * e_t, donde e_t = y_t - yhat_t
  yhat <- rep(NA_real_, n)
  yhat[2] <- y[1]
  for (t in 2:(n - 1)) {
    e_t <- y[t] - yhat[t]
    yhat[t + 1] <- yhat[t] + alpha * e_t
  }
  e_n <- y[n] - yhat[n]
  yhat_T1 <- yhat[n] + alpha * e_n  # pronostico para T+1
  
  # --- Verificacion: forma de promedio ponderado debe dar lo mismo ---
  yhat_pond <- rep(NA_real_, n)
  yhat_pond[2] <- y[1]
  for (t in 2:(n - 1)) {
    yhat_pond[t + 1] <- alpha * y[t] + (1 - alpha) * yhat_pond[t]
  }
  diferencia <- max(abs(yhat[3:n] - yhat_pond[3:n]))
  stopifnot(diferencia < 1e-10)
  
  pronosticar <- function(h) {
    stopifnot(h >= 1)
    rep(yhat_T1, h)  # SES es plano hacia adelante
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(alpha = alpha, yhat_final = yhat_T1)
  )
}
