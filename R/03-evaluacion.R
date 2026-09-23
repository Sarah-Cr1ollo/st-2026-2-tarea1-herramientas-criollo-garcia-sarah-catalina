# vamos a empezar solucionar la ecuacion 1 
ljung_box <- function(r, T, m, p) {
  #suma rezagos  
  terminos <- r^2 / (T - (1:m))
  Q <- T * (T + 2) * sum(terminos)
  #grados de libertad
  gl <- m - p
  #buscamos el valor de la distri chi-cuadrado que deja 5% de probab
  valor_critico <- stats::qchisq(0.95, df = gl)
  # con el p valor obtenemos las probabilidad acumulada 
  valor_p <- stats::pchisq(Q, df = gl, lower.tail = FALSE)
  resultado <- list(
    estadistico = Q,
    gl = gl,
    valor_critico = valor_critico,
    valor_p = valor_p
  )
  return(resultado)
}
# solucionamos la ecuacion 2  
jarque_bera <- function(e) {
  N <- length(e)
  e_media <- mean(e)
  varianza <- sum((e - e_media)^2) / N
  # potencia
  A <- (sum((e - e_media)^3) / N) / (varianza^(3/2))
  K <- (sum((e - e_media)^4) / N) / (varianza^2)
  #ecuación de  JB
  JB <- (N/6) * (A^2 + ((K-3)^2)/4)
  #grados de libertad
  gl <- 2 
  
  valor_critico <- stats::qchisq(0.95, df=2)
  valor_p <- stats::pchisq(JB, df = 2, lower.tail = FALSE)
  
  resultado <- list(
    estadistico = JB,
    gl = gl,
    valor_critico = valor_critico,
    valor_p = valor_p
  )
  return(resultado)
}

# solucionamos la ecuacion 3
durbin_watson <- function(e) {
  
  T <- length(e)
  
  numerador <- sum((e[2:T] - e[1:(T-1)])^2)
  denominador <- sum(e^2)
  
  d <- numerador / denominador
  
  return(list(
    estadistico = d,
    gl = NA,
    valor_critico = NA,
    valor_p = NA
  ))
}

# prueba t de media cero donde vamos a validar el error

validar_errores <- function(e, m, p) {
  prueba_t <- stats::t.test(e, mu = 0)
  
  T_e <- length(e)
  e_media <- mean(e)
  m_usar <- min(floor(T_e/4), 24)
  
  r_errores <- numeric(m_usar)
  for (h in 1:m_usar) {
    r_errores[h] <- (sum((e[1:(T_e-h)] - e_media) * (e[(1+h):T_e] - e_media)) / T_e) / (sum((e-e_media)^2)/T_e)
  }
  
  resultado_lb <- ljung_box(r_errores, T_e, m_usar, p)
  resultado_jb <- jarque_bera(e)
  resultado_dw <- durbin_watson(e)
  # grafico de los errores de tiempo 
  tabla_e <- tibble::tibble(t = seq_len(T_e), error = e)
  grafico_errores <- ggplot2::ggplot(tabla_e, ggplot2::aes(x = t, y = error)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Errores en el tiempo", x = "t", y = "Error")
  #para el correlograma podemos usar la informacion anteriormente obtenida 
  tabla_para_correlograma <- tibble::tibble(y = e)
  grafico_correlograma_e <- correlograma(tabla_para_correlograma, m = m_usar)
  
  return(list(
    prueba_t = prueba_t,
    ljung_box = resultado_lb,
    jarque_bera = resultado_jb,
    durbin_watson = resultado_dw,
    grafico_errores = grafico_errores,
    grafico_correlograma = grafico_correlograma_e
  ))
}

# creacion de medias (MSE, MAD, MAPE, MASE)
medidas <- function(y_obs, y_pred, y_entrenamiento = NULL) {
  

  
  stopifnot(length(y_obs) == length(y_pred))
  
  # Quitar NAs (por ejemplo, posiciones de calentamiento)
  validos <- !is.na(y_pred) & !is.na(y_obs)
  y_obs <- y_obs[validos]
  y_pred <- y_pred[validos]
  n <- length(y_obs)
  
  e <- y_obs - y_pred
  
  MSE <- mean(e^2)
  MAD <- mean(abs(e))
  MAPE <- 100 * mean(abs(e / y_obs))
  
  # MASE necesita el MAD del ingenuo dentro del tramo de ESTIMACION
  if (!is.null(y_entrenamiento)) {
    e_ingenuo <- diff(y_entrenamiento)  # Y_t - Y_{t-1}
    MAD_ingenuo <- mean(abs(e_ingenuo))
    MASE <- MAD / MAD_ingenuo
  } else {
    MASE <- NA
  }
  return(list(
    MSE = MSE,
    MAD = MAD,
    MAPE = MAPE,
    MASE = MASE,
    n = n
  ))
}
