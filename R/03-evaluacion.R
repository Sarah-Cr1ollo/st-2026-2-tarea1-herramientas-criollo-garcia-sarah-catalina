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