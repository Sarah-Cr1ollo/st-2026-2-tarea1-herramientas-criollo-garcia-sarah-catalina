# vamos a empezar solucionar la ecuacion 
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