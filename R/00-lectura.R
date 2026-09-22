# el trabajo nos pide una función devuelva un tibble con columnas t, fecha, y
leer_serie <- function(x, fuente, unidad) { 
  # aqui esta el codigo donde x pasa a ser un ts 
  if (is.ts(x)) {
  # usamos stats para saber el punto exacto donde va a empezar la serie 
    frec <- stats::frequency(x)
    y <- as.numeric(x)
    n <- length(y)
  # aqui obtenemos las variables año y periodo
    inicio <-  stats::start(x)
  # aqui hacemos una comparacion de un valor contra varias opciones posibles 
    unidad_time <- switch(
      as.character(frec),
      "12" = "month",
      "4" = "quarter",
      "1" = "year",
      "365" = "day",
      stop("Frecuencia no reconocida: ", frec)
    )
    f_inicio <- as.Date(paste(inicio[1], inicio[2], 1, sep = "-"))
    fecha <- seq(f_inicio, by = unidad_time, length.out = n)
    
    
    } else {
      #leemos el archivo 
      datos_csv <- read.csv(x)
      y <- datos_csv$valor
      fecha <- as.Date(datos_csv$fecha)
      n <- length(y)
    }
  # creamos la función que arma una tabla tipo tibble
  diferencias <- diff(fecha)
  if (!all(diferencias > 0)) {
    stop("Las fechas no son crecientes")
  }
  promedio_dif <- mean(diferencias)
  if (any(abs(diferencias - promedio_dif) > 3)) {
    stop("Las fechas no están equiespaciadas")
  }
  t <- seq_len(n)
  datos <- tibble::tibble(t = t, fecha = fecha, y = y)
  attr(datos, "frecuencia") <- frec
  attr(datos, "fuente") <- fuente
  attr(datos, "unidad") <- unidad
  
  return(datos)
  
  }


