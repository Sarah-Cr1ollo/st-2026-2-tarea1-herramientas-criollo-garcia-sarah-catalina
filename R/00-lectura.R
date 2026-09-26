# el trabajo nos pide una función devuelva un tibble con columnas t, fecha, y
leer_serie <- function(x, fuente, unidad) { 
  
  stopifnot(
    "error la fuente debe ser texto" = is.character(fuente),
    "error la unidad debe ser texto" = is.character(unidad),
    "x debe ser un objeto ts o una ruta de texto" = is.ts(x) || is.character(x)
  )
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
    f_inicio <- as.Date(sprintf("%04d-%02d-%02d", inicio[1], inicio[2], 1))
    fecha <- seq(f_inicio, by = unidad_time, length.out = n)
    
    
    } else {
      #leemos el archivo 
      datos_csv <- read.csv(x)
      y <- datos_csv$valor
      fecha <- as.Date(datos_csv$fecha)
      dif_promedio <- mean(diff(fecha))
      frec <- dplyr::case_when(
        dif_promedio < 1 ~ 365,
        dif_promedio < 35 ~ 12,
        dif_promedio < 91 ~ 4,
        TRUE ~ 1
      )
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


