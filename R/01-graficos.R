#instalamos los paquete necesarios para la creación de las graficas 
library(ggplot2)
library(tibble)
library(dplyr)

graficar_serie <- function(datos, titulo) {
  # funcion para guardar todo lo que vamos a  hacer 
  unidad <- attr(datos, "unidad")
  fuente <- attr(datos, "fuente")
  n <- nrow(datos)
  #usamos la funcion ggplot  para la creacion de el grafico
  grafico <- ggplot2::ggplot(datos, aes(x = fecha, y = y)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = titulo,
      x = "Fecha",
      y = unidad,
      caption = paste("Fuente:", fuente, ". n =", n, "observaciones")
    ) +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "2 years")
  
  return(grafico)
}