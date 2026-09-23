## 8 METODOS 

##  1. buscamos la media simple (recursiva) 

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

## 2. vamos a trabajar la funcion de la media movil de orden k

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

## 3. vamos a crear la función para el suavizamiento exponencial 

ajustar_ses <- function(y, alpha) {
  stopifnot(is.numeric(y), !any(is.na(y)), alpha > 0, alpha < 1, length(y) >= 2)
  n <- length(y)
  
  # Calculamos la forma de correccion de error
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

## 4.vamos a crear la función para la doble media movil

ajustar_dmm <- function(y, k) {
  stopifnot(is.numeric(y), !any(is.na(y)), k >= 2, 2 * k - 1 <= length(y))
  
  n <- length(y)
  MM <- rep(NA_real_, n)   # MM_t(k), definido para t >= k
  DMM <- rep(NA_real_, n)  # DMM_t(k), definido para t >= 2k-1
  E <- rep(NA_real_, n)
  beta1 <- rep(NA_real_, n)
  yhat <- rep(NA_real_, n)
  
  for (t in k:n) {
    MM[t] <- mean(y[(t - k + 1):t])
  }
  for (t in (2 * k - 1):n) {
    DMM[t] <- mean(MM[(t - k + 1):t])
    E[t] <- 2 * MM[t] - DMM[t]
    beta1[t] <- (2 / (k - 1)) * (MM[t] - DMM[t])
  }
  for (t in (2 * k - 1):(n - 1)) {
    yhat[t + 1] <- E[t] + beta1[t] * 1
  }
  
  E_final <- E[n]
  beta1_final <- beta1[n]
  
  pronosticar <- function(h) {
    stopifnot(h >= 1)
    E_final + beta1_final * (1:h)
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(k = k, E_final = E_final, beta1_final = beta1_final,
                      trayectoria_E = E, trayectoria_beta1 = beta1)
  )
}

## mejora  de la suma acumulasa que exigue el enunciado 

ajustar_dmm <- function(y, k) {
  stopifnot(is.numeric(y), !any(is.na(y)), k >= 2, 2 * k - 1 <= length(y))
  
  n <- length(y)
  MM <- rep(NA_real_, n)
  DMM <- rep(NA_real_, n)
  E <- rep(NA_real_, n)
  beta1 <- rep(NA_real_, n)
  yhat <- rep(NA_real_, n)
  
  # Primera media movil, con suma acumulada (un solo recorrido) ---
  suma_mm <- sum(y[1:k])
  MM[k] <- suma_mm / k
  if (n > k) {
    for (t in (k + 1):n) {
      suma_mm <- suma_mm - y[t - k] + y[t]
      MM[t] <- suma_mm / k
    }
  }
  
  # Segunda media movil (de MM), con suma acumulada 
  suma_dmm <- sum(MM[k:(2 * k - 1)])
  DMM[2 * k - 1] <- suma_dmm / k
  E[2 * k - 1] <- 2 * MM[2 * k - 1] - DMM[2 * k - 1]
  beta1[2 * k - 1] <- (2 / (k - 1)) * (MM[2 * k - 1] - DMM[2 * k - 1])
  
  if (n > 2 * k - 1) {
    for (t in (2 * k):n) {
      suma_dmm <- suma_dmm - MM[t - k] + MM[t]
      DMM[t] <- suma_dmm / k
      E[t] <- 2 * MM[t] - DMM[t]
      beta1[t] <- (2 / (k - 1)) * (MM[t] - DMM[t])
    }
  }
  
  for (t in (2 * k - 1):(n - 1)) {
    yhat[t + 1] <- E[t] + beta1[t] * 1
  }
  
  E_final <- E[n]
  beta1_final <- beta1[n]
  
  pronosticar <- function(h) {
    stopifnot(h >= 1)
    E_final + beta1_final * (1:h)
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(k = k, E_final = E_final, beta1_final = beta1_final,
                      trayectoria_E = E, trayectoria_beta1 = beta1)
  )
}

## 5. vamos a crear la función para la tendencia con base a al fromula requerida

# Paso 1: hallamos el errores estandar robustos, y nos apoyamos en  las puebas de Newey-West y  el nucleo de Bartlett
.se_robustos_nw <- function(X, residuos) {
  T_obs <- nrow(X)
  k <- ncol(X)
  L <- floor(4 * (T_obs / 100)^(2 / 9))  # numero de rezagos, formula del enunciado
  
  XtX_inv <- solve(crossprod(X))
  
  # parte 1: heterocedasticidad (cada residuo por separado)
  S <- matrix(0, k, k)
  for (t in 1:T_obs) {
    xt <- X[t, ]
    S <- S + residuos[t]^2 * (xt %*% t(xt))
  }
  
  # parte 2: autocorrelacion, con pesos de Bartlett, para cada rezago j = 1...L
  if (L >= 1) {
    for (j in 1:L) {
      w_j <- 1 - j / (L + 1)
      Gamma_j <- matrix(0, k, k)
      for (t in (j + 1):T_obs) {
        xt  <- X[t, ]
        xtj <- X[t - j, ]
        Gamma_j <- Gamma_j + residuos[t] * residuos[t - j] * (xt %*% t(xtj) + xtj %*% t(xt))
      }
      S <- S + w_j * Gamma_j
    }
  }
  
  vcov_robusto <- XtX_inv %*% S %*% XtX_inv
  se <- sqrt(diag(vcov_robusto))
  return(se)
}

# funcion de tendencias 
ajustar_tendencia <- function(y, tipo = c("lineal", "cuadratica", "exponencial"), corregir_sesgo = FALSE) {
  tipo <- match.arg(tipo)
  stopifnot(is.numeric(y), !any(is.na(y)), length(y) >= 4)
  T <- length(y)
  t_idx <- 1:T
  
  if (tipo == "exponencial" && any(y <= 0)) {
    stop("La tendencia exponencial requiere valores estrictamente positivos en y")
  }
  
  #  hallamos las variable respuesta y matriz de diseno segun el tipo
  if (tipo == "lineal") {
    y_ajustar <- y
    X <- cbind(1, t_idx)
    colnames(X) <- c("beta0", "beta1")
  } else if (tipo == "cuadratica") {
    y_ajustar <- y
    X <- cbind(1, t_idx, t_idx^2)
    colnames(X) <- c("beta0", "beta1", "beta2")
  } else {
    y_ajustar <- log(y)
    X <- cbind(1, t_idx)
    colnames(X) <- c("a", "theta")
  }
  
  # usamos las ecuaciones normales
  coef <- solve(crossprod(X), crossprod(X, y_ajustar))
  coef <- as.numeric(coef)
  names(coef) <- colnames(X)
  
  valores_ajustados <- as.numeric(X %*% coef)
  residuos <- y_ajustar - valores_ajustados
  k <- ncol(X)  # numero de regresores (incluye intercepto)
  sigma2 <- sum(residuos^2) / (T - k)
  
  vcov_ols <- sigma2 * solve(crossprod(X))
  se_ols <- sqrt(diag(vcov_ols))
  se_robusto <- .se_robustos_nw(X, residuos)
  
  t_stat <- coef / se_ols
  valor_p <- 2 * (1 - pt(abs(t_stat), df = T - k))
  
  SCT <- sum((y_ajustar - mean(y_ajustar))^2)
  SCE <- sum(residuos^2)
  R2 <- 1 - SCE / SCT
  
  dw <- durbin_watson(residuos)$estadistico  # de 03-evaluacion.R
  
  tabla_coef <- data.frame(
    coeficiente = names(coef),
    estimacion = coef,
    se_ols = se_ols,
    se_robusto = se_robusto,
    t = t_stat,
    valor_p = valor_p
  )
  
  # yhat de un paso: aqui la tendencia se ajusta sobre TODA la muestra
  # (no hay recursividad), asi que yhat[t] = valor ajustado en t
  yhat <- rep(NA_real_, T)
  if (tipo == "exponencial") {
    yhat <- exp(valores_ajustados)
  } else {
    yhat <- valores_ajustados
  }
  
  sigma2_ln <- if (tipo == "exponencial") sigma2 else NA
  
  pronosticar <- function(h) {
    stopifnot(h >= 1)
    t_fut <- T + (1:h)
    if (tipo == "lineal") {
      coef[1] + coef[2] * t_fut
    } else if (tipo == "cuadratica") {
      coef[1] + coef[2] * t_fut + coef[3] * t_fut^2
    } else {
      pron <- exp(coef[1] + coef[2] * t_fut)
      if (corregir_sesgo) pron <- pron * exp(sigma2_ln / 2)
      pron
    }
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(
      tipo = tipo,
      coeficientes = coef,
      tabla = tabla_coef,
      R2 = R2,
      sigma2 = sigma2,
      durbin_watson = dw,
      corregir_sesgo = corregir_sesgo
    )
  )
}

## 6. hallamos la funcion de holt lineal 

ajustar_holt <- function(y, alpha, beta) {
  stopifnot(is.numeric(y), !any(is.na(y)),
            alpha > 0, alpha < 1, beta > 0, beta < 1,
            length(y) >= 3)
  
  n <- length(y)
  L <- rep(NA_real_, n)
  Tt <- rep(NA_real_, n)
  yhat <- rep(NA_real_, n)
  
  L[1] <- y[1]
  Tt[1] <- 0
  
  for (t in 2:n) {
    L[t] <- alpha * y[t] + (1 - alpha) * (L[t - 1] + Tt[t - 1])
    Tt[t] <- beta * (L[t] - L[t - 1]) + (1 - beta) * Tt[t - 1]
    if (t < n) yhat[t + 1] <- L[t] + Tt[t]
  }
  
  # verificamos la forma de correccion de error
  L_corr <- rep(NA_real_, n)
  Tt_corr <- rep(NA_real_, n)
  L_corr[1] <- y[1]
  Tt_corr[1] <- 0
  for (t in 2:n) {
    yhat_t <- L_corr[t - 1] + Tt_corr[t - 1]
    e_t <- y[t] - yhat_t
    L_corr[t] <- L_corr[t - 1] + Tt_corr[t - 1] + alpha * e_t
    Tt_corr[t] <- Tt_corr[t - 1] + alpha * beta * e_t
  }
  dif_L <- max(abs(L - L_corr))
  dif_T <- max(abs(Tt - Tt_corr))
  stopifnot(dif_L < 1e-8, dif_T < 1e-8)
  
  L_final <- L[n]
  T_final <- Tt[n]
  
  pronosticar <- function(h) {
    stopifnot(h >= 1)
    L_final + T_final * (1:h)
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(alpha = alpha, beta = beta,
                      L_final = L_final, T_final = T_final,
                      trayectoria_L = L, trayectoria_T = Tt)
  )
}