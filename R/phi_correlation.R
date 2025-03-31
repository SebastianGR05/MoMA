#' Calcular el Coeficiente Phi para dos variables dicotómicas
#'
#' Calcula el coeficiente de asociación Phi (φ) para dos variables categóricas
#' dicotómicas (con exactamente dos niveles cada una). Phi es una medida
#' de asociación basada en la prueba Chi-cuadrado para tablas de contingencia 2x2.
#'
#' @param datos Un data frame que contiene las variables.
#' @param var1 El nombre (como cadena de texto) o índice de la primera variable dicotómica.
#' @param var2 El nombre (como cadena de texto) o índice de la segunda variable dicotómica.
#' @param ... Argumentos adicionales pasados a `stats::chisq.test` (por ejemplo,
#'   `simulate.p.value = TRUE` si hay frecuencias esperadas bajas, aunque para
#'    Phi solo usamos el estadístico chi-cuadrado).
#'
#' @return Un valor numérico que representa el coeficiente Phi. El valor varía
#'   entre -1 y +1 (aunque en R `chisq.test` no da signo, Phi suele interpretarse
#'   en magnitud, similar a la correlación, pero su rango máximo depende de las
#'   marginales y solo alcanza +/- 1 en casos perfectos). Devuelve `NA` si
#'   no se puede calcular (ej. variables no dicotómicas, error en chi-cuadrado).
#'   Imprime advertencias si las variables no son factores o no son dicotómicas.
#'
#' @export
#' @importFrom stats chisq.test
#'
#' @examples
#' # Crear datos de ejemplo dicotómicos
#' set.seed(123) # para reproducibilidad
#' df_dicotomico <- data.frame(
#'   Pregunta1 = factor(sample(c("Sí", "No"), 100, replace = TRUE)),
#'   Pregunta2 = factor(sample(c("Sí", "No"), 100, replace = TRUE)),
#'   OtraVar = rnorm(100)
#' )
#'
#' # Calcular el coeficiente Phi entre Pregunta1 y Pregunta2
#' phi_val <- coeficiente_phi(df_dicotomico, "Pregunta1", "Pregunta2")
#' print(phi_val)
#'
#' # Ejemplo con índices
#' phi_val_idx <- coeficiente_phi(df_dicotomico, 1, 2)
#' print(phi_val_idx)
#'
#' # Ejemplo con una variable no dicotómica (dará advertencia y NA)
#' df_dicotomico$Pregunta3 <- factor(sample(c("A", "B", "C"), 100, replace = TRUE))
#' phi_error <- coeficiente_phi(df_dicotomico, "Pregunta1", "Pregunta3")
#'
coeficiente_phi <- function(datos, var1, var2, ...) {

  if (!is.data.frame(datos)) {
    stop("El argumento 'datos' debe ser un data frame.")
  }

  # Validar que las variables existan
  if (!(var1 %in% names(datos) || (is.numeric(var1) && var1 <= ncol(datos)))) {
    stop("La variable especificada en 'var1' no se encuentra en el data frame.")
  }
  if (!(var2 %in% names(datos) || (is.numeric(var2) && var2 <= ncol(datos)))) {
    stop("La variable especificada en 'var2' no se encuentra en el data frame.")
  }

  # Extraer las columnas (maneja nombres o índices)
  col1 <- datos[[var1]]
  col2 <- datos[[var2]]
  nombre_var1 <- ifelse(is.character(var1), var1, names(datos)[var1])
  nombre_var2 <- ifelse(is.character(var2), var2, names(datos)[var2])

  # Intentar convertir a factor si no lo son
  if (!is.factor(col1)) {
    warning(paste("La variable '", nombre_var1, "' no es un factor. Intentando convertirla.", sep=""))
    col1 <- factor(col1)
  }
  if (!is.factor(col2)) {
    warning(paste("La variable '", nombre_var2, "' no es un factor. Intentando convertirla.", sep=""))
    col2 <- factor(col2)
  }

  # Verificar que ambas variables sean dicotómicas (exactamente 2 niveles)
  if (length(levels(col1)) != 2) {
    warning(paste("La variable '", nombre_var1, "' no es dicotómica (tiene ", length(levels(col1)), " niveles). No se puede calcular Phi.", sep=""))
    return(NA)
  }
  if (length(levels(col2)) != 2) {
    warning(paste("La variable '", nombre_var2, "' no es dicotómica (tiene ", length(levels(col2)), " niveles). No se puede calcular Phi.", sep=""))
    return(NA)
  }

  # Crear la tabla de contingencia 2x2, eliminando NAs por parejas
  # Usamos useNA = "no" para que table() no cuente los NAs como nivel
  # El manejo de NAs se hace implícitamente al crear la tabla sobre los vectores
  # filtrados por complete.cases
  completos <- complete.cases(col1, col2)
  if(sum(completos) == 0) {
    warning("No hay observaciones completas para las variables seleccionadas.")
    return(NA)
  }
  tabla_contingencia <- table(col1[completos], col2[completos], useNA = "no")

  # Verificar que la tabla sea 2x2 (puede no serlo si una categoría no tiene observaciones)
  if (!all(dim(tabla_contingencia) == c(2, 2))) {
    warning("La tabla de contingencia no es 2x2 después de eliminar NAs o por falta de observaciones en alguna categoría. No se puede calcular Phi.")
    return(NA)
  }


  # Calcular Chi-cuadrado
  # Usamos suppressWarnings para evitar mensajes sobre aproximación Chi2 si hay frec. bajas
  # El cálculo de Phi sigue siendo válido.
  resultado_chi2 <- suppressWarnings(stats::chisq.test(tabla_contingencia, correct = FALSE, ...)) # correct=FALSE es común para Phi

  # Extraer estadístico Chi-cuadrado y tamaño total
  chi2_stat <- resultado_chi2$statistic
  n_total <- sum(tabla_contingencia)

  if (n_total == 0) {
    warning("La tabla de contingencia está vacía.")
    return(NA)
  }

  # Calcular Coeficiente Phi
  phi <- sqrt(chi2_stat / n_total)

  # Devolver el valor de Phi (siempre positivo como sale de esta fórmula)
  # Nota: El signo de Phi a veces se deriva de la tabla, pero aquí devolvemos la magnitud.
  return(as.numeric(phi)) # Asegurar que devuelve un número simple
}

