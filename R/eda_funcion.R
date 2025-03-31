#' Análisis Exploratorio Básico de Datos
#'
#' Esta función proporciona un resumen estadístico básico y una visión general
#' de la estructura y los valores faltantes de un conjunto de datos.
#'
#' @param datos Un data frame o tibble que se desea explorar.
#' @param mostrar_primeras_filas Número de filas iniciales a mostrar con `head()`. Por defecto 5.
#'
#' @return Imprime en la consola un resumen de la estructura (`str`),
#'   las primeras filas (`head`), un resumen estadístico (`summary`) y
#'   la cantidad de valores faltantes por columna. No devuelve un objeto R.
#'
#' @export
#'
#' @examples
#' # Crear un data frame de ejemplo
#' df_ejemplo <- data.frame(
#'   Numerica = rnorm(50),
#'   Categorica = factor(sample(c("A", "B", "C"), 50, replace = TRUE)),
#'   Logica = sample(c(TRUE, FALSE, NA), 50, replace = TRUE)
#' )
#'
#' # Ejecutar el análisis exploratorio
#' # analisis_exploratorio(df_ejemplo)
#'
analisis_exploratorio <- function(datos, mostrar_primeras_filas = 5) {

  if (!is.data.frame(datos)) {
    stop("El argumento 'datos' debe ser un data frame.")
  }

  cat("--- Estructura del Data Frame ---\n")
  utils::str(datos)
  cat("\n") # Salto de línea

  cat("--- Primeras", mostrar_primeras_filas, "Filas ---\n")
  print(utils::head(datos, n = mostrar_primeras_filas))
  cat("\n")

  cat("--- Resumen Estadístico ---\n")
  print(summary(datos))
  cat("\n")

  cat("--- Valores Faltantes por Columna ---\n")
  faltantes <- colSums(is.na(datos))
  if (sum(faltantes) == 0) {
    cat("No hay valores faltantes en el data frame.\n")
  } else {
    print(faltantes[faltantes > 0])
  }
  cat("\n")

  cat("--- Fin del Análisis Exploratorio Básico ---\n")

  # La función no devuelve nada explícitamente (imprime en consola)
  invisible(NULL)
}
