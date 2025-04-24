#' Análisis de Correspondencia Múltiple (ACM)
#'
#' Realiza un ACM utilizando `FactoMineR::MCA`, usando solo las filas
#' completas para las variables activas (factores no suplementarios).
#' Muestra opcionalmente gráficos mejorados con `factoextra`.
#'
#' @param datos Un data frame que contiene las variables.
#' @param ncp Número de dimensiones a conservar. Por defecto 5.
#' @param quali.sup Vector de índices o nombres de columnas categóricas suplementarias.
#' @param quanti.sup Vector de índices o nombres de columnas numéricas suplementarias.
#' @param mostrar_eigen Booleano. ¿Mostrar tabla de eigenvalues? Por defecto TRUE.
#' @param grafico_scree Booleano. ¿Mostrar scree plot? Por defecto TRUE.
#' @param grafico_var Booleano. ¿Mostrar gráfico de variables? Por defecto TRUE.
#' @param ... Argumentos adicionales pasados a `FactoMineR::MCA`.
#'
#' @return Un objeto de clase `MCA`. Devuelve `NULL` si no hay suficientes variables
#'   activas válidas. Muestra gráficos y tablas solicitados.
#'
#' @export
#' @importFrom FactoMineR MCA
#' @importFrom factoextra get_eigenvalue fviz_screeplot fviz_mca_var
#' @importFrom graphics plot
#' @importFrom ggplot2 ggplot ylab xlab ggtitle theme_minimal theme element_text
#' @importFrom stats complete.cases
#'
#' @examples
#' \dontrun{
#' # Cargar datos 'tea' y añadir algunos NAs
#' if (requireNamespace("FactoMineR", quietly = TRUE)) {
#'   data(tea, package = "FactoMineR")
#'   set.seed(123)
#'   tea$Tea[sample(1:nrow(tea), 20)] <- NA # Añadir NAs en variable activa
#'   tea$How[sample(1:nrow(tea), 15)] <- NA # Añadir NAs en variable activa
#'   tea$age[sample(1:nrow(tea), 10)] <- NA # Añadir NAs en variable quanti.sup
#'
#'   # Convertir a factores si es necesario (como antes)
#'   cols_to_factor <- names(tea)[!(names(tea) %in% c("age"))]
#'   for(col in cols_to_factor) {
#'      if(!is.factor(tea[[col]])) tea[[col]] <- factor(tea[[col]])
#'   }
#'
#'   # Ejecutar ACM (manejará NAs en activas; puede usar NAs en suplementarias)
#'   resultado_acm_na <- ACM(
#'     datos = tea,
#'     ncp = 5,
#'     quali.sup = c("where", "always"), # Nombres o índices
#'     quanti.sup = "age",
#'     grafico_var = FALSE # Desactivar para ejemplo rápido
#'   )
#'   if(!is.null(resultado_acm_na)) summary(resultado_acm_na)
#'
#' } else {
#'   print("Se necesita FactoMineR y factoextra.")
#' }
#' }
#'
#'@export
ACM <- function(datos, ncp = 5, quali.sup = NULL, quanti.sup = NULL,
                mostrar_eigen = TRUE, grafico_scree = TRUE, grafico_var = TRUE, ...) {

  # --- Validaciones iniciales ---
  if (!is.data.frame(datos)) stop("El argumento 'datos' debe ser un data frame.")
  if (!requireNamespace("FactoMineR", quietly = TRUE)) stop("Instala 'FactoMineR'.")
  if ((grafico_scree || grafico_var || mostrar_eigen) && !requireNamespace("factoextra", quietly = TRUE)) {
    warning("Instala 'factoextra' para visualizaciones y tabla de eigenvalues mejoradas.")
    mostrar_eigen <- grafico_scree <- grafico_var <- FALSE # Desactivar si falta factoextra
  }
  if ((grafico_scree || grafico_var) && !requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Instala 'ggplot2' para las visualizaciones.")
    grafico_scree <- grafico_var <- FALSE # Desactivar si falta ggplot2
  }

  # --- Identificar variables activas y suplementarias ---
  var_indices <- 1:ncol(datos)
  nombres_vars <- names(datos)

  # Convertir nombres a índices si es necesario y validar
  if (!is.null(quali.sup)) {
    if(is.character(quali.sup)) quali.sup <- match(quali.sup, nombres_vars)
    quali.sup <- quali.sup[!is.na(quali.sup) & quali.sup %in% var_indices]
  } else { quali.sup <- integer(0) }

  if (!is.null(quanti.sup)) {
    if(is.character(quanti.sup)) quanti.sup <- match(quanti.sup, nombres_vars)
    quanti.sup <- quanti.sup[!is.na(quanti.sup) & quanti.sup %in% var_indices]
  } else { quanti.sup <- integer(0) }

  indices_suplementarios <- c(quali.sup, quanti.sup)
  indices_activos_potenciales <- setdiff(var_indices, indices_suplementarios)

  # Verificar que las activas sean factores
  es_factor_activo <- sapply(datos[, indices_activos_potenciales, drop = FALSE], is.factor)
  indices_activos <- indices_activos_potenciales[es_factor_activo]
  nombres_activos <- nombres_vars[indices_activos]

  if (length(indices_activos) < 2) {
    warning("Se necesitan al menos dos variables activas (factores no suplementarios) para realizar ACM.")
    return(NULL)
  }

  # --- Filtrar datos por NA en variables ACTIVAS ---
  datos_filtrados <- datos[stats::complete.cases(datos[, nombres_activos]), , drop = FALSE]

  n_original <- nrow(datos)
  n_filtradas <- nrow(datos_filtrados)

  if (n_filtradas == 0) {
    warning("No quedan filas después de eliminar NAs en las variables activas. No se puede ejecutar ACM.")
    return(NULL)
  }
  if (n_filtradas < n_original) {
    message(paste("Se eliminaron", n_original - n_filtradas,
                  "filas con valores faltantes en las variables activas (",
                  paste(nombres_activos, collapse=", "), "). Se usarán ", n_filtradas, " filas.", sep=""))
  }

  # --- Ejecutar MCA en datos filtrados ---
  # Asegurarse de que los índices suplementarios siguen siendo válidos (aunque MCA los maneja)
  # Pasamos NULL si no hay suplementarias
  quali.sup.arg <- if(length(quali.sup) > 0) quali.sup else NULL
  quanti.sup.arg <- if(length(quanti.sup) > 0) quanti.sup else NULL

  resultado_acm <- tryCatch({
    FactoMineR::MCA(
      datos_filtrados, # Usar datos filtrados
      ncp = ncp,
      quali.sup = quali.sup.arg,
      quanti.sup = quanti.sup.arg,
      graph = FALSE, # Desactivamos gráficos base
      ...
    )
  }, error = function(e) {
    warning(paste("Error durante FactoMineR::MCA:", e$message))
    return(NULL)
  })

  if (is.null(resultado_acm)) return(NULL) # Salir si MCA falló

  # --- Mostrar resultados/gráficos de factoextra (si aplica) ---
  if (requireNamespace("factoextra", quietly = TRUE)) {
    if (mostrar_eigen) {
      cat("\n--- Eigenvalues / Varianza Explicada ---\n")
      print(factoextra::get_eigenvalue(resultado_acm))
      cat("\n")
    }
    if (grafico_scree && requireNamespace("ggplot2", quietly = TRUE)) {
      cat("\n--- Gráfico de Sedimentación (Scree Plot) ---\n")
      print(factoextra::fviz_screeplot(resultado_acm, addlabels = TRUE) +
              ggplot2::ylab("Porcentaje de varianza explicada") +
              ggplot2::xlab("Dimensiones") +
              ggplot2::ggtitle("Gráfico de Sedimentación ACM") +
              ggplot2::theme_minimal())
      cat("\n")
    }
    if (grafico_var && requireNamespace("ggplot2", quietly = TRUE)) {
      cat("\n--- Gráfico de Variables (Categorías) ACM ---\n")
      print(factoextra::fviz_mca_var(resultado_acm,
                                     col.var = "contrib",
                                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                     repel = TRUE,
                                     ggtheme = ggplot2::theme_minimal(),
                                     title = "Mapa de Variables ACM (contribución)")
      )
      cat("\n")
    }
  } # Fin chequeo factoextra

  return(resultado_acm)
}
