#' Realizar Análisis de Correspondencia Múltiple (ACM) con FactoMineR y Factoextra
#'
#' Esta función realiza un ACM sobre las variables categóricas (factores)
#' de un conjunto de datos utilizando `FactoMineR::MCA` y opcionalmente genera
#' visualizaciones mejoradas con `factoextra`.
#' Permite especificar variables categóricas y cuantitativas suplementarias.
#'
#' @param datos Un data frame que contiene las variables. Las variables a incluir
#'   en el análisis activo deben ser de tipo factor.
#' @param ncp Número de dimensiones a conservar en los resultados. Por defecto 5.
#' @param quali.sup Un vector de índices o nombres de columnas categóricas
#'   (factores) que se usarán como variables suplementarias. Por defecto NULL.
#' @param quanti.sup Un vector de índices o nombres de columnas numéricas
#'   que se usarán como variables suplementarias. Por defecto NULL.
#' @param mostrar_eigen Booleano. ¿Mostrar la tabla de eigenvalues (varianza explicada)
#'   usando `factoextra::get_eigenvalue`? Por defecto TRUE.
#' @param grafico_scree Booleano. ¿Mostrar el gráfico de sedimentación (scree plot)
#'   usando `factoextra::fviz_screeplot`? Por defecto TRUE.
#' @param grafico_var Booleano. ¿Mostrar el gráfico de variables (categorías)
#'   usando `factoextra::fviz_mca_var` coloreado por contribución? Por defecto TRUE.
#' @param ... Argumentos adicionales pasados a `FactoMineR::MCA`.
#'
#' @return Un objeto de clase `MCA` que contiene todos los resultados del análisis,
#'   tal como lo devuelve `FactoMineR::MCA`. Devuelve `NULL` e imprime una advertencia
#'   si no hay suficientes variables activas (factores no suplementarios).
#'   Además, muestra los gráficos y tablas solicitados en los argumentos.
#'
#' @export
#' @importFrom FactoMineR MCA
#' @importFrom factoextra get_eigenvalue fviz_screeplot fviz_mca_var
#' @importFrom graphics plot
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' # Cargar datos 'tea' de FactoMineR
#' if (requireNamespace("FactoMineR", quietly = TRUE)) {
#'   data(tea, package = "FactoMineR")
#'
#'   # Ejecutar ACM especificando algunas variables suplementarias
#'   # (ej. Price category como cualitativa sup, age como cuantitativa sup)
#'   # Asegurarse de que las variables activas sean factores
#'   # Convertimos columnas necesarias a factor (si no lo son ya)
#'   cols_to_factor <- names(tea)[c(1:18, 20:36)] # Nombres de columnas categóricas y quali.sup
#'   for(col in cols_to_factor) {
#'      if(!is.factor(tea[[col]])) tea[[col]] <- factor(tea[[col]])
#'   }
#'
#'   resultado_acm_mejorado <- realizar_acm(
#'     datos = tea,
#'     ncp = 5,
#'     quali.sup = 20:36, # Índices de cualitativas suplementarias
#'     quanti.sup = 19,   # Índice de cuantitativa suplementaria (Age)
#'     grafico_scree = TRUE,
#'     grafico_var = TRUE
#'   )
#'
#'   # Acceder a los resultados
#'   summary(resultado_acm_mejorado)
#'
#' } else {
#'   print("Los paquetes FactoMineR y factoextra son necesarios para este ejemplo.")
#' }
#' }
#'
realizar_acm <- function(datos, ncp = 5, quali.sup = NULL, quanti.sup = NULL,
                         mostrar_eigen = TRUE, grafico_scree = TRUE, grafico_var = TRUE, ...) {

  if (!is.data.frame(datos)) {
    stop("El argumento 'datos' debe ser un data frame.")
  }

  # Comprobar si los paquetes necesarios están instalados
  if (!requireNamespace("FactoMineR", quietly = TRUE)) {
    stop("El paquete 'FactoMineR' es necesario. Instálalo con install.packages('FactoMineR').")
  }
  if (!requireNamespace("factoextra", quietly = TRUE)) {
    stop("El paquete 'factoextra' es necesario para las visualizaciones mejoradas. Instálalo con install.packages('factoextra').")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete 'ggplot2' es necesario para las visualizaciones de factoextra. Instálalo con install.packages('ggplot2').")
  }


  # Identificar variables activas (factores que no son quali.sup)
  var_indices <- 1:ncol(datos)
  if (!is.null(quali.sup)) {
    # Convertir nombres a índices si es necesario
    if(is.character(quali.sup)) quali.sup <- match(quali.sup, names(datos))
    quali.sup <- quali.sup[!is.na(quali.sup)] # Ignorar nombres no encontrados
  } else {
    quali.sup <- integer(0) # Crear vector vacío si es NULL
  }

  if (!is.null(quanti.sup)) {
    if(is.character(quanti.sup)) quanti.sup <- match(quanti.sup, names(datos))
    quanti.sup <- quanti.sup[!is.na(quanti.sup)]
  } else {
    quanti.sup <- integer(0)
  }

  indices_activos <- setdiff(var_indices, c(quali.sup, quanti.sup))
  es_factor_activo <- sapply(datos[, indices_activos, drop = FALSE], is.factor)
  columnas_activas_factor <- indices_activos[es_factor_activo]

  if (length(columnas_activas_factor) < 2) {
    warning("Se necesitan al menos dos variables activas (factores no suplementarios) para realizar ACM.")
    return(NULL)
  }

  # Asegurarse de que quali.sup y quanti.sup se pasan correctamente a MCA
  # MCA espera NULL si no hay variables suplementarias
  quali.sup.arg <- if(length(quali.sup) > 0) quali.sup else NULL
  quanti.sup.arg <- if(length(quanti.sup) > 0) quanti.sup else NULL

  # Realizar el ACM (sin los gráficos por defecto de FactoMineR)
  resultado_acm <- FactoMineR::MCA(
    datos,
    ncp = ncp,
    quali.sup = quali.sup.arg,
    quanti.sup = quanti.sup.arg,
    graph = FALSE, # Desactivamos gráficos base
    ...
  )

  # Mostrar resultados de factoextra si se solicitó
  if (mostrar_eigen) {
    cat("\n--- Eigenvalues / Varianza Explicada ---\n")
    # Usamos print explícitamente por si se ejecuta dentro de otra función
    print(factoextra::get_eigenvalue(resultado_acm))
    cat("\n")
  }

  if (grafico_scree) {
    cat("\n--- Gráfico de Sedimentación (Scree Plot) ---\n")
    print(factoextra::fviz_screeplot(resultado_acm, addlabels = TRUE) +
            ggplot2::ylab("Porcentaje de varianza explicada") +
            ggplot2::xlab("Dimensiones") +
            ggplot2::ggtitle("Gráfico de Sedimentación ACM"))
    cat("\n")
  }

  if (grafico_var) {
    cat("\n--- Gráfico de Variables (Categorías) ACM ---\n")
    print(factoextra::fviz_mca_var(resultado_acm,
                                   col.var = "contrib", # Colorear por contribución
                                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                   repel = TRUE, # Evitar solapamiento de texto
                                   ggtheme = ggplot2::theme_minimal(),
                                   title = "Mapa de Variables ACM (contribución)")
    )
    cat("\n")
  }

  return(resultado_acm)
}
