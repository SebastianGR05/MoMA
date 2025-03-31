#' Realizar Clustering K-Means sobre Coordenadas de ACM
#'
#' Aplica el algoritmo K-Means a las coordenadas de los individuos obtenidas
#' de un análisis de correspondencia múltiple (resultado de `realizar_acm` o
#' `FactoMineR::MCA`).
#'
#' @param resultado_acm El objeto devuelto por `realizar_acm` o `FactoMineR::MCA`.
#'   Debe contener las coordenadas de los individuos en `$ind$coord`.
#' @param n_clusters El número de clusters (k) a crear con K-Means.
#' @param ncp_kmeans El número de dimensiones (coordenadas) del ACM a utilizar
#'   para el clustering. Por defecto utiliza todas las calculadas en `resultado_acm`.
#'   Se recomienda usar un número reducido basado en el scree plot.
#' @param ... Argumentos adicionales pasados a `stats::kmeans` (ej. `nstart`, `iter.max`).
#'
#' @return Un objeto de clase `kmeans` que contiene los resultados del clustering.
#'   Devuelve `NULL` si hay errores (ej. `resultado_acm` inválido).
#'
#' @export
#' @importFrom stats kmeans
#'
#' @examples
#' \dontrun{
#' # Suponiendo que 'resultado_acm_mejorado' existe del ejemplo de realizar_acm
#' if (exists("resultado_acm_mejorado") && !is.null(resultado_acm_mejorado) &&
#'     requireNamespace("stats", quietly = TRUE)) {
#'
#'   # Determinar ncp a usar (ej. las primeras 5 dimensiones)
#'   ncp_usar <- 5
#'
#'   # Realizar clustering k-means con 3 clusters
#'   set.seed(123) # Para reproducibilidad del kmeans
#'   clusters_kmeans <- cluster_acm_kmeans(resultado_acm_mejorado,
#'                                         n_clusters = 3,
#'                                         ncp_kmeans = ncp_usar,
#'                                         nstart = 25) # Añadir nstart es buena práctica
#'
#'   # Ver resultados del clustering
#'   print(clusters_kmeans)
#'
#'   # Añadir el cluster al data frame original (si es útil)
#'   # data(tea, package="FactoMineR") # Recargar si es necesario
#'   # tea$cluster_kmeans <- factor(clusters_kmeans$cluster)
#'   # summary(tea$cluster_kmeans)
#'
#'   # Visualizar clusters sobre el mapa de individuos ACM (requiere factoextra)
#'   if (requireNamespace("factoextra", quietly = TRUE)) {
#'     print(factoextra::fviz_mca_ind(resultado_acm_mejorado,
#'                                    axes = c(1, 2), # Elegir dimensiones a graficar
#'                                    habillage = factor(clusters_kmeans$cluster), # Colorear por cluster
#'                                    addEllipses = TRUE, ellipse.type = "confidence",
#'                                    repel = TRUE,
#'                                    ggtheme = ggplot2::theme_minimal(),
#'                                    title = "Mapa de Individuos ACM con Clusters K-Means"))
#'   }
#'
#' } else {
#'   print("Se necesita un objeto resultado de MCA y el paquete stats.")
#'   print("Para el gráfico se necesita factoextra.")
#' }
#' }
#'
cluster_acm_kmeans <- function(resultado_acm, n_clusters, ncp_kmeans = NULL, ...) {

  # Validar entrada
  if (!inherits(resultado_acm, "MCA")) {
    stop("'resultado_acm' debe ser un objeto de clase 'MCA' (resultado de FactoMineR::MCA).")
  }
  if (!is.numeric(n_clusters) || length(n_clusters) != 1 || n_clusters < 1 || n_clusters != round(n_clusters)) {
    stop("'n_clusters' debe ser un número entero positivo.")
  }
  if (!"ind" %in% names(resultado_acm) || !"coord" %in% names(resultado_acm$ind)) {
    stop("El objeto 'resultado_acm' no contiene las coordenadas de los individuos ('$ind$coord').")
  }

  coords <- resultado_acm$ind$coord

  # Determinar cuántas dimensiones usar
  if (is.null(ncp_kmeans)) {
    ncp_usar <- ncol(coords) # Usar todas por defecto
  } else {
    if (!is.numeric(ncp_kmeans) || ncp_kmeans < 1 || ncp_kmeans > ncol(coords)) {
      warning("ncp_kmeans inválido. Se usarán todas las dimensiones disponibles.")
      ncp_usar <- ncol(coords)
    } else {
      ncp_usar <- as.integer(ncp_kmeans)
    }
  }

  if (nrow(coords) < n_clusters) {
    stop("El número de clusters solicitado es mayor que el número de individuos.")
  }

  # Seleccionar las coordenadas
  coords_seleccionadas <- coords[, 1:ncp_usar, drop = FALSE]

  # Comprobar si stats está disponible (debería estarlo siempre)
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("El paquete 'stats' (necesario para kmeans) no está disponible.")
  }

  # Realizar K-Means
  # Usar tryCatch por si kmeans falla por alguna razón
  resultado_kmeans <- tryCatch({
    stats::kmeans(coords_seleccionadas, centers = n_clusters, ...)
  }, error = function(e) {
    warning(paste("Error durante kmeans:", e$message))
    return(NULL)
  })

  return(resultado_kmeans)
}
