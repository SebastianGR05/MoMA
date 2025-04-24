#' Clustering K-Means sobre Coordenadas de ACM
#'
#' Aplica K-Means a las coordenadas de individuos de un resultado de ACM.
#'
#' @param resultado_acm Objeto devuelto por `ACM` (o `FactoMineR::MCA`).
#' @param n_clusters Número de clusters (k).
#' @param ncp_kmeans Número de dimensiones ACM a usar para clustering. Por defecto todas.
#' @param ... Argumentos adicionales pasados a `stats::kmeans`.
#'
#' @return Un objeto de clase `kmeans`. Devuelve `NULL` si hay errores.
#'
#' @export
#' @importFrom stats kmeans
#' @importFrom factoextra fviz_mca_ind
#' @importFrom ggplot2 theme_minimal ggtitle
#'
#' @examples
#' \dontrun{
#' # Suponiendo que 'resultado_acm_na' existe del ejemplo de ACM
#' if (exists("resultado_acm_na") && !is.null(resultado_acm_na) &&
#'     requireNamespace("stats", quietly = TRUE)) {
#'
#'   # Realizar clustering k-means con 3 clusters usando 5 dimensiones
#'   set.seed(123)
#'   clusters_km <- kmeans_acm(resultado_acm_na, n_clusters = 3, ncp_kmeans = 5, nstart = 25)
#'
#'   # Ver resultados
#'   print(clusters_km)
#'
#'   # Visualizar (si factoextra está instalado)
#'   if (!is.null(clusters_km) && requireNamespace("factoextra", quietly = TRUE)) {
#'     print(factoextra::fviz_mca_ind(resultado_acm_na, axes = c(1, 2),
#'                                    habillage = factor(clusters_km$cluster),
#'                                    addEllipses = TRUE, ellipse.type = "confidence",
#'                                    repel = TRUE, ggtheme = ggplot2::theme_minimal(),
#'                                    title = "Individuos ACM con Clusters K-Means"))
#'   }
#' }
#' }
#'
#'@export
kmeans_acm <- function(resultado_acm, n_clusters, ncp_kmeans = NULL, ...) {

  # --- Validaciones ---
  if (!inherits(resultado_acm, "MCA")) stop("'resultado_acm' debe ser un objeto MCA.")
  if (!is.numeric(n_clusters) || n_clusters < 1) stop("'n_clusters' debe ser entero positivo.")
  if (!"ind" %in% names(resultado_acm) || !"coord" %in% names(resultado_acm$ind)) stop("Objeto MCA no contiene '$ind$coord'.")

  coords <- resultado_acm$ind$coord

  # Determinar dimensiones a usar
  if (is.null(ncp_kmeans)) { ncp_usar <- ncol(coords)
  } else {
    if (!is.numeric(ncp_kmeans) || ncp_kmeans < 1 || ncp_kmeans > ncol(coords)) {
      warning("ncp_kmeans inválido. Usando todas las dimensiones.")
      ncp_usar <- ncol(coords)
    } else { ncp_usar <- as.integer(ncp_kmeans) }
  }

  if (nrow(coords) < n_clusters) stop("Más clusters que individuos.")

  coords_seleccionadas <- coords[, 1:ncp_usar, drop = FALSE]

  # --- K-Means ---
  if (!requireNamespace("stats", quietly = TRUE)) stop("El paquete 'stats' es necesario.")
  resultado_kmeans <- tryCatch({
    stats::kmeans(coords_seleccionadas, centers = n_clusters, ...)
  }, error = function(e) {
    warning(paste("Error durante kmeans:", e$message)); return(NULL)
  })

  return(resultado_kmeans)
}
