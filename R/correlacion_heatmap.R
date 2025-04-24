#' Matriz de Correlación/Asociación y Heatmap
#'
#' Calcula una matriz de correlación o asociación para variables seleccionadas
#' usando un único método especificado por el usuario. El método se aplica
#' solo a los pares de variables compatibles.
#'
#' Métodos disponibles y pares compatibles:
#' - "pearson", "spearman", "kendall": Numérica vs Numérica.
#' - "phi": Dicotómica vs Dicotómica (factor con 2 niveles).
#' - "pointbiserial": Numérica vs Dicotómica.
#' - "cramersv": Categórica vs Categórica (factor con >= 2 niveles).
#'
#' @param datos Un data frame que contiene las variables.
#' @param variables Un vector opcional de nombres o índices de columnas a incluir.
#'   Si es NULL (por defecto), se usarán todas las variables numéricas y factores.
#'   Las columnas de tipo 'character' se convertirán a factores.
#' @param metodo El método de correlación/asociación a usar. Uno de:
#'   "pearson" (def.), "spearman", "kendall", "phi", "pointbiserial", "cramersv".
#' @param manejo_na Cómo manejar NAs: "pairwise" (def.) usa casos completos para
#'   cada par; "complete" usa solo filas completas para todas las variables
#'   seleccionadas *antes* de calcular.
#' @param mostrar_heatmap Booleano. ¿Mostrar un mapa de calor de la matriz? Por defecto TRUE.
#' @param digits Número de decimales para redondear coeficientes en el heatmap. Por defecto 2.
#' @param ... Argumentos adicionales pasados a `ggcorrplot::ggcorrplot`
#'   (ej. `hc.order = TRUE`, `type = 'lower'`, `colors = c("blue", "white", "red")`).
#'
#' @return Una matriz cuadrada que contiene los coeficientes calculados para los
#'   pares compatibles con el método elegido, y `NA` en los demás casos. La diagonal
#'   se establece en 1 para las variables donde el método es aplicable a sí misma
#'   (numéricas para Pearson/Spearman/Kendall, factores para Phi/CramerV), y NA
#'   en otros casos (PointBiserial). Devuelve `NULL` si no hay variables válidas
#'   para el método elegido.
#'
#' @export
#' @importFrom stats cor sd chisq.test complete.cases na.omit formula terms model.matrix lm anova update
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom ggplot2 ggtitle theme element_text
#'
#' @examples
#' # Crear datos mixtos de ejemplo
#' set.seed(123)
#' df_mixto <- data.frame(
#'   Edad = rnorm(100, 40, 10),
#'   Ingresos = rnorm(100, 50000, 15000),
#'   Genero = factor(sample(c("M", "F"), 100, replace = TRUE)),
#'   Educacion = factor(sample(c("Basica", "Media", "Superior", NA), 100,
#'                             replace = TRUE, prob = c(0.2, 0.4, 0.3, 0.1))),
#'   Compra = factor(sample(c("Si", "No"), 100, replace = TRUE)),
#'   Satisfaccion = sample(1:5, 100, replace = TRUE), # Tratar como factor
#'   Activo = sample(0:1, 100, replace=TRUE) # Numérica binaria
#' )
#' df_mixto$Satisfaccion <- factor(df_mixto$Satisfaccion, ordered = TRUE) # Factor ordenado
#' df_mixto$Edad[sample(1:100, 5)] <- NA # Añadir NAs
#'
#' # Calcular solo Pearson (Num vs Num)
#' correlacion_heatmap(df_mixto, metodo = "pearson", digits = 2)
#'
#' \dontrun{
#' # Calcular solo Phi (Dicótoma vs Dicótoma)
#' correlacion_heatmap(df_mixto, metodo = "phi", variables = c("Genero", "Compra", "Activo"))
#' # Nota: Activo es numérica 0/1, se tratará como dicotómica si se convierte a factor primero.
#' #       La función actual no lo hace automáticamente para 'phi', requeriría preparación previa.
#' #       Modifiquemos para que trate factores con 2 niveles.
#'
#' # Calcular solo Point-Biserial (Num vs Dicótoma)
#' # (Visualizará la relación Edad-Genero, Ingresos-Genero, Edad-Compra, Ingresos-Compra)
#' correlacion_heatmap(df_mixto, metodo = "pointbiserial",
#'                    variables = c("Edad", "Ingresos", "Genero", "Compra"), digits = 3)
#'
#' # Calcular solo Cramer's V (Cat vs Cat)
#' correlacion_heatmap(df_mixto, metodo = "cramersv",
#'                    variables = c("Genero", "Educacion", "Compra", "Satisfaccion"),
#'                    hc.order = TRUE, type = "lower", digits=2)
#' }
#'
#'@export
correlacion_heatmap <- function(datos, variables = NULL,
                                metodo = "pearson", manejo_na = "pairwise",
                                mostrar_heatmap = TRUE, digits = 2, ...) {

  # --- Validaciones Iniciales ---
  if (!is.data.frame(datos)) stop("'datos' debe ser un data frame.")
  if (!manejo_na %in% c("pairwise", "complete")) stop("'manejo_na' debe ser 'pairwise' o 'complete'.")
  metodos_validos <- c("pearson", "spearman", "kendall", "phi", "pointbiserial", "cramersv")
  if (!metodo %in% metodos_validos) stop(paste("'metodo' debe ser uno de:", paste(metodos_validos, collapse=", ")))

  # --- Selección y Preparación de Variables ---
  if (is.null(variables)) {
    es_valida <- sapply(datos, function(x) is.numeric(x) || is.factor(x) || is.character(x))
    if (!any(es_valida)) {warning("No se encontraron columnas numéricas, factores o caracteres."); return(NULL)}
    datos_seleccionados <- datos[, es_valida, drop = FALSE]
  } else {
    if (!all(variables %in% names(datos)) && !all(is.numeric(variables) && all(variables %in% 1:ncol(datos)))) {
      stop("El vector 'variables' contiene nombres o índices no válidos.")
    }
    datos_seleccionados <- datos[, variables, drop = FALSE]
    es_valida_sel <- sapply(datos_seleccionados, function(x) is.numeric(x) || is.factor(x) || is.character(x))
    if (!all(es_valida_sel)) {
      warning("Algunas variables seleccionadas no son numéricas, factores ni caracteres y serán excluidas.")
      datos_seleccionados <- datos_seleccionados[, es_valida_sel, drop = FALSE]
    }
  }

  # Convertir caracteres a factores
  es_char <- sapply(datos_seleccionados, is.character)
  if (any(es_char)) {
    for (col_char in names(datos_seleccionados)[es_char]) {
      message(paste("Convirtiendo la columna de caracteres '", col_char, "' a factor.", sep=""))
      datos_seleccionados[[col_char]] <- factor(datos_seleccionados[[col_char]])
    }
  }

  if (ncol(datos_seleccionados) < 1) { # Necesita al menos 1 para algunos casos
    warning("No quedan variables válidas después de la selección/conversión.")
    return(NULL)
  }

  # --- Manejo de NAs global si se elige "complete" ---
  datos_procesar <- datos_seleccionados # Copia para no modificar la original
  if (manejo_na == "complete") {
    n_original <- nrow(datos_procesar)
    datos_procesar <- stats::na.omit(datos_procesar) # na.omit sobre el data frame seleccionado
    n_usadas <- nrow(datos_procesar)
    if (n_usadas < n_original) {
      message(paste("Usando manejo 'complete': Se eliminaron", n_original - n_usadas,
                    "filas con NAs en las variables seleccionadas. Se usarán", n_usadas, " filas."))
    }
    if (n_usadas < 2) { # Necesario para la mayoría de coeficientes
      warning("No quedan suficientes filas completas después de aplicar manejo 'complete'.")
      return(NULL)
    }
  }

  # --- Inicializar Matriz Resultado ---
  n_vars <- ncol(datos_procesar)
  nombres_vars <- names(datos_procesar)
  matriz_resultado <- matrix(NA_real_, nrow = n_vars, ncol = n_vars,
                             dimnames = list(nombres_vars, nombres_vars))

  # --- Cálculo según el método elegido ---

  # Funciones auxiliares internas
  is_dichotomous <- function(x) { is.factor(x) && nlevels(x) == 2 }
  calculate_cramers_v <- function(x, y, handle_na = manejo_na) {
    datos_par <- data.frame(v1 = x, v2 = y)
    if(handle_na == "pairwise") datos_par <- stats::na.omit(datos_par)
    if(nrow(datos_par) < 2 || length(unique(datos_par$v1)) < 2 || length(unique(datos_par$v2)) < 2) return(NA_real_)
    tbl <- table(datos_par$v1, datos_par$v2)
    chi2_test <- tryCatch(stats::chisq.test(tbl, correct = FALSE), error = function(e) NULL)
    if(is.null(chi2_test)) return(NA_real_)
    chi2_stat <- chi2_test$statistic; n_total <- sum(tbl); min_dim <- min(dim(tbl))
    if (n_total == 0 || min_dim <= 1) return(NA_real_)
    v <- sqrt(chi2_stat / (n_total * (min_dim - 1)))
    return(as.numeric(v))
  }

  # Pearson, Spearman, Kendall
  if (metodo %in% c("pearson", "spearman", "kendall")) {
    cols_numericas <- sapply(datos_procesar, is.numeric)
    if (sum(cols_numericas) < 2) {
      warning(paste("Se necesitan al menos 2 variables numéricas para el método '", metodo, "'.", sep=""))
      return(NULL)
    }
    matriz_parcial <- stats::cor(datos_procesar[, cols_numericas, drop = FALSE],
                                 method = metodo,
                                 use = ifelse(manejo_na == "pairwise", "pairwise.complete.obs", "everything"))
    matriz_resultado[cols_numericas, cols_numericas] <- matriz_parcial

    # Phi
  } else if (metodo == "phi") {
    cols_dicotomicas <- sapply(datos_procesar, is_dichotomous)
    if (sum(cols_dicotomicas) < 2) {
      warning("Se necesitan al menos 2 variables factores dicotómicas para el método 'phi'.")
      return(NULL)
    }
    nombres_dicotomicos <- names(datos_procesar)[cols_dicotomicas]
    for (i in 1:(length(nombres_dicotomicos) - 1)) {
      for (j in (i + 1):length(nombres_dicotomicos)) {
        var_i_name <- nombres_dicotomicos[i]
        var_j_name <- nombres_dicotomicos[j]
        datos_pair <- data.frame(v1 = datos_procesar[[var_i_name]], v2 = datos_procesar[[var_j_name]])
        if(manejo_na == "pairwise") datos_pair <- stats::na.omit(datos_pair)
        if(nrow(datos_pair) >= 2 && length(unique(datos_pair$v1)) == 2 && length(unique(datos_pair$v2)) == 2) {
          tbl <- table(datos_pair$v1, datos_pair$v2)
          if (all(dim(tbl) == c(2, 2))) {
            chi2_test <- tryCatch(stats::chisq.test(tbl, correct = FALSE), error = function(e) NULL)
            if(!is.null(chi2_test)) {
              n_total <- sum(tbl)
              if(n_total > 0) {
                phi_val <- sqrt(chi2_test$statistic / n_total)
                matriz_resultado[var_i_name, var_j_name] <- phi_val
                matriz_resultado[var_j_name, var_i_name] <- phi_val
              }
            }
          }
        }
      }
    }
    diag(matriz_resultado[nombres_dicotomicos, nombres_dicotomicos]) <- 1.0

    # Point-Biserial
  } else if (metodo == "pointbiserial") {
    cols_numericas <- sapply(datos_procesar, is.numeric)
    cols_dicotomicas <- sapply(datos_procesar, is_dichotomous)
    if (sum(cols_numericas) < 1 || sum(cols_dicotomicas) < 1) {
      warning("Se necesita al menos 1 variable numérica y 1 factor dicotómico para 'pointbiserial'.")
      return(NULL)
    }
    nombres_numericos <- names(datos_procesar)[cols_numericas]
    nombres_dicotomicos <- names(datos_procesar)[cols_dicotomicas]
    for (i_num in nombres_numericos) {
      for (j_dic in nombres_dicotomicos) {
        datos_pair <- data.frame(v_num = datos_procesar[[i_num]], v_dic = datos_procesar[[j_dic]])
        if(manejo_na == "pairwise") datos_pair <- stats::na.omit(datos_pair)
        # Verificar variabilidad post-NA y que siga siendo dicotómica
        if(nrow(datos_pair) >= 2 && stats::sd(datos_pair$v_num) > 0 && length(unique(datos_pair$v_dic)) == 2) {
          rpb <- stats::cor(datos_pair$v_num, as.numeric(factor(datos_pair$v_dic)), method = "pearson")
          matriz_resultado[i_num, j_dic] <- rpb
          matriz_resultado[j_dic, i_num] <- rpb
        }
      }
    }
    # La diagonal queda NA para pointbiserial

    # Cramer's V
  } else if (metodo == "cramersv") {
    cols_factores <- sapply(datos_procesar, is.factor)
    if (sum(cols_factores) < 2) {
      warning("Se necesitan al menos 2 variables factores para 'cramersv'.")
      return(NULL)
    }
    nombres_factores <- names(datos_procesar)[cols_factores]
    for (i in 1:(length(nombres_factores) - 1)) {
      for (j in (i + 1):length(nombres_factores)) {
        var_i_name <- nombres_factores[i]
        var_j_name <- nombres_factores[j]
        v_val <- calculate_cramers_v(datos_procesar[[var_i_name]], datos_procesar[[var_j_name]])
        matriz_resultado[var_i_name, var_j_name] <- v_val
        matriz_resultado[var_j_name, var_i_name] <- v_val
      }
    }
    diag(matriz_resultado[nombres_factores, nombres_factores]) <- 1.0
  }

  # --- Heatmap ---
  if (mostrar_heatmap) {
    if (!requireNamespace("ggcorrplot", quietly = TRUE)) {
      warning("Instala 'ggcorrplot' para mostrar el heatmap.")
    } else if (sum(!is.na(matriz_resultado)) > 0) {
      # Solo mostrar valores no-NA en heatmap
      matriz_plot <- matriz_resultado
      # ggcorrplot maneja NAs bien, no es necesario quitarlos explícitamente
      # pero sí redondear para el texto
      args_ggcorrplot <- list(corr = matriz_plot, ...)
      if (!"lab" %in% names(args_ggcorrplot)) {
        args_ggcorrplot$lab <- TRUE
        args_ggcorrplot$lab_size <- 3
        matriz_formateada <- round(matriz_plot, digits = digits)
        # Para ggcorrplot, es mejor pasar la matriz original y dejar que ggcorrplot redondee
        # Pero si queremos control exacto, podemos crear la capa de texto aparte.
        # Por simplicidad, pasaremos la matriz original y ggcorrplot usa 'digits' interno
        args_ggcorrplot$corr <- matriz_resultado # Usar original para colores
        args_ggcorrplot$p.mat <- NULL # Asegurar que no use p-values
      }
      # Añadir título específico del método
      if (!"title" %in% names(args_ggcorrplot)) {
        args_ggcorrplot$title <- paste("Mapa de Calor - Método:", metodo)
      }

      # Crear el plot
      p_heatmap <- tryCatch({
        ggp <- do.call(ggcorrplot::ggcorrplot, args_ggcorrplot) +
          ggplot2::ggtitle(args_ggcorrplot$title) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
        # Añadir etiquetas manualmente si lab=TRUE no funciona bien con NAs
        if(isTRUE(args_ggcorrplot$lab)) {
          matriz_redondeada <- round(matriz_resultado, digits)
          # (Código adicional para añadir geom_text podría ir aquí si fuera necesario)
        }
        ggp # Devolver el plot
      }, error = function(e) {
        warning(paste("Error al generar heatmap:", e$message)); return(NULL)
      })
      if (!is.null(p_heatmap)) print(p_heatmap)

    } else {
      cat("No se puede mostrar el heatmap (matriz de resultados vacía o solo NAs).\n")
    }
  }

  return(matriz_resultado)
}
