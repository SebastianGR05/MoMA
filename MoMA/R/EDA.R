#' Análisis Exploratorio de Datos (EDA)
#'
#' Realiza un análisis exploratorio, incluyendo resumen básico, sugerencias
#' de tipos de variables, histogramas para numéricas, y gráficos de barras
#' y circulares para categóricas.
#'
#' @param datos Un data frame o tibble.
#' @param max_cat_levels Número máximo de niveles para considerar una variable
#'   categórica para los gráficos de barras/circulares. Ayuda a evitar gráficos
#'   ilegibles con demasiadas categorías. Por defecto 30.
#'
#' @return Imprime en consola resúmenes, recomendaciones y muestra gráficos.
#'   No devuelve un objeto R específico, su valor principal son los efectos
#'   secundarios (impresión y gráficos).
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram labs ggtitle theme_minimal geom_bar coord_flip geom_col coord_polar scale_fill_brewer theme_void element_text geom_text position_stack
#' @importFrom dplyr summarise n group_by mutate arrange desc .data
#' @importFrom utils head str
#' @importFrom stats complete.cases sd
#'
#' @examples
#' # Crear datos de ejemplo diversos
#' set.seed(123)
#' df_prueba_eda <- data.frame(
#'   ID_Usuario = 1:100,
#'   Numerica_Normal = rnorm(100, 50, 15),
#'   Numerica_Sesgada = rgamma(100, 2, 0.1),
#'   Numerica_Constante = rep(10, 100),
#'   Categorica_PocosNiveles = factor(sample(c("A", "B", "C", NA), 100, replace = TRUE)),
#'   Categorica_MuchosNiveles = factor(sample(paste0("Cat", 1:50), 100, replace = TRUE)),
#'   Logica = sample(c(TRUE, FALSE), 100, replace = TRUE),
#'   Texto = sample(c("Rojo", "Verde", "Azul", "Verde"), 100, replace = TRUE),
#'   Numerica_Con_NA = sample(c(1:5, NA), 100, replace = TRUE)
#' )
#'
#' \dontrun{
#' # Ejecutar EDA
#' EDA(df_prueba_eda)
#' }
#'
#' @export
EDA <- function(datos, max_cat_levels = 30) {

  if (!is.data.frame(datos)) {
    stop("El argumento 'datos' debe ser un data frame.")
  }
  if (nrow(datos) == 0) {
    message("El data frame está vacío.")
    return(invisible(NULL))
  }

  cat("--- Análisis Exploratorio de Datos (EDA) ---\n")
  cat(paste("Dataset:", deparse(substitute(datos)), "\n"))
  cat(paste("Dimensiones:", nrow(datos), "filas,", ncol(datos), "columnas\n"))

  cat("\n--- 1. Resumen Básico ---\n")
  cat("Estructura (str):\n")
  utils::str(datos, list.len = 6, give.attr = FALSE) # Limitar output de str
  cat("\nPrimeras filas (head):\n")
  print(utils::head(datos))
  cat("\nResumen Estadístico (summary):\n")
  print(summary(datos))

  # --- 2. Sugerencias y Tipos de Variables ---
  cat("\n--- 2. Tipos de Variables y Sugerencias ---\n")
  tipos <- sapply(datos, class)
  n_unicos <- sapply(datos, function(x) length(unique(x)))
  n_faltantes <- colSums(is.na(datos))

  for (i in 1:ncol(datos)) {
    col_nombre <- names(datos)[i]
    col_tipo <- paste(tipos[[i]], collapse=", ") # Para clases múltiples como POSIXct, POSIXt
    col_unicos <- n_unicos[i]
    col_faltantes <- n_faltantes[i]

    cat(paste0("* Columna '", col_nombre, "':"))
    cat(paste0(" Tipo=", col_tipo, ","))
    cat(paste0(" Únicos=", col_unicos, ","))
    cat(paste0(" Faltantes=", col_faltantes, " (", round(100*col_faltantes/nrow(datos), 1), "%)\n"))

    # Sugerencias
    if (col_tipo == "numeric" || col_tipo == "integer") {
      if (col_unicos <= 1 && !all(is.na(datos[[i]]))) cat("    - Sugerencia: Variable numérica constante (o casi). Considera excluirla si no aporta variabilidad.\n")
      if (stats::sd(datos[[i]], na.rm = TRUE) == 0 && !all(is.na(datos[[i]]))) cat("    - Sugerencia: Varianza cero detectada.\n")
      cat("    - Sugerencia: Tipo 'Numérico'. Adecuada para análisis cuantitativos.\n")
    } else if (col_tipo == "factor") {
      if (col_unicos > max_cat_levels) cat(paste0("    - Sugerencia: Factor con muchos niveles (", col_unicos, "). Podría ser difícil de visualizar/analizar directamente. Considera agrupar niveles o tratarla de otra forma.\n"))
      else cat("    - Sugerencia: Tipo 'Factor'. Adecuada para análisis categóricos.\n")
    } else if (col_tipo == "character") {
      cat("    - Sugerencia: Tipo 'Texto'. Considera convertir a 'Factor' para análisis categóricos con `as.factor()`.\n")
      if (col_unicos / nrow(datos) > 0.9 && nrow(datos) > 50) cat ("    - Sugerencia: Muchos valores únicos. ¿Podría ser un identificador o texto libre?\n")
    } else if (col_tipo == "logical") {
      cat("    - Sugerencia: Tipo 'Lógico'. Puede usarse directamente o convertir a factor ('Si'/'No') o numérico (1/0).\n")
    } else if (grepl("POSIXct|POSIXt|Date", col_tipo)) {
      cat("    - Sugerencia: Tipo 'Fecha/Hora'. Adecuada para análisis de series de tiempo o extracción de componentes (año, mes, día...).\n")
    } else {
      cat("    - Sugerencia: Tipo no estándar. Revisar naturaleza de los datos.\n")
    }
    if (col_faltantes > 0) {
      if(col_faltantes / nrow(datos) > 0.5) cat(paste0("    - Advertencia: Alto porcentaje de valores faltantes (", round(100*col_faltantes/nrow(datos), 1), "%). Considera imputación o exclusión.\n"))
      else cat("    - Nota: Contiene valores faltantes a considerar.\n")
    }

  }
  cat("*Fin de sugerencias*\n")


  # --- 3. Gráficos para Variables Numéricas ---
  cat("\n--- 3. Histogramas para Variables Numéricas ---\n")
  columnas_numericas <- names(datos)[sapply(datos, is.numeric)]
  if (length(columnas_numericas) > 0) {
    for (col_num in columnas_numericas) {
      # Evitar error si hay varianza cero o solo NAs
      if (stats::sd(datos[[col_num]], na.rm = TRUE) > 0 || sum(!is.na(datos[[col_num]])) > 1) {
        # Crear copia para no modificar original si hay NAs
        plot_data_num <- data.frame(valor = datos[[col_num]])

        p_hist <- ggplot2::ggplot(plot_data_num, ggplot2::aes(x = .data$valor)) +
          ggplot2::geom_histogram(bins = min(30, max(10, round(nrow(datos)/10))), fill = "skyblue", color = "black", na.rm = TRUE) +
          ggplot2::labs(title = paste("Histograma de", col_num), x = col_num, y = "Frecuencia") +
          ggplot2::theme_minimal()
        print(p_hist)
      } else {
        cat(paste("* Omitiendo histograma para '", col_num, "' (varianza cero o datos insuficientes).\n", sep=""))
      }
    }
  } else {
    cat("No se encontraron variables numéricas adecuadas para histogramas.\n")
  }


  # --- 4. Gráficos para Variables Categóricas ---
  cat("\n--- 4. Gráficos para Variables Categóricas (Factores/Texto con niveles <= ", max_cat_levels, ") ---\n")
  columnas_categoricas <- names(datos)[sapply(datos, function(x) is.factor(x) || is.character(x))]

  if (length(columnas_categoricas) > 0) {
    # Convertir caracteres a factores temporalmente para análisis
    datos_temp_cat <- datos[, columnas_categoricas, drop = FALSE]
    for(col_char in names(datos_temp_cat)[sapply(datos_temp_cat, is.character)]) {
      datos_temp_cat[[col_char]] <- factor(datos_temp_cat[[col_char]])
    }

    # Filtrar por número de niveles
    niveles_por_col <- sapply(datos_temp_cat, function(x) nlevels(x))
    cols_a_graficar <- names(niveles_por_col[niveles_por_col <= max_cat_levels & niveles_por_col > 0])

    if (length(cols_a_graficar) > 0) {
      for (col_cat in cols_a_graficar) {
        cat(paste("\n** Gráficos para:", col_cat, "**\n"))

        # Datos para gráficos (manejando NA como categoría opcionalmente o excluyéndolo)
        # Aquí los excluimos por simplicidad con na.omit, pero se podría hacer un factor explícito
        plot_data_cat <- data.frame(categoria = stats::na.omit(datos_temp_cat[[col_cat]]))
        if(nrow(plot_data_cat) == 0) {
          cat(paste("* Omitiendo gráficos para '", col_cat, "' (solo valores faltantes).\n", sep=""))
          next # Saltar al siguiente loop
        }

        # a) Gráfico de Barras
        p_bar <- ggplot2::ggplot(plot_data_cat, ggplot2::aes(y = .data$categoria)) + # y=categoria para barras horizontales
          ggplot2::geom_bar(fill = "lightblue", color = "black") +
          ggplot2::labs(title = paste("Diagrama de Barras de", col_cat), x = "Frecuencia", y = col_cat) +
          ggplot2::theme_minimal()
        print(p_bar)

        # b) Gráfico Circular (Pie Chart)
        # Calcular frecuencias para el pie chart
        pie_data <- plot_data_cat |>
          dplyr::group_by(.data$categoria) |>
          dplyr::summarise(n = dplyr::n(), .groups = 'drop') |>
          dplyr::mutate(prop = .data$n / sum(.data$n) * 100,
                        etiqueta_pos = cumsum(.data$prop) - 0.5 * .data$prop, # Posición de etiquetas
                        etiqueta = paste0(round(.data$prop, 1), "%")) |>
          dplyr::arrange(dplyr::desc(.data$categoria)) # Ordenar para mejor visualización

        # No mostrar etiquetas si hay muchas categorías o son muy pequeñas
        if (nrow(pie_data) > 8) {
          pie_data$etiqueta <- ""
        } else {
          pie_data$etiqueta[pie_data$prop < 5] <- "" # Ocultar etiquetas < 5%
        }


        p_pie <- ggplot2::ggplot(pie_data, ggplot2::aes(x = "", y = .data$prop, fill = .data$categoria)) +
          ggplot2::geom_col(width = 1, color = "white") +
          ggplot2::coord_polar(theta = "y") + # Transformación a pie chart
          # Usar una paleta de colores (ej. Brewer)
          ggplot2::scale_fill_brewer(palette="Pastel1") +
          ggplot2::geom_text(ggplot2::aes(y = .data$etiqueta_pos, label = .data$etiqueta), color = "black", size=3.5) +
          ggplot2::labs(title = paste("Diagrama Circular de", col_cat), fill = col_cat) +
          ggplot2::theme_void() + # Tema limpio para pie charts
          ggplot2::theme(legend.position = "right",
                         plot.title = ggplot2::element_text(hjust = 0.5)) # Centrar título

        print(p_pie)
      }
    } else {
      cat("No se encontraron variables categóricas con un número adecuado de niveles (<= ", max_cat_levels, ") para graficar.\n")
    }

  } else {
    cat("No se encontraron variables categóricas (factor/character) en el dataset.\n")
  }

  cat("\n--- Fin del EDA ---\n")
  invisible(NULL) # No devuelve nada explícitamente
}
