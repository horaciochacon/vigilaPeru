#' Agregaci\u00f3n r\u00e1pida de datos epidemiol\u00f3gicos / Fast aggregation of epidemiological data
#'
#' @description
#' Realiza agregaciones r\u00e1pidas de datos epidemiol\u00f3gicos usando data.table.
#' Soporta agrupaci\u00f3n flexible por m\u00faltiples variables y c\u00e1lculo de estad\u00edsticas.
#' 
#' Performs fast aggregations of epidemiological data using data.table.
#' Supports flexible grouping by multiple variables and statistics calculation.
#'
#' @importFrom data.table := .N data.table
#'
#' @param data data.table o data.frame con datos epidemiol\u00f3gicos.
#'   / data.table or data.frame with epidemiological data.
#' @param by Vector de caracteres o lista con variables de agrupaci\u00f3n.
#'   / Character vector or list with grouping variables.
#' @param summarize Lista con funciones de resumen a aplicar.
#'   / List with summary functions to apply.
#' @param cases_col Nombre de la columna de casos (por defecto busca autom\u00e1ticamente).
#'   / Name of cases column (default searches automatically).
#' @param as_tibble L\u00f3gico. Si TRUE (predeterminado), devuelve un tibble.
#'   / Logical. If TRUE (default), returns a tibble.
#' @param na.rm L\u00f3gico. Si TRUE, elimina valores NA en c\u00e1lculos.
#'   / Logical. If TRUE, removes NA values in calculations.
#'
#' @return data.table o tibble con datos agregados / data.table or tibble with aggregated data
#' @export
#'
#' @examples
#' # Cargar datos de malaria / Load malaria data
#' mal <- vp_download("malaria")
#' 
#' # Agregar por a\u00f1o y departamento / Aggregate by year and department
#' agg1 <- vp_aggregate(mal, by = c("ano", "departamento"))
#' 
#' # Agregar por semana epidemiol\u00f3gica / Aggregate by epidemiological week
#' agg2 <- vp_aggregate(mal, by = list(year = "ano", week = "semana"))
#' 
#' # Agregar con m\u00faltiples estad\u00edsticas / Aggregate with multiple statistics
#' agg3 <- vp_aggregate(mal, 
#'   by = c("ano", "sexo"),
#'   summarize = list(
#'     casos_total = sum,
#'     casos_promedio = mean,
#'     casos_max = max
#'   )
#' )
vp_aggregate <- function(data, 
                        by, 
                        summarize = NULL,
                        cases_col = NULL,
                        as_tibble = TRUE,
                        na.rm = TRUE) {
  
  # Convertir a data.table si no lo es / Convert to data.table if not
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Detectar columna de casos si no se especifica / Detect cases column if not specified
  if (is.null(cases_col)) {
    # Buscar columnas que puedan contener conteos / Look for columns that might contain counts
    potential_cols <- c("casos", "cases", "cantidad", "count", "n", "total",
                       "vivax", "falciparum", "confirmados", "probables", "sospechosos")
    
    # Verificar cuáles existen / Check which exist
    existing <- intersect(tolower(names(data)), potential_cols)
    
    if (length(existing) == 0) {
      # Si no hay columna de casos, contar filas / If no cases column, count rows
      cases_col <- NULL
      count_rows <- TRUE
    } else {
      cases_col <- existing[1]
      count_rows <- FALSE
      futile.logger::flog.debug(sprintf("Usando columna de casos: %s / Using cases column: %s", 
                                       cases_col, cases_col))
    }
  } else {
    count_rows <- FALSE
  }
  
  # Procesar variables de agrupación / Process grouping variables
  if (is.list(by) && !is.data.frame(by)) {
    # Si es una lista con nombres, usar para renombrar / If named list, use for renaming
    by_vars <- names(by)
    if (is.null(by_vars)) {
      by_vars <- as.character(by)
    }
  } else if (is.character(by)) {
    by_vars <- by
  } else {
    stop("'by' debe ser un vector de caracteres o lista / 'by' must be character vector or list")
  }
  
  # Verificar que las variables existen / Verify variables exist
  missing_vars <- setdiff(by_vars, names(data))
  if (length(missing_vars) > 0) {
    stop(sprintf("Variables no encontradas: %s / Variables not found: %s", 
                 paste(missing_vars, collapse = ", ")))
  }
  
  # Preparar funciones de resumen / Prepare summary functions
  if (is.null(summarize)) {
    if (count_rows) {
      # Contar filas si no hay columna de casos / Count rows if no cases column
      summarize <- list(casos = length)
    } else {
      # Suma por defecto / Default to sum
      summarize <- list(casos = sum)
    }
  }
  
  # Construir expresión de agregación / Build aggregation expression
  if (count_rows) {
    # Contar filas por grupo / Count rows per group
    agg_expr <- quote(.N)
    result <- data[, .(casos = .N), by = by_vars]
  } else {
    # Aplicar funciones de resumen / Apply summary functions
    agg_list <- list()
    
    for (stat_name in names(summarize)) {
      func <- summarize[[stat_name]]
      if (na.rm) {
        agg_list[[stat_name]] <- substitute(
          func(col, na.rm = TRUE),
          list(func = func, col = as.name(cases_col))
        )
      } else {
        agg_list[[stat_name]] <- substitute(
          func(col),
          list(func = func, col = as.name(cases_col))
        )
      }
    }
    
    # Ejecutar agregación / Execute aggregation
    result <- data[, eval(as.call(c(as.name("."), agg_list))), by = by_vars]
  }
  
  # Ordenar por variables de agrupación / Sort by grouping variables
  data.table::setkeyv(result, by_vars)
  
  # Agregar metadatos útiles / Add useful metadata
  attr(result, "aggregated_by") <- by_vars
  attr(result, "aggregation_date") <- Sys.Date()
  
  # Convertir a tibble si se solicita / Convert to tibble if requested
  return(maybe_as_tibble(result, as_tibble))
}

#' Agregaci\u00f3n por ubicaci\u00f3n geogr\u00e1fica / Aggregation by geographic location
#'
#' @description
#' Agrega datos por niveles geogr\u00e1ficos usando c\u00f3digos ubigeo.
#' 
#' Aggregates data by geographic levels using ubigeo codes.
#'
#' @param data data.table o data.frame con columna ubigeo.
#'   / data.table or data.frame with ubigeo column.
#' @param level Nivel geogr\u00e1fico: "departamento", "provincia", "distrito".
#'   / Geographic level: "departamento", "provincia", "distrito".
#' @param include_totals L\u00f3gico. Si TRUE, incluye totales nacionales.
#'   / Logical. If TRUE, includes national totals.
#' @param ... Argumentos adicionales para vp_aggregate.
#'   / Additional arguments for vp_aggregate.
#'
#' @return data.table o tibble con datos agregados por ubicaci\u00f3n / data.table or tibble aggregated by location
#' @export
#'
#' @examples
#' # Agregar por departamento / Aggregate by department
#' mal_dep <- vp_aggregate_geo(mal, level = "departamento")
#' 
#' # Agregar por provincia con totales / Aggregate by province with totals
#' mal_prov <- vp_aggregate_geo(mal, level = "provincia", include_totals = TRUE)
vp_aggregate_geo <- function(data, 
                            level = c("departamento", "provincia", "distrito"),
                            include_totals = FALSE,
                            ...) {
  
  level <- match.arg(level)
  
  # Verificar que existe columna ubigeo / Check ubigeo column exists
  if (!"ubigeo" %in% names(data)) {
    stop("Datos deben contener columna 'ubigeo' / Data must contain 'ubigeo' column")
  }
  
  # Convertir a data.table si no lo es / Convert to data.table if not
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Crear columnas geográficas basadas en ubigeo / Create geographic columns based on ubigeo
  # Ubigeo: DDPPTT (DD=depto, PP=provincia, TT=distrito)
  dt_copy <- data.table::copy(data)
  
  if (level == "departamento") {
    dt_copy[, dep_code := substr(as.character(ubigeo), 1, 2)]
    result <- vp_aggregate(dt_copy, by = "dep_code", ...)
    data.table::setnames(result, "dep_code", "departamento_codigo")
  } else if (level == "provincia") {
    dt_copy[, `:=`(
      dep_code = substr(as.character(ubigeo), 1, 2),
      prov_code = substr(as.character(ubigeo), 1, 4)
    )]
    result <- vp_aggregate(dt_copy, by = c("dep_code", "prov_code"), ...)
    data.table::setnames(result, 
                        c("dep_code", "prov_code"), 
                        c("departamento_codigo", "provincia_codigo"))
  } else {
    # Distrito - usar ubigeo completo / District - use full ubigeo
    result <- vp_aggregate(data, by = "ubigeo", ...)
  }
  
  # Agregar totales si se solicita / Add totals if requested
  if (include_totals) {
    total_row <- vp_aggregate(data, by = NULL, ...)
    
    if (level == "departamento") {
      total_row[, departamento_codigo := "00"]
    } else if (level == "provincia") {
      total_row[, `:=`(departamento_codigo = "00", provincia_codigo = "0000")]
    } else {
      total_row[, ubigeo := "000000"]
    }
    
    result <- data.table::rbindlist(list(total_row, result), use.names = TRUE, fill = TRUE)
  }
  
  return(result)
}

#' Agregaci\u00f3n temporal (series de tiempo) / Temporal aggregation (time series)
#'
#' @description
#' Agrega datos por per\u00edodos de tiempo: semana, mes, trimestre, a\u00f1o.
#' 
#' Aggregates data by time periods: week, month, quarter, year.
#'
#' @param data data.table o data.frame con columnas temporales.
#'   / data.table or data.frame with temporal columns.
#' @param period Per\u00edodo de agregaci\u00f3n: "semana", "mes", "trimestre", "a\u00f1o".
#'   / Aggregation period: "semana", "mes", "trimestre", "año".
#' @param date_cols Lista con nombres de columnas de fecha (a\u00f1o, semana, etc.).
#'   / List with date column names (year, week, etc.).
#' @param ... Argumentos adicionales para vp_aggregate.
#'   / Additional arguments for vp_aggregate.
#'
#' @return data.table o tibble con serie temporal / data.table or tibble with time series
#' @export
#'
#' @examples
#' # Serie mensual / Monthly series
#' mal_mensual <- vp_aggregate_time(mal, period = "mes")
#' 
#' # Serie trimestral por sexo / Quarterly series by sex
#' mal_trim <- vp_aggregate_time(mal, period = "trimestre", by = "sexo")
vp_aggregate_time <- function(data,
                             period = c("semana", "mes", "trimestre", "a\u00f1o"),
                             date_cols = NULL,
                             ...) {
  
  period <- match.arg(period)
  
  # Convertir a data.table si no lo es / Convert to data.table if not
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Detectar columnas de fecha si no se especifican / Detect date columns if not specified
  if (is.null(date_cols)) {
    year_cols <- c("ano", "a\u00f1o", "year", "anio")
    week_cols <- c("semana", "week", "semana_epidemiologica", "epiweek")
    
    year_col <- intersect(names(data), year_cols)[1]
    week_col <- intersect(names(data), week_cols)[1]
    
    if (is.na(year_col)) {
      stop("No se encontr\u00f3 columna de a\u00f1o / Year column not found")
    }
    
    date_cols <- list(year = year_col, week = week_col)
  }
  
  dt_copy <- data.table::copy(data)
  
  # Crear columnas temporales según período / Create temporal columns by period
  if (period == "semana") {
    if (is.na(date_cols$week)) {
      stop("No se encontr\u00f3 columna de semana / Week column not found")
    }
    by_vars <- c(date_cols$year, date_cols$week)
  } else if (period == "mes") {
    # Convertir semana a mes (aproximado) / Convert week to month (approximate)
    if (!is.na(date_cols$week)) {
      dt_copy[, mes := ceiling(get(date_cols$week) * 12 / 52)]
    } else {
      stop("No se puede calcular mes sin columna de semana / Cannot calculate month without week column")
    }
    by_vars <- c(date_cols$year, "mes")
  } else if (period == "trimestre") {
    # Convertir semana a trimestre / Convert week to quarter
    if (!is.na(date_cols$week)) {
      dt_copy[, trimestre := ceiling(get(date_cols$week) * 4 / 52)]
    } else {
      stop("No se puede calcular trimestre sin columna de semana / Cannot calculate quarter without week column")
    }
    by_vars <- c(date_cols$year, "trimestre")
  } else {
    # A\u00f1o / Year
    by_vars <- date_cols$year
  }
  
  # Agregar by adicionales si se proporcionan / Add additional by if provided
  dots <- list(...)
  if ("by" %in% names(dots)) {
    by_vars <- c(by_vars, dots$by)
    dots$by <- NULL
  }
  
  # Ejecutar agregación / Execute aggregation
  do.call(vp_aggregate, c(list(data = dt_copy, by = by_vars), dots))
}