#' Agregacion rapida de datos epidemiologicos / Fast aggregation of epidemiological data
#'
#' @description
#' Realiza agregaciones rapidas de datos epidemiologicos usando data.table.
#' Soporta agrupacion flexible por multiples variables y calculo de estadisticas.
#' 
#' Performs fast aggregations of epidemiological data using data.table.
#' Supports flexible grouping by multiple variables and statistics calculation.
#'
#' @importFrom data.table := .N data.table
#'
#' @param data data.table o data.frame con datos epidemiologicos.
#'   / data.table or data.frame with epidemiological data.
#' @param by Vector de caracteres o lista con variables de agrupacion.
#'   / Character vector or list with grouping variables.
#' @param summarize Lista con funciones de resumen a aplicar.
#'   / List with summary functions to apply.
#' @param cases_col Nombre de la columna de casos (por defecto busca automaticamente).
#'   / Name of cases column (default searches automatically).
#' @param as_tibble Logico. Si TRUE (predeterminado), devuelve un tibble.
#'   / Logical. If TRUE (default), returns a tibble.
#' @param na.rm Logico. Si TRUE, elimina valores NA en calculos.
#'   / Logical. If TRUE, removes NA values in calculations.
#'
#' @return data.table o tibble con datos agregados / data.table or tibble with aggregated data
#' @export
#'
#' @examples
#' # Cargar datos de malaria / Load malaria data
#' mal <- vp_download("malaria")
#' 
#' # Agregar por ano y departamento / Aggregate by year and department
#' agg1 <- vp_aggregate(mal, by = c("ano", "departamento"))
#' 
#' # Agregar por semana epidemiologica / Aggregate by epidemiological week
#' agg2 <- vp_aggregate(mal, by = c("ano", "semana"))
#' 
#' # Agregar con multiples estadisticas / Aggregate with multiple statistics
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
  
  # Ensure na.rm is a single logical value
  na.rm <- isTRUE(na.rm[1])
  
  # Convertir a data.table si no lo es / Convert to data.table if not
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Detectar columna de casos si no se especifica / Detect cases column if not specified
  if (is.null(cases_col)) {
    # Buscar columnas que puedan contener conteos / Look for columns that might contain counts
    potential_cols <- c("casos", "cases", "cantidad", "count", "n", "total", "value",
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
                 paste(missing_vars, collapse = ", "), paste(missing_vars, collapse = ", ")))
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
      
      # Get function name more robustly
      if (is.function(func)) {
        # Try to get function name from the function itself
        func_name <- tryCatch({
          # First try to match against known functions
          if (identical(func, sum)) "sum"
          else if (identical(func, mean)) "mean"
          else if (identical(func, median)) "median"
          else if (identical(func, min)) "min"
          else if (identical(func, max)) "max"
          else if (identical(func, sd)) "sd"
          else if (identical(func, var)) "var"
          else if (identical(func, mad)) "mad"
          else if (identical(func, length)) "length"
          else if (identical(func, quantile)) "quantile"
          else "unknown"
        }, error = function(e) {
          "unknown"
        })
      } else {
        func_name <- as.character(func)
      }
      
      # Check if function accepts na.rm parameter
      accepts_na_rm <- any(func_name %in% c("sum", "mean", "median", "min", "max", "sd", "var", "mad", "quantile"))
      if (na.rm && accepts_na_rm) {
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

#' Agregacion por ubicacion geografica / Aggregation by geographic location
#'
#' @description
#' Agrega datos por niveles geograficos usando codigos ubigeo.
#' 
#' Aggregates data by geographic levels using ubigeo codes.
#'
#' @param data data.table o data.frame con columna ubigeo.
#'   / data.table or data.frame with ubigeo column.
#' @param level Nivel geografico: "departamento", "provincia", "distrito".
#'   / Geographic level: "departamento", "provincia", "distrito".
#' @param include_totals Logico. Si TRUE, incluye totales nacionales.
#'   / Logical. If TRUE, includes national totals.
#' @param ... Argumentos adicionales para vp_aggregate.
#'   / Additional arguments for vp_aggregate.
#'
#' @return data.table o tibble con datos agregados por ubicacion / data.table or tibble aggregated by location
#' @export
#'
#' @examples
#' \dontrun{
#' # Cargar datos de malaria / Load malaria data
#' mal <- vp_download("malaria")
#' 
#' # Agregar por departamento / Aggregate by department
#' mal_dep <- vp_aggregate_geo(mal, level = "departamento")
#' 
#' # Agregar por provincia con totales / Aggregate by province with totals
#' mal_prov <- vp_aggregate_geo(mal, level = "provincia", include_totals = TRUE)
#' }
vp_aggregate_geo <- function(data, 
                            level = c("departamento", "provincia", "distrito"),
                            include_totals = FALSE,
                            ...) {
  
  level <- match.arg(level)
  
  # Extract additional arguments, excluding 'by' since we set it ourselves
  extra_args <- list(...)
  extra_args$by <- NULL
  
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
    result <- do.call(vp_aggregate, c(list(data = dt_copy, by = "dep_code"), extra_args))
    data.table::setnames(result, "dep_code", "departamento_codigo")
  } else if (level == "provincia") {
    dt_copy[, `:=`(
      dep_code = substr(as.character(ubigeo), 1, 2),
      prov_code = substr(as.character(ubigeo), 1, 4)
    )]
    result <- do.call(vp_aggregate, c(list(data = dt_copy, by = c("dep_code", "prov_code")), extra_args))
    data.table::setnames(result, 
                        c("dep_code", "prov_code"), 
                        c("departamento_codigo", "provincia_codigo"))
  } else {
    # Distrito - usar ubigeo completo / District - use full ubigeo
    result <- do.call(vp_aggregate, c(list(data = data, by = "ubigeo"), extra_args))
  }
  
  # Agregar totales si se solicita / Add totals if requested
  if (include_totals) {
    # For totals, we aggregate the entire dataset without grouping
    # We'll create a dummy grouping variable and then aggregate
    data_copy <- data.table::copy(data)
    data_copy[, dummy_group := "total"]
    # Force data.table output for totals to use := operator
    extra_args_dt <- extra_args
    extra_args_dt$as_tibble <- FALSE
    total_result <- do.call(vp_aggregate, c(list(data = data_copy, by = "dummy_group"), extra_args_dt))
    # Convert to data.table and remove dummy column
    if (!data.table::is.data.table(total_result)) {
      total_result <- data.table::as.data.table(total_result)
    }
    total_result[, dummy_group := NULL]
    
    if (level == "departamento") {
      total_result[, departamento_codigo := "00"]
    } else if (level == "provincia") {
      total_result[, `:=`(departamento_codigo = "00", provincia_codigo = "0000")]
    } else {
      total_result[, ubigeo := "000000"]
    }
    
    result <- data.table::rbindlist(list(total_result, result), use.names = TRUE, fill = TRUE)
  }
  
  return(result)
}

#' Agregacion temporal (series de tiempo) / Temporal aggregation (time series)
#'
#' @description
#' Agrega datos por periodos de tiempo: semana, mes, trimestre, ano.
#' 
#' Aggregates data by time periods: week, month, quarter, year.
#'
#' @param data data.table o data.frame con columnas temporales.
#'   / data.table or data.frame with temporal columns.
#' @param period Periodo de agregacion: "semana", "mes", "trimestre", "ano".
#'   / Aggregation period: "semana", "mes", "trimestre", "año".
#' @param date_cols Lista con nombres de columnas de fecha (ano, semana, etc.).
#'   / List with date column names (year, week, etc.).
#' @param ... Argumentos adicionales para vp_aggregate.
#'   / Additional arguments for vp_aggregate.
#'
#' @return data.table o tibble con serie temporal / data.table or tibble with time series
#' @export
#'
#' @examples
#' \dontrun{
#' # Cargar datos de malaria / Load malaria data
#' mal <- vp_download("malaria")
#' 
#' # Serie mensual / Monthly series
#' mal_mensual <- vp_aggregate_time(mal, period = "mes")
#' 
#' # Serie trimestral por sexo / Quarterly series by sex
#' mal_trim <- vp_aggregate_time(mal, period = "trimestre", by = "sexo")
#' }
vp_aggregate_time <- function(data,
                             period = c("semana", "mes", "trimestre", "ano"),
                             date_cols = NULL,
                             ...) {
  
  period <- match.arg(period)
  
  # Convertir a data.table si no lo es / Convert to data.table if not
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Detectar columnas de fecha si no se especifican / Detect date columns if not specified
  if (is.null(date_cols)) {
    year_cols <- c("ano", "year", "anio")
    week_cols <- c("semana", "week", "semana_epidemiologica", "epiweek")
    
    year_col <- intersect(names(data), year_cols)[1]
    week_col <- intersect(names(data), week_cols)[1]
    
    if (is.na(year_col)) {
      stop("No se encontro columna de ano / Year column not found")
    }
    
    date_cols <- list(year = year_col, week = week_col)
  }
  
  dt_copy <- data.table::copy(data)
  
  # Crear columnas temporales según período / Create temporal columns by period
  if (period == "semana") {
    if (is.na(date_cols$week)) {
      stop("No se encontro columna de semana / Week column not found")
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
    # Ano / Year
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