#' Funciones de utilidad internas / Internal utility functions
#' @noRd

#' Verificar conexión a internet / Check internet connection
#' @noRd
check_internet <- function() {
  tryCatch({
    httr2::request("https://www.datosabiertos.gob.pe") |>
      httr2::req_timeout(5) |>
      httr2::req_perform()
    TRUE
  }, error = function(e) {
    FALSE
  })
}

#' Obtener ruta de caché para un archivo / Get cache path for a file
#' @noRd
get_cache_path <- function(dataset_id, resource_id = NULL, extension = "csv") {
  cache_dir <- vp_cache_dir()
  
  if (is.null(resource_id)) {
    filename <- paste0(dataset_id, "_metadata.json")
  } else {
    filename <- paste0(dataset_id, "_", resource_id, ".", extension)
  }
  
  file.path(cache_dir, filename)
}

#' Verificar si el caché necesita actualización / Check if cache needs update
#' @noRd
cache_needs_update <- function(cache_path, remote_modified = NULL, max_age_hours = 24) {
  if (!file.exists(cache_path)) {
    return(TRUE)
  }
  
  # Si tenemos fecha de modificación remota / If we have remote modification date
  if (!is.null(remote_modified)) {
    cache_time <- file.info(cache_path)$mtime
    
    # Convertir a POSIXct si es necesario / Convert to POSIXct if needed
    if (is.character(remote_modified)) {
      # Limpiar formato de fecha del portal / Clean portal date format
      # Ejemplo: "Date changed  Mié, 12/11/2024 - 14:57"
      remote_modified <- gsub("Date changed\\s+", "", remote_modified)
      
      # Intentar parsear fecha / Try to parse date
      tryCatch({
        # Formato: "Día, MM/DD/YYYY - HH:MM"
        # Extraer solo la parte de fecha y hora / Extract only date and time part
        date_match <- regmatches(remote_modified, regexpr("\\d{2}/\\d{2}/\\d{4}", remote_modified))
        if (length(date_match) > 0) {
          # Convertir MM/DD/YYYY a formato estándar / Convert MM/DD/YYYY to standard format
          parts <- strsplit(date_match[[1]], "/")[[1]]
          if (length(parts) == 3) {
            std_date <- paste(parts[3], parts[1], parts[2], sep = "-")
            remote_modified <- as.POSIXct(std_date, tz = "UTC")
            return(remote_modified > cache_time)
          }
        }
      }, error = function(e) {
        # Si no se puede parsear, usar edad máxima / If can't parse, use max age
        futile.logger::flog.debug(sprintf("No se pudo parsear fecha remota: %s", remote_modified))
      })
    }
  }
  
  # Verificar por edad máxima / Check by max age
  cache_age_hours <- difftime(Sys.time(), file.info(cache_path)$mtime, units = "hours")
  return(cache_age_hours > max_age_hours)
}

#' Configurar logging / Configure logging
#' @noRd
setup_logging <- function(verbose = getOption("vigilaPeru.verbose", FALSE)) {
  if (verbose) {
    futile.logger::flog.threshold(futile.logger::INFO)
  } else {
    futile.logger::flog.threshold(futile.logger::WARN)
  }
}

#' Convertir a tibble si se solicita / Convert to tibble if requested
#' @noRd
maybe_as_tibble <- function(dt, as_tibble = TRUE) {
  if (as_tibble && requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(dt))
  }
  return(dt)
}

#' Validar parámetros de entrada / Validate input parameters
#' @noRd
validate_dataset_id <- function(dataset_id) {
  if (!is.character(dataset_id) || length(dataset_id) != 1 || nchar(dataset_id) == 0) {
    stop("dataset_id debe ser una cadena de caracteres no vacía / dataset_id must be a non-empty character string")
  }
}

#' Formatear bytes a tamaño legible / Format bytes to human readable size
#' @noRd
format_bytes <- function(bytes) {
  if (bytes < 1024) return(paste(bytes, "B"))
  if (bytes < 1024^2) return(paste(round(bytes/1024, 1), "KB"))
  if (bytes < 1024^3) return(paste(round(bytes/1024^2, 1), "MB"))
  return(paste(round(bytes/1024^3, 1), "GB"))
}

#' Extraer metadatos DCAT de respuesta CKAN / Extract DCAT metadata from CKAN response
#' @noRd
extract_dcat_metadata <- function(ckan_response) {
  if (is.null(ckan_response$result)) {
    return(list())
  }
  
  result <- ckan_response$result
  
  # Mapear campos CKAN a términos DCAT / Map CKAN fields to DCAT terms
  dcat_metadata <- list(
    `dct:identifier` = result$id,
    `dct:title` = result$title,
    `dct:description` = result$notes,
    `dct:issued` = result$metadata_created,
    `dct:modified` = result$metadata_modified,
    `dct:publisher` = result$organization$title,
    `dcat:theme` = result$groups,
    `dct:license` = result$license_title,
    `dcat:distribution` = result$resources
  )
  
  # Limpiar valores NULL / Clean NULL values
  dcat_metadata <- dcat_metadata[!sapply(dcat_metadata, is.null)]
  
  return(dcat_metadata)
}

#' Manejo seguro de errores con caché / Safe error handling with cache
#' @noRd
with_cache_fallback <- function(expr, cache_path, error_msg) {
  tryCatch({
    expr
  }, error = function(e) {
    if (file.exists(cache_path)) {
      futile.logger::flog.warn(paste(error_msg, "- usando caché / using cache"))
      return(readRDS(cache_path))
    } else {
      stop(e)
    }
  })
}