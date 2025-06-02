#' Descubrir datasets de vigilancia epidemiologica / Discover epidemiological surveillance datasets
#'
#' @description
#' Busca dinamicamente todos los datasets etiquetados con "vigilancia-epidemiologica"
#' en el portal de datos abiertos.
#' 
#' Dynamically searches for all datasets tagged with "vigilancia-epidemiologica"
#' on the open data portal.
#'
#' @param update_known Logico. Si TRUE, actualiza la lista interna de datasets conocidos.
#'   / Logical. If TRUE, updates the internal list of known datasets.
#' @param cache_results Logico. Si TRUE, guarda los resultados en cache.
#'   / Logical. If TRUE, caches the results.
#'
#' @return data.frame con informacion de datasets descubiertos / data.frame with discovered dataset information
#' @export
#'
#' @examples
#' # Descubrir todos los datasets de vigilancia / Discover all surveillance datasets
#' datasets <- vp_discover_datasets()
#' 
#' # Actualizar lista interna / Update internal list
#' vp_discover_datasets(update_known = TRUE)
vp_discover_datasets <- function(update_known = FALSE, cache_results = TRUE) {
  setup_logging()
  
  # URL de busqueda por etiqueta / Tag search URL
  search_url <- "https://datosabiertos.gob.pe/search/field_tags/vigilancia-epidemiol%C3%B3gica-1540"
  
  cache_path <- get_cache_path("discovered_datasets", extension = "rds")
  
  # Intentar usar cache si existe / Try to use cache if exists
  if (cache_results && file.exists(cache_path)) {
    cache_age_hours <- difftime(Sys.time(), file.info(cache_path)$mtime, units = "hours")
    if (cache_age_hours < 24) {  # Cache valido por 24 horas / Cache valid for 24 hours
      futile.logger::flog.info("Usando lista de datasets descubiertos en cache / Using cached discovered datasets list")
      return(readRDS(cache_path))
    }
  }
  
  futile.logger::flog.info("Buscando datasets de vigilancia epidemiologica / Searching for epidemiological surveillance datasets")
  
  tryCatch({
    # Obtener la pagina HTML / Get HTML page
    resp <- httr2::request(search_url) |>
      httr2::req_timeout(30) |>
      httr2::req_user_agent("vigilaPeru R package") |>
      httr2::req_perform()
    
    html_content <- httr2::resp_body_string(resp)
    
    # Extraer informacion de datasets usando regex / Extract dataset info using regex
    # Buscar enlaces a datasets / Look for dataset links
    dataset_pattern <- '<a href="/dataset/([^"]+)"[^>]*>([^<]+)</a>'
    matches <- gregexpr(dataset_pattern, html_content, perl = TRUE)
    
    dataset_info <- list()
    
    if (matches[[1]][1] != -1) {
      # Extraer todos los matches / Extract all matches
      match_data <- regmatches(html_content, matches)[[1]]
      
      for (match in match_data) {
        # Extraer ID y titulo / Extract ID and title
        parts <- regmatches(match, regexec(dataset_pattern, match, perl = TRUE))[[1]]
        if (length(parts) >= 3) {
          dataset_id <- parts[2]
          dataset_title <- parts[3]
          
          # Limpiar titulo HTML / Clean HTML title
          dataset_title <- gsub("&amp;", "&", dataset_title)
          dataset_title <- gsub("&quot;", '"', dataset_title)
          dataset_title <- gsub("&#39;", "'", dataset_title)
          dataset_title <- gsub("<[^>]+>", "", dataset_title)
          dataset_title <- trimws(dataset_title)
          
          # Solo incluir si parece ser un dataset de vigilancia / Only include if it seems to be surveillance dataset
          if (grepl("vigilancia|epidemi", dataset_title, ignore.case = TRUE)) {
            dataset_info[[length(dataset_info) + 1]] <- list(
              id = dataset_id,
              title = dataset_title,
              url = paste0("https://datosabiertos.gob.pe/dataset/", dataset_id)
            )
          }
        }
      }
    }
    
    # Buscar informacion adicional en la pagina / Look for additional info on page
    # Extraer organizaciones / Extract organizations
    org_pattern <- '<div class="organization"[^>]*>\\s*<a[^>]+>([^<]+)</a>'
    org_matches <- gregexpr(org_pattern, html_content, perl = TRUE)
    organizations <- unique(regmatches(html_content, org_matches)[[1]])
    
    if (length(dataset_info) == 0) {
      warning("No se encontraron datasets de vigilancia epidemiologica / No epidemiological surveillance datasets found")
      return(data.frame())
    }
    
    # Convertir a data.frame / Convert to data.frame
    datasets_df <- data.frame(
      id = sapply(dataset_info, function(x) x$id),
      titulo = sapply(dataset_info, function(x) x$title),
      url = sapply(dataset_info, function(x) x$url),
      stringsAsFactors = FALSE
    )
    
    # Eliminar duplicados / Remove duplicates
    datasets_df <- unique(datasets_df)
    
    futile.logger::flog.info(sprintf("Encontrados %d datasets de vigilancia / Found %d surveillance datasets", 
                                     nrow(datasets_df)))
    
    # Guardar en cache si se solicita / Save to cache if requested
    if (cache_results) {
      saveRDS(datasets_df, cache_path)
    }
    
    # Actualizar lista interna si se solicita / Update internal list if requested
    if (update_known) {
      update_known_datasets(datasets_df)
    }
    
    return(datasets_df)
    
  }, error = function(e) {
    # Si hay error, intentar usar cache antiguo / If error, try old cache
    if (file.exists(cache_path)) {
      futile.logger::flog.warn(sprintf("Error descubriendo datasets: %s. Usando cache / Error discovering datasets: %s. Using cache", 
                                       e$message))
      return(readRDS(cache_path))
    } else {
      stop(sprintf("Error descubriendo datasets: %s / Error discovering datasets: %s", e$message))
    }
  })
}

#' Actualizar lista interna de datasets conocidos / Update internal list of known datasets
#' @noRd
update_known_datasets <- function(datasets_df) {
  if (nrow(datasets_df) == 0) return(invisible(NULL))
  
  # Crear mapeo nombre simplificado -> id / Create simplified name -> id mapping
  new_datasets <- list()
  
  for (i in 1:nrow(datasets_df)) {
    # Extraer nombre clave del titulo / Extract key name from title
    title <- tolower(datasets_df$titulo[i])
    
    # Buscar palabras clave / Look for keywords
    name <- NULL
    if (grepl("malaria", title)) name <- "malaria"
    else if (grepl("dengue", title)) name <- "dengue"
    else if (grepl("leishmaniosis|leishmaniasis", title)) name <- "leishmaniasis"
    else if (grepl("ira|infecciones.*respiratorias", title)) name <- "ira"
    else if (grepl("eda|enfermedades.*diarreicas", title)) name <- "eda"
    else if (grepl("covid", title)) name <- "covid19"
    else if (grepl("tuberculosis|tbc", title)) name <- "tuberculosis"
    else if (grepl("vih|sida", title)) name <- "vih"
    else if (grepl("hepatitis", title)) name <- "hepatitis"
    else if (grepl("zika", title)) name <- "zika"
    else if (grepl("chikungunya", title)) name <- "chikungunya"
    else if (grepl("leptospirosis", title)) name <- "leptospirosis"
    else if (grepl("rabia", title)) name <- "rabia"
    else if (grepl("fiebre.*amarilla", title)) name <- "fiebre_amarilla"
    
    if (!is.null(name)) {
      new_datasets[[name]] <- datasets_df$id[i]
    }
  }
  
  # Guardar en archivo para referencia / Save to file for reference
  discovered_path <- file.path(vp_cache_dir(), "discovered_datasets_mapping.rds")
  saveRDS(new_datasets, discovered_path)
  
  futile.logger::flog.info(sprintf("Actualizados %d datasets en mapeo interno / Updated %d datasets in internal mapping", 
                                   length(new_datasets)))
  
  invisible(new_datasets)
}

#' Obtener lista expandida de datasets / Get expanded dataset list
#' @noRd
get_all_known_datasets <- function() {
  # Combinar hardcoded con descubiertos / Combine hardcoded with discovered
  base_datasets <- KNOWN_DATASETS
  
  # Verificar si hay datasets descubiertos / Check for discovered datasets
  discovered_path <- file.path(vp_cache_dir(), "discovered_datasets_mapping.rds")
  if (file.exists(discovered_path)) {
    discovered <- readRDS(discovered_path)
    # Combinar, dando prioridad a los hardcoded / Combine, giving priority to hardcoded
    all_datasets <- c(base_datasets, discovered[!names(discovered) %in% names(base_datasets)])
  } else {
    all_datasets <- base_datasets
  }
  
  return(all_datasets)
}