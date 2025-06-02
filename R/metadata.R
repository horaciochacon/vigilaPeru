#' Obtener metadatos DCAT/DCAP-AP de un dataset / Get DCAT/DCAP-AP metadata for a dataset
#'
#' @description
#' Recupera los metadatos completos de un dataset en formato DCAT/DCAP-AP,
#' incluyendo informacion sobre recursos, distribuciones y terminos de uso.
#' 
#' Retrieves complete dataset metadata in DCAT/DCAP-AP format,
#' including information about resources, distributions and terms of use.
#'
#' @param dataset Nombre del dataset ("malaria", "dengue", etc.)
#'   / Dataset name ("malaria", "dengue", etc.)
#' @param refresh Logico. Si TRUE, actualiza la informacion desde el portal.
#'   / Logical. If TRUE, refreshes information from the portal.
#'
#' @return Lista con metadatos DCAT / List with DCAT metadata
#' @export
#'
#' @examples
#' # Obtener metadatos de malaria / Get malaria metadata
#' metadata <- vp_metadata("malaria")
#' 
#' # Ver terminos DCAT disponibles / View available DCAT terms
#' names(metadata)
#' 
#' # Acceder a distribuciones / Access distributions
#' metadata$`dcat:distribution`
vp_metadata <- function(dataset, refresh = FALSE) {
  validate_dataset_id(dataset)
  setup_logging()
  
  # Resolver nombre a ID / Resolve name to ID
  dataset_id <- resolve_dataset_id(dataset)
  
  cache_path <- get_cache_path(paste0("metadata_", dataset), extension = "rds")
  
  # Verificar cache / Check cache
  if (!refresh && file.exists(cache_path)) {
    futile.logger::flog.info(sprintf("Usando metadatos en cache para %s / Using cached metadata for %s", 
                                     dataset, dataset))
    return(readRDS(cache_path))
  }
  
  # Obtener metadatos frescos / Get fresh metadata
  futile.logger::flog.info(sprintf("Obteniendo metadatos para %s / Getting metadata for %s", 
                                   dataset, dataset))
  
  tryCatch({
    raw_metadata <- get_dataset_metadata(dataset_id)
    
    # Convertir a formato DCAT / Convert to DCAT format
    dcat_metadata <- list(
      `@context` = "https://www.w3.org/ns/dcat",
      `@type` = "dcat:Dataset",
      `dct:identifier` = raw_metadata$id,
      `dct:title` = raw_metadata$title,
      `dct:description` = raw_metadata$notes,
      `dct:issued` = raw_metadata$metadata_created,
      `dct:modified` = raw_metadata$metadata_modified,
      `dcat:landingPage` = raw_metadata$url,
      `dct:license` = raw_metadata$license_title,
      `dct:publisher` = list(
        `@type` = "foaf:Organization",
        `foaf:name` = if(!is.null(raw_metadata$organization) && !is.null(raw_metadata$organization$title)) {
          raw_metadata$organization$title
        } else {
          "Centro Nacional de Epidemiologia, Prevencion y Control de Enfermedades"
        }
      ),
      `dcat:theme` = if(!is.null(raw_metadata$tags)) {
        lapply(raw_metadata$tags, function(tag) {
          if(is.list(tag$name)) tag$name[[1]] else tag$name
        })
      } else {
        list("epidemiologia", "salud publica", "vigilancia")
      },
      `dcat:distribution` = lapply(raw_metadata$resources, function(r) {
        list(
          `@type` = "dcat:Distribution",
          `dct:identifier` = if(is.list(r$id)) r$id[[1]] else r$id,
          `dct:title` = if(is.list(r$name)) r$name[[1]] else r$name,
          `dct:description` = if(is.list(r$description)) r$description[[1]] else r$description,
          `dcat:accessURL` = if(is.list(r$url)) r$url[[1]] else r$url,
          `dcat:downloadURL` = if(is.list(r$url)) r$url[[1]] else r$url,
          `dct:format` = if(is.list(r$format)) r$format[[1]] else r$format,
          `dcat:byteSize` = parse_size_to_bytes(if(is.list(r$size)) r$size[[1]] else r$size),
          `dct:issued` = if(is.list(r$created)) r$created[[1]] else r$created,
          `dct:modified` = if(is.list(r$last_modified)) r$last_modified[[1]] else r$last_modified
        )
      }),
      `dcat:keyword` = extract_keywords_from_description(raw_metadata$notes),
      `dct:spatial` = list(
        `@type` = "dct:Location",
        `locn:geometry` = "POLYGON((-81.4109 -18.3479, -68.6650 -18.3479, -68.6650 -0.0572, -75.0889 -0.0572, -81.4109 -18.3479))",
        `locn:name` = "Peru"
      ),
      `dct:temporal` = extract_temporal_coverage(raw_metadata),
      `_raw` = raw_metadata  # Incluir datos crudos para referencia / Include raw data for reference
    )
    
    # Guardar en cache / Save to cache
    saveRDS(dcat_metadata, cache_path)
    futile.logger::flog.info(sprintf("Metadatos guardados en cache / Metadata saved to cache"))
    
    return(dcat_metadata)
    
  }, error = function(e) {
    # Si hay error, intentar usar cache antiguo / If error, try to use old cache
    if (file.exists(cache_path)) {
      futile.logger::flog.warn(sprintf("Error obteniendo metadatos: %s. Usando cache / Error getting metadata: %s. Using cache", 
                                       e$message))
      return(readRDS(cache_path))
    } else {
      stop(e)
    }
  })
}

#' Funciones auxiliares para metadatos / Metadata helper functions
#' @noRd

# Convertir tamano a bytes / Convert size to bytes
parse_size_to_bytes <- function(size_str) {
  if (is.null(size_str) || size_str == "") return(NULL)
  
  # Remover espacios / Remove spaces
  size_str <- gsub(" ", "", size_str)
  
  # Extraer numero y unidad / Extract number and unit
  matches <- regmatches(size_str, regexec("([0-9.]+)([A-Za-z]+)", size_str))
  
  if (length(matches[[1]]) < 3) return(NULL)
  
  value <- as.numeric(matches[[1]][2])
  unit <- toupper(matches[[1]][3])
  
  multiplier <- switch(unit,
    "B" = 1,
    "KB" = 1024,
    "MB" = 1024^2,
    "GB" = 1024^3,
    1
  )
  
  return(as.integer(value * multiplier))
}

# Extraer palabras clave de la descripcion / Extract keywords from description
extract_keywords_from_description <- function(description) {
  if (is.null(description)) return(list())
  
  # Lista de palabras clave comunes / Common keywords list
  keywords <- c("malaria", "dengue", "leishmaniasis", "epidemiologia", "vigilancia", 
                "salud publica", "RENACE", "CDC", "Peru", "notificacion", "casos")
  
  # Buscar cuales estan presentes / Find which are present
  found <- keywords[sapply(keywords, function(k) grepl(k, description, ignore.case = TRUE))]
  
  return(as.list(found))
}

# Extraer cobertura temporal / Extract temporal coverage
extract_temporal_coverage <- function(metadata) {
  # Buscar anos en los nombres de recursos y descripcion / Look for years in resource names and description
  years <- c()
  
  # Buscar en recursos / Search in resources
  if (!is.null(metadata$resources)) {
    for (r in metadata$resources) {
      name <- if(is.list(r$name)) r$name[[1]] else r$name
      # Extraer anos (4 digitos) / Extract years (4 digits)
      year_matches <- gregexpr("\\b(19|20)\\d{2}\\b", name)
      if (year_matches[[1]][1] != -1) {
        years <- c(years, regmatches(name, year_matches)[[1]])
      }
    }
  }
  
  # Buscar en la descripcion / Search in description
  if (!is.null(metadata$notes) && length(years) == 0) {
    year_matches <- gregexpr("\\b(19|20)\\d{2}\\b", metadata$notes)
    if (year_matches[[1]][1] != -1) {
      years <- c(years, regmatches(metadata$notes, year_matches)[[1]])
    }
  }
  
  if (length(years) > 0) {
    years <- as.numeric(unique(years))
    # Filtrar anos futuros / Filter future years
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    years <- years[years <= current_year]
    
    if (length(years) > 0) {
      return(list(
        `dcat:startDate` = paste0(min(years), "-01-01"),
        `dcat:endDate` = paste0(max(years), "-12-31")
      ))
    }
  }
  
  return(NULL)
}