#' Funciones auxiliares para API CKAN / CKAN API helper functions
#' @noRd

# URL base del portal de datos abiertos / Open data portal base URL
CKAN_BASE_URL <- "https://www.datosabiertos.gob.pe/api/3/action/"

# Nota: Este portal parece tener solo package_show habilitado, no package_search
# Note: This portal seems to only have package_show enabled, not package_search

#' Cliente CKAN interno / Internal CKAN client
#' @noRd
ckan_request <- function(action, params = list(), timeout = 30) {
  url <- paste0(CKAN_BASE_URL, action)
  
  request <- httr2::request(url) |>
    httr2::req_timeout(timeout) |>
    httr2::req_user_agent("vigilaPeru R package") |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2)
  
  if (length(params) > 0) {
    request <- request |> httr2::req_url_query(!!!params)
  }
  
  response <- request |> httr2::req_perform()
  
  if (httr2::resp_status(response) != 200) {
    stop(sprintf("Error en API CKAN: %s / CKAN API error: %s", 
                 httr2::resp_status(response)))
  }
  
  content <- httr2::resp_body_json(response)
  
  if (!content$success) {
    stop(sprintf("Error CKAN: %s / CKAN error: %s", 
                 content$error$message))
  }
  
  # Este portal devuelve result como una lista de un elemento
  # This portal returns result as a single-element list
  if (length(content$result) == 1 && is.list(content$result[[1]])) {
    content$result <- content$result[[1]]
  }
  
  return(content)
}

#' Buscar datasets por organización y etiquetas / Search datasets by organization and tags
#' @noRd
search_cdc_datasets <- function(rows = 1000, start = 0) {
  # package_search no está disponible en este portal
  # package_search is not available on this portal
  # Retornamos una lista vacía o usamos datasets conocidos
  # Return empty list or use known datasets
  
  warning("La búsqueda de datasets no está disponible en este portal. Usando lista predefinida. / Dataset search not available on this portal. Using predefined list.")
  
  # Retornar estructura similar a package_search con datasets conocidos
  # Return structure similar to package_search with known datasets
  return(list(
    count = length(KNOWN_DATASETS),
    results = lapply(names(KNOWN_DATASETS), function(name) {
      list(
        name = KNOWN_DATASETS[[name]],
        title = paste("Vigilancia epidemiológica de", name)
      )
    })
  ))
}

#' Obtener metadatos de un dataset específico / Get metadata for specific dataset
#' @noRd
get_dataset_metadata <- function(dataset_id) {
  params <- list(id = dataset_id)
  result <- ckan_request("package_show", params)
  
  # Normalizar la estructura de datos (convertir arrays a valores únicos)
  # Normalize data structure (convert arrays to single values)
  metadata <- result$result
  
  # Función auxiliar para extraer primer elemento de arrays
  # Helper function to extract first element from arrays
  extract_first <- function(x) {
    if (is.list(x) && length(x) == 1) x[[1]] else x
  }
  
  # Normalizar campos principales / Normalize main fields
  for (field in names(metadata)) {
    if (field != "resources" && field != "tags" && field != "extras") {
      metadata[[field]] <- extract_first(metadata[[field]])
    }
  }
  
  # Normalizar organización si existe / Normalize organization if exists
  if (!is.null(metadata$organization)) {
    for (field in names(metadata$organization)) {
      metadata$organization[[field]] <- extract_first(metadata$organization[[field]])
    }
  }
  
  return(metadata)
}

#' Obtener información de un recurso específico / Get information for specific resource
#' @noRd
get_resource_info <- function(resource_id) {
  params <- list(id = resource_id)
  result <- ckan_request("resource_show", params)
  return(result$result)
}

#' Mapeo de nombres conocidos a IDs de dataset / Mapping of known names to dataset IDs
#' @noRd
KNOWN_DATASETS <- list(
  malaria = "vigilancia-epidemiológica-de-malaria",
  dengue = "vigilancia-epidemiológica-de-dengue",
  leishmaniasis = "vigilancia-epidemiológica-de-leishmaniosis",
  zoonosis = "vigilancia-epidemiológica-de-las-zoonosis",
  ira = "vigilancia-epidemiologica-de-infecciones-respiratoiras-agudas-ira",
  carrion = "vigilancia-epidemiológica-de-enfermedad-de-carrión",
  eda = "vigilancia-epidemiológica-de-enfermedad-diarreica-aguda-eda"
)

#' Resolver nombre de dataset a ID / Resolve dataset name to ID
#' @noRd
resolve_dataset_id <- function(dataset_name) {
  # Si es un nombre conocido / If it's a known name
  if (dataset_name %in% names(KNOWN_DATASETS)) {
    return(KNOWN_DATASETS[[dataset_name]])
  }
  
  # Si ya es un ID completo / If it's already a full ID
  if (grepl("^[a-z0-9-]+$", dataset_name)) {
    return(dataset_name)
  }
  
  # Intentar buscar / Try to search
  search_result <- search_cdc_datasets(rows = 10)
  
  for (dataset in search_result$results) {
    if (grepl(dataset_name, dataset$title, ignore.case = TRUE) ||
        grepl(dataset_name, dataset$name, ignore.case = TRUE)) {
      return(dataset$name)
    }
  }
  
  stop(sprintf("No se encontró el dataset '%s' / Dataset '%s' not found", 
               dataset_name, dataset_name))
}

#' Extraer recursos CSV de un dataset / Extract CSV resources from a dataset
#' @noRd
extract_csv_resources <- function(dataset_metadata) {
  resources <- dataset_metadata$resources
  
  # El portal devuelve valores como arrays, necesitamos extraer el primer elemento
  # The portal returns values as arrays, we need to extract the first element
  csv_resources <- Filter(function(r) {
    format_val <- if(is.list(r$format)) r$format[[1]] else r$format
    url_val <- if(is.list(r$url)) r$url[[1]] else r$url
    
    tolower(format_val) %in% c("csv", "text/csv") || 
      grepl("\\.csv$", url_val, ignore.case = TRUE)
  }, resources)
  
  if (length(csv_resources) == 0) {
    stop("No se encontraron recursos CSV en este dataset / No CSV resources found in this dataset")
  }
  
  # Ordenar por fecha de modificación (más reciente primero) / Sort by modification date (most recent first)
  csv_resources <- csv_resources[order(sapply(csv_resources, function(r) {
    lm <- if(is.list(r$last_modified)) r$last_modified[[1]] else r$last_modified
    lm
  }), decreasing = TRUE)]
  
  return(csv_resources)
}

#' Construir URL de descarga directa / Build direct download URL
#' @noRd
build_download_url <- function(resource) {
  # La URL ya viene completa en el recurso / URL comes complete in resource
  # Manejar caso donde URL es un array / Handle case where URL is an array
  url <- if(is.list(resource$url)) resource$url[[1]] else resource$url
  return(url)
}

#' Verificar disponibilidad del portal / Check portal availability
#' @noRd
check_portal_status <- function() {
  tryCatch({
    result <- ckan_request("site_read", timeout = 5)
    return(TRUE)
  }, error = function(e) {
    futile.logger::flog.warn("Portal de datos abiertos no disponible / Open data portal not available")
    return(FALSE)
  })
}

#' Obtener información resumida de recursos / Get summarized resource information
#' @noRd
summarize_resources <- function(resources) {
  data.frame(
    id = sapply(resources, function(r) r$id),
    nombre = sapply(resources, function(r) r$name),
    formato = sapply(resources, function(r) r$format),
    tamaño = sapply(resources, function(r) format_bytes(as.numeric(r$size))),
    modificado = sapply(resources, function(r) r$last_modified),
    stringsAsFactors = FALSE
  )
}