#' Listar datasets de vigilancia epidemiológica / List epidemiological surveillance datasets
#'
#' @description
#' Devuelve una tabla con todos los datasets de vigilancia epidemiológica disponibles
#' en el portal de datos abiertos del Perú.
#' 
#' Returns a table with all epidemiological surveillance datasets available
#' on Peru's open data portal.
#'
#' @param as_tibble Lógico. Si TRUE (predeterminado), devuelve un tibble. Si FALSE, devuelve data.table.
#'   / Logical. If TRUE (default), returns a tibble. If FALSE, returns data.table.
#' @param refresh Lógico. Si TRUE, actualiza la información desde el portal.
#'   / Logical. If TRUE, refreshes information from the portal.
#'
#' @return data.table o tibble con información de datasets / data.table or tibble with dataset information
#' @export
#'
#' @examples
#' # Listar todos los datasets / List all datasets
#' datasets <- vp_datasets()
#' 
#' # Obtener como data.table / Get as data.table
#' datasets_dt <- vp_datasets(as_tibble = FALSE)
vp_datasets <- function(as_tibble = TRUE, refresh = FALSE) {
  setup_logging()
  
  cache_path <- get_cache_path("datasets_list", extension = "rds")
  
  # Verificar caché / Check cache
  if (!refresh && file.exists(cache_path)) {
    futile.logger::flog.info("Usando lista de datasets en caché / Using cached dataset list")
    datasets <- readRDS(cache_path)
  } else {
    futile.logger::flog.info("Obteniendo lista de datasets / Getting dataset list")
    
    # Como package_search no funciona, usamos la lista conocida y obtenemos metadatos
    # Since package_search doesn't work, use known list and get metadata
    datasets_info <- lapply(names(KNOWN_DATASETS), function(name) {
      tryCatch({
        dataset_id <- KNOWN_DATASETS[[name]]
        metadata <- get_dataset_metadata(dataset_id)
        
        # Extraer información básica / Extract basic information
        list(
          nombre = name,
          dataset_id = dataset_id,
          titulo = metadata$title,
          organizacion = if(!is.null(metadata$organization) && !is.null(metadata$organization$title)) {
            metadata$organization$title
          } else {
            "Centro Nacional de Epidemiología, Prevención y Control de Enfermedades"
          },
          creado = metadata$metadata_created,
          modificado = metadata$metadata_modified,
          recursos = length(metadata$resources),
          licencia = metadata$license_title
        )
      }, error = function(e) {
        futile.logger::flog.warn(sprintf("Error obteniendo info de %s: %s", name, e$message))
        NULL
      })
    })
    
    # Filtrar NULLs y convertir a data.table / Filter NULLs and convert to data.table
    datasets_info <- Filter(Negate(is.null), datasets_info)
    
    if (length(datasets_info) == 0) {
      stop("No se pudo obtener información de ningún dataset / Could not get information from any dataset")
    }
    
    datasets <- data.table::rbindlist(datasets_info)
    
    # Guardar en caché / Save to cache
    saveRDS(datasets, cache_path)
    futile.logger::flog.info(sprintf("Lista de datasets guardada en caché / Dataset list saved to cache"))
  }
  
  # Convertir a tibble si se solicita / Convert to tibble if requested
  return(maybe_as_tibble(datasets, as_tibble))
}

#' Información detallada de datasets / Detailed dataset information
#'
#' @description
#' Muestra información detallada de los datasets disponibles, incluyendo
#' descripción de variables y recursos.
#' 
#' Shows detailed information about available datasets, including
#' variable descriptions and resources.
#'
#' @param dataset Nombre del dataset ("malaria", "dengue", etc.) o NULL para todos.
#'   / Dataset name ("malaria", "dengue", etc.) or NULL for all.
#'
#' @return Lista con información detallada / List with detailed information
#' @export
#'
#' @examples
#' # Información de malaria / Malaria information
#' vp_dataset_info("malaria")
#' 
#' # Información de todos los datasets / All datasets information
#' vp_dataset_info()
vp_dataset_info <- function(dataset = NULL) {
  if (!is.null(dataset)) {
    validate_dataset_id(dataset)
    
    if (!(dataset %in% names(KNOWN_DATASETS))) {
      stop(sprintf("Dataset '%s' no reconocido. Opciones: %s / Dataset '%s' not recognized. Options: %s",
                   dataset, paste(names(KNOWN_DATASETS), collapse = ", "),
                   dataset, paste(names(KNOWN_DATASETS), collapse = ", ")))
    }
    
    dataset_id <- resolve_dataset_id(dataset)
    metadata <- get_dataset_metadata(dataset_id)
    
    # Procesar recursos / Process resources
    resources_info <- NULL
    if (length(metadata$resources) > 0) {
      resources_info <- lapply(metadata$resources, function(r) {
        list(
          nombre = if(is.list(r$name)) r$name[[1]] else r$name,
          formato = if(is.list(r$format)) r$format[[1]] else r$format,
          tamaño = if(is.list(r$size)) r$size[[1]] else r$size,
          url = if(is.list(r$url)) r$url[[1]] else r$url,
          modificado = if(is.list(r$last_modified)) r$last_modified[[1]] else r$last_modified
        )
      })
    }
    
    return(list(
      dataset = dataset,
      titulo = metadata$title,
      descripcion = metadata$notes,
      organizacion = if(!is.null(metadata$organization) && !is.null(metadata$organization$title)) {
        metadata$organization$title
      } else {
        "Centro Nacional de Epidemiología, Prevención y Control de Enfermedades"
      },
      licencia = metadata$license_title,
      creado = metadata$metadata_created,
      modificado = metadata$metadata_modified,
      recursos = resources_info
    ))
  } else {
    # Información de todos los datasets / Information for all datasets
    all_info <- lapply(names(KNOWN_DATASETS), function(ds_name) {
      tryCatch({
        vp_dataset_info(ds_name)
      }, error = function(e) {
        futile.logger::flog.warn(sprintf("Error obteniendo info de %s: %s", ds_name, e$message))
        NULL
      })
    })
    # Filtrar NULLs / Filter NULLs
    all_info <- Filter(Negate(is.null), all_info)
    names(all_info) <- sapply(all_info, function(x) x$dataset)
    return(all_info)
  }
}