#' Gestion del directorio de cache / Cache directory management
#'
#' @description
#' `vp_cache_dir()` devuelve o establece la ruta del directorio de cache para vigilaPeru.
#' 
#' `vp_cache_dir()` returns or sets the cache directory path for vigilaPeru.
#'
#' @importFrom stats aggregate
#'
#' @param path Ruta al nuevo directorio de cache. Si es NULL, devuelve la ruta actual.
#'   / Path to new cache directory. If NULL, returns current path.
#'
#' @return Ruta del directorio de cache (character) / Cache directory path (character)
#' @export
#'
#' @examples
#' # Obtener directorio actual / Get current directory
#' vp_cache_dir()
#' 
#' # Establecer nuevo directorio / Set new directory
#' \dontrun{
#' vp_cache_dir("/mi/nueva/ruta")
#' }
vp_cache_dir <- function(path = NULL) {
  if (!is.null(path)) {
    if (!is.character(path) || length(path) != 1) {
      stop("path debe ser una cadena de caracteres unica / path must be a single character string")
    }
    
    options(vigilaPeru.cache_dir = path)
    
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      futile.logger::flog.info(paste("Directorio de cache creado en / Cache directory created at:", path))
    }
    
    return(invisible(path))
  }
  
  cache_dir <- getOption("vigilaPeru.cache_dir")
  
  if (is.null(cache_dir)) {
    cache_dir <- rappdirs::user_cache_dir("vigilaPeru")
    options(vigilaPeru.cache_dir = cache_dir)
    
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      futile.logger::flog.info(paste("Directorio de cache predeterminado creado en / Default cache directory created at:", cache_dir))
    }
  }
  
  return(cache_dir)
}

#' Limpiar cache / Clear cache
#'
#' @description
#' Elimina archivos del cache de vigilaPeru. Puede eliminar todo el cache o solo archivos especificos.
#' 
#' Removes files from vigilaPeru cache. Can remove all cache or specific files.
#'
#' @param dataset Nombre del dataset a limpiar. Si es NULL, limpia todo el cache.
#'   / Dataset name to clear. If NULL, clears all cache.
#' @param older_than Eliminar archivos mas antiguos que este numero de dias.
#'   / Remove files older than this number of days.
#'
#' @return NULL (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' # Limpiar todo el cache / Clear all cache
#' vp_cache_clear()
#' 
#' # Limpiar cache de malaria / Clear malaria cache
#' vp_cache_clear("malaria")
#' 
#' # Limpiar archivos de mas de 30 dias / Clear files older than 30 days
#' vp_cache_clear(older_than = 30)
#' }
vp_cache_clear <- function(dataset = NULL, older_than = NULL) {
  cache_dir <- vp_cache_dir()
  
  if (!is.null(dataset) && !is.null(older_than)) {
    stop("Especifique solo 'dataset' o 'older_than', no ambos / Specify either 'dataset' or 'older_than', not both")
  }
  
  if (is.null(dataset) && is.null(older_than)) {
    # Limpiar todo / Clear everything
    files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
    if (length(files) > 0) {
      unlink(files)
      futile.logger::flog.info(sprintf("Eliminados %d archivos del cache / Removed %d files from cache", length(files), length(files)))
    } else {
      futile.logger::flog.info("Cache ya esta vacio / Cache already empty")
    }
  } else if (!is.null(dataset)) {
    # Limpiar dataset especifico / Clear specific dataset
    pattern <- paste0("^", dataset, "_")
    files <- list.files(cache_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
    if (length(files) > 0) {
      unlink(files)
      futile.logger::flog.info(sprintf("Eliminados %d archivos de cache para '%s' / Removed %d cache files for '%s'", 
                                       length(files), dataset, length(files), dataset))
    } else {
      futile.logger::flog.info(sprintf("No se encontraron archivos de cache para '%s' / No cache files found for '%s'", 
                                       dataset, dataset))
    }
  } else if (!is.null(older_than)) {
    # Limpiar archivos antiguos / Clear old files
    if (!is.numeric(older_than) || older_than < 0) {
      stop("'older_than' debe ser un numero positivo de dias / 'older_than' must be a positive number of days")
    }
    
    files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
    if (length(files) > 0) {
      file_info <- file.info(files)
      old_files <- files[difftime(Sys.time(), file_info$mtime, units = "days") > older_than]
      
      if (length(old_files) > 0) {
        unlink(old_files)
        futile.logger::flog.info(sprintf("Eliminados %d archivos de mas de %d dias / Removed %d files older than %d days", 
                                         length(old_files), older_than, length(old_files), older_than))
      } else {
        futile.logger::flog.info(sprintf("No se encontraron archivos de mas de %d dias / No files older than %d days found", 
                                         older_than, older_than))
      }
    }
  }
  
  invisible(NULL)
}

#' Informacion del cache / Cache information
#'
#' @description
#' Muestra informacion sobre el uso del cache de vigilaPeru.
#' 
#' Shows information about vigilaPeru cache usage.
#'
#' @return data.frame con informacion del cache / data.frame with cache information
#' @export
#'
#' @examples
#' vp_cache_info()
vp_cache_info <- function() {
  cache_dir <- vp_cache_dir()
  
  if (!dir.exists(cache_dir)) {
    return(list(
      resumen = data.frame(
        directorio = cache_dir,
        existe = FALSE,
        archivos_total = 0,
        tamano_total_mb = 0,
        stringsAsFactors = FALSE
      ),
      por_dataset = data.frame(
        dataset = character(0),
        tamano_mb = numeric(0),
        archivos = integer(0),
        stringsAsFactors = FALSE
      )
    ))
  }
  
  files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
  
  if (length(files) == 0) {
    return(list(
      resumen = data.frame(
        directorio = cache_dir,
        existe = TRUE,
        archivos_total = 0,
        tamano_total_mb = 0,
        stringsAsFactors = FALSE
      ),
      por_dataset = data.frame(
        dataset = character(0),
        tamano_mb = numeric(0),
        archivos = integer(0),
        stringsAsFactors = FALSE
      )
    ))
  }
  
  file_info <- file.info(files)
  total_size <- sum(file_info$size, na.rm = TRUE) / 1024 / 1024  # Convert to MB
  
  # Agrupar por dataset / Group by dataset
  dataset_names <- gsub("_.*", "", basename(files))
  dataset_summary <- aggregate(file_info$size, 
                              by = list(dataset = dataset_names), 
                              FUN = function(x) sum(x, na.rm = TRUE) / 1024 / 1024)
  names(dataset_summary)[2] <- "tamano_mb"
  dataset_summary$archivos <- table(dataset_names)[dataset_summary$dataset]
  
  list(
    resumen = data.frame(
      directorio = cache_dir,
      existe = TRUE,
      archivos_total = length(files),
      tamano_total_mb = round(total_size, 2),
      stringsAsFactors = FALSE
    ),
    por_dataset = dataset_summary[order(dataset_summary$tamano_mb, decreasing = TRUE), ]
  )
}