#' Gestión del directorio de caché / Cache directory management
#'
#' @description
#' `vp_cache_dir()` devuelve o establece la ruta del directorio de caché para vigilaPeru.
#' 
#' `vp_cache_dir()` returns or sets the cache directory path for vigilaPeru.
#'
#' @param path Ruta al nuevo directorio de caché. Si es NULL, devuelve la ruta actual.
#'   / Path to new cache directory. If NULL, returns current path.
#'
#' @return Ruta del directorio de caché (character) / Cache directory path (character)
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
      stop("path debe ser una cadena de caracteres única / path must be a single character string")
    }
    
    options(vigilaPeru.cache_dir = path)
    
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      futile.logger::flog.info(paste("Directorio de caché creado en / Cache directory created at:", path))
    }
    
    return(invisible(path))
  }
  
  cache_dir <- getOption("vigilaPeru.cache_dir")
  
  if (is.null(cache_dir)) {
    cache_dir <- rappdirs::user_cache_dir("vigilaPeru")
    options(vigilaPeru.cache_dir = cache_dir)
    
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      futile.logger::flog.info(paste("Directorio de caché predeterminado creado en / Default cache directory created at:", cache_dir))
    }
  }
  
  return(cache_dir)
}

#' Limpiar caché / Clear cache
#'
#' @description
#' Elimina archivos del caché de vigilaPeru. Puede eliminar todo el caché o solo archivos específicos.
#' 
#' Removes files from vigilaPeru cache. Can remove all cache or specific files.
#'
#' @param dataset Nombre del dataset a limpiar. Si es NULL, limpia todo el caché.
#'   / Dataset name to clear. If NULL, clears all cache.
#' @param older_than Eliminar archivos más antiguos que este número de días.
#'   / Remove files older than this number of days.
#'
#' @return NULL (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' # Limpiar todo el caché / Clear all cache
#' vp_cache_clear()
#' 
#' # Limpiar caché de malaria / Clear malaria cache
#' vp_cache_clear("malaria")
#' 
#' # Limpiar archivos de más de 30 días / Clear files older than 30 days
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
      futile.logger::flog.info(sprintf("Eliminados %d archivos del caché / Removed %d files from cache", length(files)))
    } else {
      futile.logger::flog.info("Caché ya está vacío / Cache already empty")
    }
  } else if (!is.null(dataset)) {
    # Limpiar dataset específico / Clear specific dataset
    pattern <- paste0("^", dataset, "_")
    files <- list.files(cache_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
    if (length(files) > 0) {
      unlink(files)
      futile.logger::flog.info(sprintf("Eliminados %d archivos de caché para '%s' / Removed %d cache files for '%s'", 
                                       length(files), dataset, length(files), dataset))
    } else {
      futile.logger::flog.info(sprintf("No se encontraron archivos de caché para '%s' / No cache files found for '%s'", 
                                       dataset, dataset))
    }
  } else if (!is.null(older_than)) {
    # Limpiar archivos antiguos / Clear old files
    if (!is.numeric(older_than) || older_than < 0) {
      stop("'older_than' debe ser un número positivo de días / 'older_than' must be a positive number of days")
    }
    
    files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
    if (length(files) > 0) {
      file_info <- file.info(files)
      old_files <- files[difftime(Sys.time(), file_info$mtime, units = "days") > older_than]
      
      if (length(old_files) > 0) {
        unlink(old_files)
        futile.logger::flog.info(sprintf("Eliminados %d archivos de más de %d días / Removed %d files older than %d days", 
                                         length(old_files), older_than, length(old_files), older_than))
      } else {
        futile.logger::flog.info(sprintf("No se encontraron archivos de más de %d días / No files older than %d days found", 
                                         older_than, older_than))
      }
    }
  }
  
  invisible(NULL)
}

#' Información del caché / Cache information
#'
#' @description
#' Muestra información sobre el uso del caché de vigilaPeru.
#' 
#' Shows information about vigilaPeru cache usage.
#'
#' @return data.frame con información del caché / data.frame with cache information
#' @export
#'
#' @examples
#' vp_cache_info()
vp_cache_info <- function() {
  cache_dir <- vp_cache_dir()
  
  if (!dir.exists(cache_dir)) {
    return(data.frame(
      directorio = cache_dir,
      existe = FALSE,
      archivos = 0,
      tamaño_mb = 0,
      stringsAsFactors = FALSE
    ))
  }
  
  files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
  
  if (length(files) == 0) {
    return(data.frame(
      directorio = cache_dir,
      existe = TRUE,
      archivos = 0,
      tamaño_mb = 0,
      stringsAsFactors = FALSE
    ))
  }
  
  file_info <- file.info(files)
  total_size <- sum(file_info$size, na.rm = TRUE) / 1024 / 1024  # Convert to MB
  
  # Agrupar por dataset / Group by dataset
  dataset_names <- gsub("_.*", "", basename(files))
  dataset_summary <- aggregate(file_info$size, 
                              by = list(dataset = dataset_names), 
                              FUN = function(x) sum(x, na.rm = TRUE) / 1024 / 1024)
  names(dataset_summary)[2] <- "tamaño_mb"
  dataset_summary$archivos <- table(dataset_names)[dataset_summary$dataset]
  
  list(
    resumen = data.frame(
      directorio = cache_dir,
      existe = TRUE,
      archivos_total = length(files),
      tamaño_total_mb = round(total_size, 2),
      stringsAsFactors = FALSE
    ),
    por_dataset = dataset_summary[order(dataset_summary$tamaño_mb, decreasing = TRUE), ]
  )
}