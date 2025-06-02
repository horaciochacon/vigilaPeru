#' Descargar y leer datasets de vigilancia epidemiológica / Download and read epidemiological surveillance datasets
#'
#' @description
#' Descarga datasets del portal de datos abiertos y los carga como data.table.
#' Utiliza caché local para mejorar el rendimiento y permitir trabajo offline.
#' 
#' Downloads datasets from the open data portal and loads them as data.table.
#' Uses local cache to improve performance and enable offline work.
#'
#' @param dataset Nombre del dataset ("malaria", "dengue", etc.)
#'   / Dataset name ("malaria", "dengue", etc.)
#' @param resource_index Índice del recurso a descargar (NULL para el más reciente).
#'   / Resource index to download (NULL for most recent).
#' @param refresh Lógico. Si TRUE, fuerza nueva descarga incluso si existe caché.
#'   / Logical. If TRUE, forces new download even if cache exists.
#' @param as_tibble Lógico. Si TRUE (predeterminado), devuelve un tibble. Si FALSE, devuelve data.table.
#'   / Logical. If TRUE (default), returns a tibble. If FALSE, returns data.table.
#' @param encoding Codificación del archivo CSV (por defecto "UTF-8").
#'   / CSV file encoding (default "UTF-8").
#' @param progress Lógico. Si TRUE, muestra barra de progreso durante la descarga.
#'   / Logical. If TRUE, shows progress bar during download.
#' @param max_rows Número máximo de filas a leer (NULL para todas).
#'   / Maximum number of rows to read (NULL for all).
#'
#' @return data.table o tibble con los datos del dataset / data.table or tibble with dataset data
#' @export
#'
#' @examples
#' # Descargar datos de malaria más recientes / Download most recent malaria data
#' mal <- vp_download("malaria")
#' 
#' # Descargar recurso específico / Download specific resource
#' mal_2008 <- vp_download("malaria", resource_index = 1)
#' 
#' # Forzar actualización / Force update
#' mal_fresh <- vp_download("malaria", refresh = TRUE)
#' 
#' # Obtener como data.table / Get as data.table
#' mal_dt <- vp_download("malaria", as_tibble = FALSE)
vp_download <- function(dataset, 
                       resource_index = NULL, 
                       refresh = FALSE,
                       as_tibble = TRUE,
                       encoding = "UTF-8",
                       progress = TRUE,
                       max_rows = NULL) {
  
  validate_dataset_id(dataset)
  setup_logging()
  
  # Resolver nombre a ID / Resolve name to ID
  dataset_id <- resolve_dataset_id(dataset)
  
  futile.logger::flog.info(sprintf("Iniciando descarga de %s / Starting download of %s", 
                                   dataset, dataset))
  
  # Obtener metadatos del dataset / Get dataset metadata
  metadata <- get_dataset_metadata(dataset_id)
  
  # Extraer recursos CSV / Extract CSV resources
  csv_resources <- extract_csv_resources(metadata)
  
  if (length(csv_resources) == 0) {
    stop(sprintf("No se encontraron recursos CSV para %s / No CSV resources found for %s", 
                 dataset, dataset))
  }
  
  # Seleccionar recurso / Select resource
  if (is.null(resource_index)) {
    # Usar el más reciente (ya están ordenados) / Use most recent (already sorted)
    resource <- csv_resources[[1]]
    resource_index <- 1
  } else {
    if (resource_index < 1 || resource_index > length(csv_resources)) {
      stop(sprintf("Índice de recurso inválido. Disponibles: 1-%d / Invalid resource index. Available: 1-%d", 
                   length(csv_resources), length(csv_resources)))
    }
    resource <- csv_resources[[resource_index]]
  }
  
  # Información del recurso / Resource information
  resource_id <- if(is.list(resource$id)) resource$id[[1]] else resource$id
  resource_name <- if(is.list(resource$name)) resource$name[[1]] else resource$name
  resource_size <- if(is.list(resource$size)) resource$size[[1]] else resource$size
  resource_url <- build_download_url(resource)
  
  futile.logger::flog.info(sprintf("Recurso seleccionado: %s (%s) / Selected resource: %s (%s)", 
                                   resource_name, resource_size, resource_name, resource_size))
  
  # Rutas de caché / Cache paths
  cache_csv_path <- get_cache_path(dataset, resource_id, "csv")
  cache_rds_path <- get_cache_path(dataset, resource_id, "rds")
  
  # Verificar si necesita actualización / Check if needs update
  needs_download <- refresh || !file.exists(cache_rds_path)
  
  if (!needs_download && !is.null(resource$last_modified)) {
    # Verificar si el recurso remoto es más nuevo / Check if remote resource is newer
    remote_modified <- if(is.list(resource$last_modified)) resource$last_modified[[1]] else resource$last_modified
    needs_download <- cache_needs_update(cache_rds_path, remote_modified)
  }
  
  if (!needs_download) {
    futile.logger::flog.info("Usando datos en caché / Using cached data")
    dt <- readRDS(cache_rds_path)
    return(maybe_as_tibble(dt, as_tibble))
  }
  
  # Descargar archivo / Download file
  futile.logger::flog.info(sprintf("Descargando %s / Downloading %s", resource_url))
  
  tryCatch({
    # Verificar conexión / Check connection
    if (!check_internet()) {
      stop("Sin conexión a internet / No internet connection")
    }
    
    # Crear request con configuración / Create request with configuration
    req <- httr2::request(resource_url) |>
      httr2::req_timeout(600) |>  # 10 minutos para archivos grandes / 10 minutes for large files
      httr2::req_retry(max_tries = 3, backoff = ~ 2) |>
      httr2::req_user_agent("vigilaPeru R package")
    
    if (progress) {
      req <- req |> httr2::req_progress()
    }
    
    # Descargar a archivo temporal / Download to temp file
    temp_file <- tempfile(fileext = ".csv")
    resp <- req |> httr2::req_perform(path = temp_file)
    
    futile.logger::flog.info("Descarga completada. Leyendo datos... / Download completed. Reading data...")
    
    # Leer CSV con data.table / Read CSV with data.table
    dt <- data.table::fread(
      temp_file,
      encoding = encoding,
      showProgress = progress,
      nrows = if(!is.null(max_rows)) max_rows else -1L,
      stringsAsFactors = FALSE,
      data.table = TRUE
    )
    
    # Limpiar nombres de columnas / Clean column names
    names(dt) <- tolower(gsub("[^a-z0-9_]", "_", names(dt)))
    
    # Guardar en caché / Save to cache
    saveRDS(dt, cache_rds_path)
    
    # También guardar CSV si es pequeño / Also save CSV if small
    file_size <- file.info(temp_file)$size
    if (file_size < 100 * 1024 * 1024) {  # < 100MB
      file.copy(temp_file, cache_csv_path, overwrite = TRUE)
    }
    
    # Limpiar archivo temporal / Clean temp file
    unlink(temp_file)
    
    futile.logger::flog.info(sprintf("Datos guardados en caché: %d filas, %d columnas / Data cached: %d rows, %d columns", 
                                     nrow(dt), ncol(dt)))
    
    # Información adicional / Additional information
    if (progress) {
      message(sprintf("\n✓ Dataset '%s' descargado exitosamente / Dataset '%s' downloaded successfully", 
                      dataset, dataset))
      message(sprintf("  Filas/Rows: %s | Columnas/Columns: %d", 
                      format(nrow(dt), big.mark = ","), ncol(dt)))
      message(sprintf("  Columnas/Columns: %s", paste(names(dt), collapse = ", ")))
    }
    
    return(maybe_as_tibble(dt, as_tibble))
    
  }, error = function(e) {
    # Intentar usar caché antiguo si existe / Try to use old cache if exists
    if (file.exists(cache_rds_path)) {
      futile.logger::flog.warn(sprintf("Error en descarga: %s. Usando caché antiguo / Download error: %s. Using old cache", 
                                       e$message))
      dt <- readRDS(cache_rds_path)
      return(maybe_as_tibble(dt, as_tibble))
    } else {
      stop(sprintf("Error descargando datos: %s / Error downloading data: %s", e$message, e$message))
    }
  })
}

#' Listar recursos disponibles para un dataset / List available resources for a dataset
#'
#' @description
#' Muestra todos los recursos CSV disponibles para un dataset específico.
#' 
#' Shows all available CSV resources for a specific dataset.
#'
#' @param dataset Nombre del dataset ("malaria", "dengue", etc.)
#'   / Dataset name ("malaria", "dengue", etc.)
#'
#' @return data.frame con información de recursos / data.frame with resource information
#' @export
#'
#' @examples
#' # Ver recursos de malaria / View malaria resources
#' vp_list_resources("malaria")
vp_list_resources <- function(dataset) {
  validate_dataset_id(dataset)
  
  # Resolver nombre a ID / Resolve name to ID
  dataset_id <- resolve_dataset_id(dataset)
  
  # Obtener metadatos / Get metadata
  metadata <- get_dataset_metadata(dataset_id)
  
  # Extraer recursos CSV / Extract CSV resources
  csv_resources <- extract_csv_resources(metadata)
  
  if (length(csv_resources) == 0) {
    message(sprintf("No se encontraron recursos CSV para %s / No CSV resources found for %s", 
                    dataset, dataset))
    return(data.frame())
  }
  
  # Crear tabla de información / Create information table
  resources_df <- data.frame(
    index = seq_along(csv_resources),
    nombre = sapply(csv_resources, function(r) {
      if(is.list(r$name)) r$name[[1]] else r$name
    }),
    formato = sapply(csv_resources, function(r) {
      if(is.list(r$format)) r$format[[1]] else r$format
    }),
    tamaño = sapply(csv_resources, function(r) {
      if(is.list(r$size)) r$size[[1]] else r$size
    }),
    modificado = sapply(csv_resources, function(r) {
      if(is.list(r$last_modified)) r$last_modified[[1]] else r$last_modified
    }),
    stringsAsFactors = FALSE
  )
  
  return(resources_df)
}