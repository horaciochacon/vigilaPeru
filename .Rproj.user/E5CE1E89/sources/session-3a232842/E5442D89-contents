#' Funciones auxiliares para manejo de códigos UBIGEO / Helper functions for UBIGEO codes
#'
#' @description
#' Funciones para trabajar con códigos UBIGEO (Ubicación Geográfica) del Perú.
#' Los códigos UBIGEO tienen formato DDPPTT donde:
#' - DD: Departamento (2 dígitos)
#' - PP: Provincia (2 dígitos)
#' - TT: Distrito (2 dígitos)
#'
#' Functions to work with Peru's UBIGEO (Geographic Location) codes.
#' UBIGEO codes have format DDPPTT where:
#' - DD: Department (2 digits)
#' - PP: Province (2 digits)
#' - TT: District (2 digits)

#' Extraer departamento de UBIGEO / Extract department from UBIGEO
#'
#' @param ubigeo Vector de códigos UBIGEO de 6 dígitos / Vector of 6-digit UBIGEO codes
#' @return Vector con códigos de departamento (2 dígitos) / Vector with department codes (2 digits)
#' @export
#' @examples
#' ubigeo_get_department("150101")  # "15" (Lima)
ubigeo_get_department <- function(ubigeo) {
  if (!is.character(ubigeo)) {
    ubigeo <- as.character(ubigeo)
  }
  substr(ubigeo, 1, 2)
}

#' Extraer provincia de UBIGEO / Extract province from UBIGEO
#'
#' @param ubigeo Vector de códigos UBIGEO de 6 dígitos / Vector of 6-digit UBIGEO codes
#' @return Vector con códigos de provincia (4 dígitos: DDPP) / Vector with province codes (4 digits: DDPP)
#' @export
#' @examples
#' ubigeo_get_province("150101")  # "1501" (Lima province)
ubigeo_get_province <- function(ubigeo) {
  if (!is.character(ubigeo)) {
    ubigeo <- as.character(ubigeo)
  }
  substr(ubigeo, 1, 4)
}

#' Validar formato de UBIGEO / Validate UBIGEO format
#'
#' @param ubigeo Vector de códigos UBIGEO / Vector of UBIGEO codes
#' @return Vector lógico indicando si cada código es válido / Logical vector indicating if each code is valid
#' @export
#' @examples
#' ubigeo_is_valid(c("150101", "15", "999999", "ABC"))
ubigeo_is_valid <- function(ubigeo) {
  if (!is.character(ubigeo)) {
    ubigeo <- as.character(ubigeo)
  }
  # Valid UBIGEO: 6 digits, department 01-25
  grepl("^(0[1-9]|1[0-9]|2[0-5])[0-9]{4}$", ubigeo)
}

#' Agregar ceros a la izquierda en UBIGEO / Pad UBIGEO with leading zeros
#'
#' @param ubigeo Vector de códigos UBIGEO que pueden estar sin ceros / Vector of UBIGEO codes that may lack zeros
#' @return Vector con códigos UBIGEO de 6 dígitos / Vector with 6-digit UBIGEO codes
#' @export
#' @examples
#' ubigeo_pad("10101")   # "010101"
#' ubigeo_pad("150101")  # "150101"
ubigeo_pad <- function(ubigeo) {
  if (!is.character(ubigeo)) {
    ubigeo <- as.character(ubigeo)
  }
  # Remove any non-numeric characters
  ubigeo <- gsub("[^0-9]", "", ubigeo)
  # Pad with zeros to 6 digits
  sprintf("%06d", as.numeric(ubigeo))
}

#' Tabla de referencia de departamentos / Department reference table
#'
#' @return data.table con códigos y nombres de departamentos / data.table with department codes and names
#' @export
#' @examples
#' deps <- ubigeo_departments()
ubigeo_departments <- function() {
  data.table::data.table(
    codigo = sprintf("%02d", 1:25),
    departamento = c(
      "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
      "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO",
      "ICA", "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA",
      "LORETO", "MADRE DE DIOS", "MOQUEGUA", "PASCO", "PIURA",
      "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI"
    ),
    capital = c(
      "Chachapoyas", "Huaraz", "Abancay", "Arequipa", "Ayacucho",
      "Cajamarca", "Callao", "Cusco", "Huancavelica", "Huánuco",
      "Ica", "Huancayo", "Trujillo", "Chiclayo", "Lima",
      "Iquitos", "Puerto Maldonado", "Moquegua", "Cerro de Pasco", "Piura",
      "Puno", "Moyobamba", "Tacna", "Tumbes", "Pucallpa"
    ),
    region = c(
      "SELVA", "SIERRA", "SIERRA", "SIERRA", "SIERRA",
      "SIERRA", "COSTA", "SIERRA", "SIERRA", "SIERRA",
      "COSTA", "SIERRA", "COSTA", "COSTA", "COSTA",
      "SELVA", "SELVA", "COSTA", "SIERRA", "COSTA",
      "SIERRA", "SELVA", "COSTA", "COSTA", "SELVA"
    )
  )
}

#' Agregación geográfica usando UBIGEO / Geographic aggregation using UBIGEO
#'
#' @description
#' Agrega datos a nivel de departamento o provincia usando códigos UBIGEO.
#' 
#' Aggregates data at department or province level using UBIGEO codes.
#'
#' @param data data.table o data.frame con columna 'ubigeo'
#' @param level Nivel de agregación: "department" o "province"
#' @param ubigeo_col Nombre de la columna con códigos UBIGEO (default: "ubigeo")
#' @param include_names Lógico. Si TRUE, incluye nombres de departamentos
#'
#' @return data.table con código geográfico agregado
#' @export
#' @examples
#' # Datos de ejemplo / Example data
#' dt <- data.table::data.table(
#'   ubigeo = c("150101", "150102", "130101"),
#'   casos = c(10, 20, 15)
#' )
#' ubigeo_aggregate(dt, level = "department")
ubigeo_aggregate <- function(data, level = c("department", "province"), 
                           ubigeo_col = "ubigeo", include_names = TRUE) {
  
  level <- match.arg(level)
  
  # Convertir a data.table si es necesario
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  } else {
    data <- data.table::copy(data)
  }
  
  # Verificar que existe la columna ubigeo
  if (!ubigeo_col %in% names(data)) {
    stop(sprintf("Columna '%s' no encontrada en los datos / Column '%s' not found in data", 
                 ubigeo_col, ubigeo_col))
  }
  
  # Crear columna de agregación
  if (level == "department") {
    data[, geo_code := ubigeo_get_department(get(ubigeo_col))]
    
    if (include_names) {
      deps <- ubigeo_departments()
      data <- data[deps, departamento := i.departamento, on = .(geo_code = codigo)]
    }
  } else {  # province
    data[, geo_code := ubigeo_get_province(get(ubigeo_col))]
    
    if (include_names) {
      # For provinces, we'd need a more complete lookup table
      # For now, just include department name
      data[, dep_code := substr(geo_code, 1, 2)]
      deps <- ubigeo_departments()
      data <- data[deps, departamento := i.departamento, on = .(dep_code = codigo)]
      data[, dep_code := NULL]
    }
  }
  
  return(data)
}