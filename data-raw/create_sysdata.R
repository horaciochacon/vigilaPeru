# Script to create internal dataset metadata constants
# This data will be saved in R/sysdata.rda and available internally to package functions

# Extended dataset metadata with bilingual information
DATASET_METADATA <- list(
  malaria = list(
    id = "vigilancia-epidemiológica-de-malaria",
    name = "Malaria",
    name_es = "Malaria",
    description = "Weekly epidemiological surveillance data for Malaria cases in Peru",
    description_es = "Datos de vigilancia epidemiológica semanal de casos de Malaria en Perú",
    keywords = c("malaria", "plasmodium", "B50", "B51", "B52", "B53", "B54"),
    typical_columns = c("enfermedad", "ano", "semana", "diagnostic", "tipo_dx", "ubigeo", "departamento", "provincia", "distrito", "edad", "sexo"),
    update_frequency = "weekly",
    start_year = 2000
  ),
  dengue = list(
    id = "vigilancia-epidemiológica-de-dengue", 
    name = "Dengue",
    name_es = "Dengue",
    description = "Weekly epidemiological surveillance data for Dengue cases in Peru",
    description_es = "Datos de vigilancia epidemiológica semanal de casos de Dengue en Perú",
    keywords = c("dengue", "aedes", "A90", "A91"),
    typical_columns = c("enfermedad", "ano", "semana", "diagnostic", "tipo_dx", "ubigeo", "departamento", "provincia", "distrito", "edad", "sexo"),
    update_frequency = "weekly",
    start_year = 2000
  ),
  leishmaniasis = list(
    id = "vigilancia-epidemiológica-de-leishmaniosis",
    name = "Leishmaniasis", 
    name_es = "Leishmaniasis",
    description = "Weekly epidemiological surveillance data for Leishmaniasis cases in Peru",
    description_es = "Datos de vigilancia epidemiológica semanal de casos de Leishmaniasis en Perú", 
    keywords = c("leishmaniasis", "B55", "kala-azar", "uta"),
    typical_columns = c("enfermedad", "ano", "semana", "diagnostic", "tipo_dx", "ubigeo", "departamento", "provincia", "distrito", "edad", "sexo"),
    update_frequency = "weekly",
    start_year = 2000
  ),
  zoonosis = list(
    id = "vigilancia-epidemiológica-de-las-zoonosis",
    name = "Zoonosis",
    name_es = "Zoonosis", 
    description = "Weekly epidemiological surveillance data for Zoonotic diseases in Peru",
    description_es = "Datos de vigilancia epidemiológica semanal de enfermedades Zoonóticas en Perú",
    keywords = c("zoonosis", "rabies", "leptospirosis", "brucellosis", "plague"),
    typical_columns = c("enfermedad", "ano", "semana", "diagnostic", "tipo_dx", "ubigeo", "departamento", "provincia", "distrito", "edad", "sexo"),
    update_frequency = "weekly",
    start_year = 2000
  ),
  ira = list(
    id = "vigilancia-epidemiologica-de-infecciones-respiratoiras-agudas-ira",
    name = "Acute Respiratory Infections (ARI)",
    name_es = "Infecciones Respiratorias Agudas (IRA)",
    description = "Weekly epidemiological surveillance data for Acute Respiratory Infections in Peru",
    description_es = "Datos de vigilancia epidemiológica semanal de Infecciones Respiratorias Agudas en Perú",
    keywords = c("ira", "ari", "respiratory", "pneumonia", "bronchitis"),
    typical_columns = c("enfermedad", "ano", "semana", "diagnostic", "tipo_dx", "ubigeo", "departamento", "provincia", "distrito", "edad", "sexo"),
    update_frequency = "weekly",
    start_year = 2000
  ),
  carrion = list(
    id = "vigilancia-epidemiológica-de-enfermedad-de-carrión",
    name = "Carrion's Disease",
    name_es = "Enfermedad de Carrión",
    description = "Weekly epidemiological surveillance data for Carrion's Disease (Bartonellosis) in Peru",
    description_es = "Datos de vigilancia epidemiológica semanal de Enfermedad de Carrión (Bartonelosis) en Perú",
    keywords = c("carrion", "bartonella", "oroya fever", "verruga peruana", "A44"),
    typical_columns = c("enfermedad", "ano", "semana", "diagnostic", "tipo_dx", "ubigeo", "departamento", "provincia", "distrito", "edad", "sexo"),
    update_frequency = "weekly",
    start_year = 2000
  ),
  eda = list(
    id = "vigilancia-epidemiológica-de-enfermedad-diarreica-aguda-eda",
    name = "Acute Diarrheal Disease (ADD)", 
    name_es = "Enfermedad Diarreica Aguda (EDA)",
    description = "Weekly epidemiological surveillance data for Acute Diarrheal Disease in Peru",
    description_es = "Datos de vigilancia epidemiológica semanal de Enfermedad Diarreica Aguda en Perú",
    keywords = c("eda", "add", "diarrhea", "cholera", "A00", "A09"),
    typical_columns = c("enfermedad", "ano", "semana", "diagnostic", "tipo_dx", "ubigeo", "departamento", "provincia", "distrito", "edad", "sexo"),
    update_frequency = "weekly",
    start_year = 2000
  )
)

# Column metadata with bilingual labels
COLUMN_METADATA <- list(
  enfermedad = list(
    type = "character",
    label = "Disease",
    label_es = "Enfermedad",
    description = "Disease name",
    description_es = "Nombre de la enfermedad"
  ),
  ano = list(
    type = "integer",
    label = "Year",
    label_es = "Año",
    description = "Epidemiological year",
    description_es = "Año epidemiológico"
  ),
  semana = list(
    type = "integer",
    label = "Week",
    label_es = "Semana",
    description = "Epidemiological week (1-52/53)",
    description_es = "Semana epidemiológica (1-52/53)"
  ),
  diagnostic = list(
    type = "character", 
    label = "Diagnosis Code",
    label_es = "Código Diagnóstico",
    description = "ICD-10 diagnosis code",
    description_es = "Código de diagnóstico CIE-10"
  ),
  tipo_dx = list(
    type = "character",
    label = "Diagnosis Type",
    label_es = "Tipo de Diagnóstico",
    description = "Type of diagnosis (C=Confirmed, P=Probable, S=Suspected)",
    description_es = "Tipo de diagnóstico (C=Confirmado, P=Probable, S=Sospechoso)"
  ),
  ubigeo = list(
    type = "character",
    label = "UBIGEO Code",
    label_es = "Código UBIGEO",
    description = "6-digit geographic code (DDPPTT format)",
    description_es = "Código geográfico de 6 dígitos (formato DDPPTT)"
  ),
  departamento = list(
    type = "character",
    label = "Department",
    label_es = "Departamento",
    description = "Department/Region name",
    description_es = "Nombre del departamento/región"
  ),
  provincia = list(
    type = "character",
    label = "Province",
    label_es = "Provincia",
    description = "Province name",
    description_es = "Nombre de la provincia"
  ),
  distrito = list(
    type = "character",
    label = "District",
    label_es = "Distrito",
    description = "District name",
    description_es = "Nombre del distrito"
  ),
  edad = list(
    type = "integer",
    label = "Age",
    label_es = "Edad",
    description = "Age in years",
    description_es = "Edad en años"
  ),
  sexo = list(
    type = "character",
    label = "Sex",
    label_es = "Sexo",
    description = "Sex (M=Male, F=Female)",
    description_es = "Sexo (M=Masculino, F=Femenino)"
  )
)

# Save to R/sysdata.rda
usethis::use_data(DATASET_METADATA, COLUMN_METADATA, internal = TRUE, overwrite = TRUE)

message("Internal data saved to R/sysdata.rda")