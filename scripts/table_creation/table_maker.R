#'Este script contiene la funcion que lee y crea las tablas limpias para el informe. Primero crea las
#'tablas de CRM, luego las de Tenencia y por ultimo la de creditos 
#'@param yearmonth, en este se debe espeficicar el periodo en que se debe generar el reporte, en formato yyyymm
#'@param original_path contiene la ruta de los datos
#'@param staging_path contiene la ruta donde quedan guardadas las nuevas tablas
#'@usage table_maker(yearmonth) 


table_maker <- function(yearmonth) {
  
  ##################################################################################
  ## crm ##
  ##################################################################################
  
  #Leer archivos formato csv y extraer caracteres numéricos
  crm_path <- paste0(external_path, "/CRM_Persona_Natural")
  files <- list.files(path = crm_path, pattern = ".csv")
  position <-
    paste0(sapply(str_extract_all(files, "[0-9]+"), "[[", 1),
           sapply(str_extract_all(files, "[0-9]+"), "[[", 2))  %>% as.numeric
  
  # comparing cut months from user to master files created
  if (FALSE %in% (yearmonth %in% position)) {
    stop("Does not exist table for some of the months specified")
  }
  
  df_position <-
    data.frame(files = files, position = position)
  
  if (staging_path == "data/staging") {
    inactivos <- c(grep("INACTIVOS", df_position$files))
    index(df_position)
    df_position <- df_position[-inactivos, ]
  }
  
  files <- df_position[df_position$position == yearmonth, ]
  files <- as.character(files[[1]])
  
  #Cambiar formato de fechas, nombres en min
  print("Reading crm data")
  crm <-
    fread(
      paste0(crm_path, "/", files),
      colClasses = "character",
      na.strings = c("", "NA")
    )
  names(crm) <- tolower(names(crm))
  
  
  # Date treatment
  print("Dates treatment")
  crm[, periodo := paste0(periodo, "01")]
  crm[, periodo := as.Date(periodo, "%Y%m%d")]
  dates <- grep("fecha", names(crm), value = T)
  crm[, (dates) := lapply(.SD, as.Date), .SDcols = dates]
  
  
  # Read dictionary
  dicc_segmentos <- get.path(dictionary_path, "dic_segmento") %>%
    fread(colClasses = "character", header = T)
  dicc_segmentos[, segmento := factor(segmento)]
  
  
  # Age creation
  crm[, edad := round(interval(start = crm_fecha_nacimiento,
                               end = periodo) /
                        duration(num = 1, units = "years"))]
  
  # Antiguedad creation
  crm[, antiguedad := round(
    interval(start = crm_fecha_vinculacion_banco,
             end = periodo) /
      duration(num = 1, units = "months")
  )]
  
  # Estrato
  crm[crm_estrato == 0, crm_estrato := NA]  ## If "estrato" is 0 then put "NA"
  
  
  # Genero
  crm[crm_genero == "D", crm_genero := NA]  ##If "genero" is "D" then put "NA"
  
  
  # Financial variables (ingreso, activos brutos, etc...)
  fin_vars <- grep("valor", names(crm), value = T)
  
  crm[, (fin_vars) := lapply(.SD, as.numeric), .SDcols = fin_vars]  ##Convert it to numeric
  
  
  # Merge crm with dictionary to create new segmentation
  crm <-
    merge(crm, dicc_segmentos, by = "crm_nombre_segmento", all.x = T)
  
  # crm[, .N, by = crm_nombre_segmento][order(-N)]
  # crm[, .N, by = segmento][order(-N)]
  
  # New variable "segmento_comercial" with specific segmentation
  crm[, segmento_comercial := segmento]
  crm[segmento == "Preferente" & is.na(crm_codigo_subsegmento),
      segmento_comercial := "Preferente"]
  crm[segmento == "Preferente" & crm_codigo_subsegmento == "35",
      segmento_comercial := "Preferente Plus"]
  crm <-
    crm[segmento_comercial != "Pj"]  ##Not take into account "Pj"
  
  # Subsegmento definition
  crm[, subsegmento := ifelse(
    crm_codigo_subsegmento == "35",
    "Preferente Plus",
    ifelse(crm_codigo_subsegmento == "40", "Otro", "NA")
  )]
  
  # Creation "segmentos tacticos"
  cortes_segmentos <- c(min(crm$edad), 13, 17, 25, 59, max(crm$edad))
  nombres_cortes <-
    c("Infantil", "Adolescente", "Joven", "Adulto", "Experiencia")
  # head(cut(crm$edad, cortes_segmentos, include.lowest = T, labels = nombres_cortes))
  crm[, segmento_tactico := cut(edad,
                                cortes_segmentos,
                                include.lowest = T,
                                labels = nombres_cortes)]
  # crm[, .(edad, segmento_tactico)]
  
  
  ##################################################################################################
  print("Cleaning duplicated ID (if there are)")
  # Duplicated values cleaning
  crm[, crm_id := paste0(crm_tipo_identificacion, crm_numero_identificacion)]
  base_duplicados <-
    crm[crm_id %in% crm[duplicated(crm_id), crm_id]][order(-crm_id)]
  # gg_miss_fct(x = base_duplicados, fct = crm_nombre_segmento)
  
  # crm[, .N, by = crm_nombre_segmento][order(-N)]
  
  # Choose variables of interest
  var_interest <-
    c(
      fin_vars,
      "crm_codigo_tipo_vivienda",
      "crm_codigo_nivel_educativo",
      "crm_codigo_estado_civil",
      "crm_estrato"
    )
  base_duplicados[, lapply(.SD, summary), .SDcols = var_interest, by = crm_nombre_segmento]
  
  # Clean duplicated
  limpieza_duplicados <-
    base_duplicados[, .(crm_id, crm_fecha_vinculacion_banco)]
  limpieza_duplicados <-
    limpieza_duplicados[order(crm_id,-crm_fecha_vinculacion_banco)]
  limpieza_duplicados[, crm_fecha_vin_lag := shift(crm_fecha_vinculacion_banco, 1, 0, "lead"), by = crm_id]
  limpieza_duplicados <-
    limpieza_duplicados[crm_fecha_vin_lag != "1970-01-01"]
  limpieza_duplicados[, days := crm_fecha_vinculacion_banco - crm_fecha_vin_lag]
  id_duplicads <- limpieza_duplicados[days < 1, crm_id]
  
  # See if cleaning worked
  crm_no_duplicados <- crm[crm_id %!in% limpieza_duplicados$crm_id]
  crm_no_duplicados[crm_id %in% crm_no_duplicados[duplicated(crm_id), crm_id]][order(-crm_id)]
  
  
  # Unique values dt
  crm_registros_unicos <-
    merge(limpieza_duplicados[, crm_id, crm_fecha_vinculacion_banco],
          crm,
          by = c("crm_id", "crm_fecha_vinculacion_banco"))
  nrow(crm_registros_unicos) == nrow(limpieza_duplicados)
  crm_registros_unicos <-
    crm_registros_unicos[crm_id != id_duplicads |
                           crm_nombre_segmento == "Preferente Medio"]
  crm_registros_unicos[crm_id == id_duplicads]
  
  # Cleaned crm
  setcolorder(crm_registros_unicos, names(crm_no_duplicados))
  crm_lp <-  rbindlist(list(crm_registros_unicos, crm_no_duplicados))
  nrow(crm) - nrow(crm_lp)
  
  nrow(crm_lp)
  rm(crm,
     base_duplicados,
     crm_no_duplicados,
     crm_registros_unicos,
     dicc_segmentos)
  gc()
  
  
  # Missing values per age segment
  # gg_miss_fct(x = crm_lp, fct = segmento_tactico)
  
  
  # Municipio
  dicc_munic <- get.path(dictionary_path, "CIUDAD") %>%
    fread(colClasses = "character", header = T)
  
  names(dicc_munic) <- tolower(names(dicc_munic))
  
  crm_lp[, codigo_depto := as.character(substr(crm_codigo_ciudad_ppal, 1, 2))]
  crm_lp[, codigo_municipio := as.character(substr(crm_codigo_ciudad_ppal, 3, 100))]
  crm_lp <-
    merge(
      crm_lp,
      dicc_munic,
      by = c("codigo_depto", "codigo_municipio"),
      all.x = T
    )
  
  
  #clean memory
  rm(dicc_munic)
  gc()
  print("crm process finished")
  
  ##################################################################################################
  ## tenencia ##
  ##################################################################################################
  # Charge data
  tenencia_path <- paste0(external_path, "/Tenencia_Productos")
  files <- list.files(path = tenencia_path, pattern = ".csv")
  position <-
    paste0(sapply(str_extract_all(files, "[0-9]+"), "[[", 1),
           sapply(str_extract_all(files, "[0-9]+"), "[[", 2))  %>% as.numeric
  
  # comparing cut months from user to master files created
  if (FALSE %in% (yearmonth %in% position)) {
    stop("Does not exist table for some of the months specified")
  }
  
  df_position <-
    data.frame(files = files, position = position)
  
  files <- df_position[df_position$position == yearmonth, ]
  files <- as.character(files[[1]])
  
  #Cambiar formato de fechas, nombres en min
  print("Reading tenencia data")
  tenencia <-
    fread(
      paste0(tenencia_path, "/", files),
      colClasses = "character",
      na.strings = c("", "NA")
    )
  
  names(tenencia) <- tolower(names(tenencia))
  
  
  # Numeric vars
  num_vars <- grep("cant_", names(tenencia), value = T)
  tenencia[, (num_vars) := lapply(.SD, as.numeric), .SDcols = num_vars]
  tenencia[, periodo := NULL]
  
  #Creation columns monoproducto y multiproducto
  tenencia[, count := rowSums(.SD, na.rm = T), .SDcols = num_vars]
  tenencia$monmulti <-
    as.factor(ifelse(tenencia$count == 1, "Monoproducto", "Multiproducto"))
  
  
  # Merge with crm
  setnames(
    crm_lp,
    old = c("crm_tipo_identificacion", "crm_numero_identificacion"),
    new = c("tipo_identificacion", "numero_identificacion")
  )  ##Change name for merge
  crm_lp[tipo_identificacion == "L", tipo_identificacion := "N"]
  
  print("Generating merge between crm and tenencia tables")
  dt <-
    merge(
      crm_lp,
      tenencia,
      by = c("tipo_identificacion", "numero_identificacion"),
      all = T
    )
  
  
  # Verificar que no hay duplicados
  dt[, crm_id := paste0(tipo_identificacion, numero_identificacion)]
  base_duplicados <-
    dt[crm_id %in% dt[duplicated(crm_id), crm_id]][order(-crm_id)]
  
  if (nrow(base_duplicados) > 0) {
    stop("There are duplicated ID's in big table")
  }
  
  rm(tenencia)
  gc()
  
  ###################################################################################
  ## creditos ##
  ###################################################################################
  # Charge data
  cred_path <- paste0(external_path, "/Detalle_Productos")
  files <- list.files(path = cred_path, pattern = "CREDITOS")
  position <-
    paste0(sapply(str_extract_all(files, "[0-9]+"), "[[", 1),
           sapply(str_extract_all(files, "[0-9]+"), "[[", 2))  %>% as.numeric
  
  # comparing cut months from user to master files created
  if (FALSE %in% (yearmonth %in% position)) {
    stop("Does not exist table for some of the months specified")
  }
  
  df_position <-
    data.frame(files = files, position = position)
  
  files <- df_position[df_position$position == yearmonth, ]
  files <- as.character(files[[1]])
  
  #Cambiar formato de fechas, nombres en min
  print("Reading creditos data")
  creditos <-
    fread(
      paste0(cred_path, "/", files),
      colClasses = "character",
      na.strings = c("", "NA")
    )
  
  names(creditos) <- tolower(names(creditos))
  
  
  # Change names
  setnames(
    creditos,
    old = c(
      "attcr_tip_identif",
      "attcr_num_identif" ,
      "attcr_codigo_linea"
    ),
    new = c(
      "tipo_identificacion",
      "numero_identificacion",
      "codigo_linea"
    )
  )
  creds <-
    copy(creditos[, .(tipo_identificacion, numero_identificacion, codigo_linea)])
  
  # Read "tipo ID" dictionary
  dicc_tipoid <- get.path(dictionary_path, "IDENTIFICACION") %>%
    fread(colClasses = "character", header = T)
  
  names(dicc_tipoid) <- tolower(names(dicc_tipoid))
  # names(dicc_tipoid)
  
  # Change name
  setnames(dicc_tipoid, old = "tipo_crm", new = "tipo_identificacion")
  
  setnames(creds, "tipo_identificacion", "btiid_codigo")
  # Create final dictionary
  dicc_cred_fin <-
    merge(creds,
          dicc_tipoid[, .(tipo_identificacion, btiid_codigo)],
          by = 'btiid_codigo',
          all.x = T)
  # names(dicc_cred_fin)
  
  rm(creditos, dicc_tipoid)
  gc()
  
  
  # Merge big data table (dt) with final dict
  print("Merging big data table with final dictionary")
  dt_cred <- merge(
    dt,
    dicc_cred_fin,
    by = c("tipo_identificacion", "numero_identificacion"),
    all.x = T
  )
  
  
  rm(dicc_cred_fin)
  gc()
  
  
  # Especificar nombre de las variables. Asignar codigos 61, 152, 155 y 156 a "Crediestudiantil"
  dt_cred[, codigo_linea := as.factor(codigo_linea)]
  
  rm(base_duplicados, limpieza_duplicados)
  gc()
  
  dt_cred$Crediestudiantil <-
    as.factor(
      ifelse(
        dt_cred$codigo_linea == 61 |
          dt_cred$codigo_linea == 152 |
          dt_cred$codigo_linea == 155 |
          dt_cred$codigo_linea == 156,
        "Crediestudiantil",
        "Otros"
      )
    )
  
  ########################################################################################################
  ##Saving tables
  #crm
  save <- paste0("crm_", yearmonth)
  saveRDS(crm_lp,
          os.path.join(staging_path, "/crm", paste0(save, ".rds")))
  
  #creditos
  save <- paste0("creditos_", yearmonth)
  saveRDS(dt_cred,
          os.path.join(staging_path, "/creditos", paste0(save, ".rds")))
  
  #dt
  save <- paste0("dt_", yearmonth)
  saveRDS(dt,
          os.path.join(staging_path, "/crm-tenencia" , paste0(save, ".rds")))
  
  print("Tables saved :). Finished process.")
}
