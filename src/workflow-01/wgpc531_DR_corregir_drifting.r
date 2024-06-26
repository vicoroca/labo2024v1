# Experimentos Colaborativos Default
# Workflow  Data Drifting repair

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")

#------------------------------------------------------------------------------

options(error = function() {
  traceback(20)
  options(error = NULL)
  
  t <- format(Sys.time(), "%Y%m%d %H%M%S")
  cat( t, "\n",
    file = "z-Rabort.txt",
    append = TRUE
  )

  cat( t, "\n",
    file = "z-Rabort-hist.txt",
    append = TRUE
  )

  stop("exiting after script error")
})
#------------------------------------------------------------------------------

# Parametros del script
PARAM <- read_yaml( "parametros.yml" )

# Si PARAM$variables_iter1 no esta definido, lo defino y seteo en TRUE
if( !exists("PARAM$variables_iter1") ) {
  PARAM$variables_iter1 <- TRUE
}

# Si PARAM$variables_iter2 no esta definido, lo defino y seteo en TRUE
if( !exists("PARAM$variables_iter2") ) {
  PARAM$variables_iter2 <- TRUE
}

OUTPUT <- list()

#------------------------------------------------------------------------------

GrabarOutput <- function() {
  write_yaml(OUTPUT, file = "output.yml") # grabo output
}
#------------------------------------------------------------------------------
# Esta es la parte que los alumnos deben desplegar todo su ingenio
# Agregar aqui sus PROPIAS VARIABLES manuales

AgregarVariables_IntraMes <- function(dataset) {
  gc()
  # INICIO de la seccion donde se deben hacer cambios con variables nuevas

  # creo un ctr_quarter que tenga en cuenta cuando
  # los clientes hace 3 menos meses que estan
  cat( "\n","Inicia variables intrames cátedra")
  dataset[, ctrx_quarter_normalizado := ctrx_quarter]
  dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]
  dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]
  dataset[cliente_antiguedad == 3, ctrx_quarter_normalizado := ctrx_quarter * 1.2]

  # variable extraida de una tesis de maestria de Irlanda
  dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

  # se crean los nuevos campos para MasterCard  y Visa,
  #  teniendo en cuenta los NA's
  # varias formas de combinar Visa_status y Master_status
  dataset[, vm_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
  dataset[, vm_status02 := Master_status + Visa_status]

  dataset[, vm_status03 := pmax(
    ifelse(is.na(Master_status), 10, Master_status),
    ifelse(is.na(Visa_status), 10, Visa_status)
  )]

  dataset[, vm_status04 := ifelse(is.na(Master_status), 10, Master_status)
    + ifelse(is.na(Visa_status), 10, Visa_status)]

  dataset[, vm_status05 := ifelse(is.na(Master_status), 10, Master_status)
    + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]

  dataset[, vm_status06 := ifelse(is.na(Visa_status),
    ifelse(is.na(Master_status), 10, Master_status),
    Visa_status
  )]

  dataset[, mv_status07 := ifelse(is.na(Master_status),
    ifelse(is.na(Visa_status), 10, Visa_status),
    Master_status
  )]


  # combino MasterCard y Visa
  dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

  dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]
  dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]
  dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]
  dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]
  dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]
  dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]
  dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]
  dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]
  dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]
  dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]
  dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]
  dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]
  dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]
  dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]
  dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]
  dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]
  dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]
  dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]
  dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]
  

  # a partir de aqui juego con la suma de Mastercard y Visa
  
  dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]
  dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]
  dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]
  dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]
  dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]
  dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]
  dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]
  dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]
  dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]
  dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]
  dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]
  dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]
  dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]
  dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]
  dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]
  dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]

  cat( "\n","Fin variables intrames cátedra")
  # Aqui debe usted agregar sus propias nuevas variables
  if (PARAM$variables_iter1) {
    cat( "\n","Inicio variables combinadas agregadas iteración 1")
    dataset[, totconsumostcpesos := rowSums(cbind(mtarjeta_visa_consumo, mtarjeta_master_consumo), na.rm = TRUE)]
    dataset[, totconsumostccant  := rowSums(cbind(ctarjeta_visa_transacciones, ctarjeta_master_transacciones), na.rm = TRUE)]
    dataset[, totalprestamospesos := rowSums(cbind(mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios ), na.rm = TRUE)]
    dataset[, totalprestamoscant := rowSums(cbind(cprestamos_personales, cprestamos_prendarios, cprestamos_hipotecarios ), na.rm = TRUE)]
    dataset[, totalinvpesos := rowSums(cbind(mplazo_fijo_dolares, mplazo_fijo_pesos, minversion1_pesos, minversion1_dolares, minversion2 ), na.rm = TRUE)]
    dataset[, totalinvcantidad := rowSums(cbind(cplazo_fijo, cinversion1, cinversion2), na.rm = TRUE)]
    dataset[, totalserv := rowSums(cbind(cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales, ccaja_seguridad), na.rm = TRUE)]
    dataset[, totalpayrollpesos := rowSums(cbind(mpayroll, mpayroll2), na.rm = TRUE)]
    dataset[, totalpayrollcant := rowSums(cbind(cpayroll_trx, cpayroll2_trx), na.rm = TRUE)]
    dataset[, totalgastospesos := rowSums(cbind(mcuenta_debitos_automaticos, mttarjeta_visa_debitos_automaticos,mttarjeta_master_debitos_automaticos, 
                                          mpagodeservicios, mpagomiscuentas), na.rm = TRUE)]
    dataset[, totalgastoscant := rowSums(cbind(ccuenta_debitos_automaticos, ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos,
                                          cpagodeservicios, cpagomiscuentas), na.rm = TRUE)]
    dataset[, totalbenefpesos := rowSums(cbind(mcajeros_propios_descuentos, mtarjeta_visa_descuentos, mtarjeta_master_descuentos), na.rm = TRUE)]
    dataset[, totalbenefcant := rowSums(cbind(ccajeros_propios_descuentos, ctarjeta_visa_descuentos, ctarjeta_master_descuentos), na.rm = TRUE)]
    dataset[, totalforexpesos := rowSums(cbind(mforex_buy, mforex_sell), na.rm = TRUE)]
    dataset[, interaccant := rowSums(cbind(ccallcenter_transacciones, chomebanking_transacciones, ccajas_transacciones, ccajas_consultas, 
                                           ccajas_depositos, ccajas_extracciones, ccajas_otras, catm_trx, catm_trx_other, ctrx_quarter, 
                                           cmobile_app_trx), na.rm = TRUE)]
    
    dataset[, transftotpesos := rowSums(cbind(mtransferencias_recibidas, mtransferencias_emitidas), na.rm = TRUE)]
    dataset[, transftotcant := rowSums(cbind(ctransferencias_recibidas, ctransferencias_emitidas), na.rm = TRUE)]
    dataset[, cheqtotpesos := rowSums(cbind(mcheques_depositados, mcheques_emitidos, mcheques_depositados_rechazados, mcheques_emitidos_rechazados), na.rm = TRUE)]
    dataset[, cheqtotcant := rowSums(cbind(ccheques_depositados, ccheques_emitidos, ccheques_depositados_rechazados, ccheques_emitidos_rechazados), na.rm = TRUE)]
    dataset[, cheqbalpesos := rowSums(cbind(mcheques_depositados, mcheques_emitidos, -mcheques_depositados_rechazados, -mcheques_emitidos_rechazados), na.rm = TRUE)]
    dataset[, cheqbalcant := rowSums(cbind(ccheques_depositados, ccheques_emitidos, -ccheques_depositados_rechazados, -ccheques_emitidos_rechazados), na.rm = TRUE)]
    
    dataset[, masteruso := Master_msaldototal / Master_mlimitecompra]
    dataset[, mastermaniobra := Master_mconsumototal / Master_msaldototal]
    dataset[, masterlimit := Master_mfinanciacion_limite + Master_mlimitecompra]
    dataset[, masterrisk := Master_Finiciomora / Master_fechaalta]
    dataset[, visauso := Visa_msaldototal / Visa_mlimitecompra]
    dataset[, visamaniobra := Visa_mconsumototal / Visa_msaldototal]
    dataset[, visalimit := Visa_mfinanciacion_limite + Visa_mlimitecompra]
    dataset[, visarisk := Visa_Finiciomora / Visa_fechaalta] 
  
    dataset[, movsyantig := rowSums(cbind(totconsumostccant, totalprestamoscant,totalinvcantidad), na.rm = TRUE)/cliente_antiguedad]
    dataset[, tcyantig := totconsumostccant / cliente_antiguedad]
    dataset[, prestyantig := totalprestamoscant / cliente_antiguedad]
    dataset[, invyantig := totalinvcantidad / cliente_antiguedad]
    dataset[, saldoyantig := rowSums(cbind(totconsumostcpesos, totalprestamospesos,totalinvpesos,totalforexpesos), na.rm = TRUE)/cliente_antiguedad]
    dataset[, movsyedad := rowSums(cbind(totconsumostccant, totalprestamoscant,totalinvcantidad), na.rm = TRUE)/cliente_edad]
    dataset[, tcyedad := totconsumostccant / cliente_edad]
    dataset[, prestyedad := totalprestamoscant / cliente_edad]
    dataset[, invyedad := totalinvcantidad / cliente_edad]
    dataset[, saldoyedad := rowSums(cbind(totconsumostcpesos, totalprestamospesos,totalinvpesos,totalforexpesos), na.rm = TRUE)/cliente_edad]
  
    dataset[, cheqydescubierto := cheqbalpesos / cdescubierto_preacordado]
    dataset[, saldoydescubierto := mcuentas_saldo / cdescubierto_preacordado]
    dataset[, volumen := rowSums(cbind(totconsumostccant, totalprestamoscant, totalinvcantidad, totalserv, 
                                           totalpayrollcant, totalgastoscant, totalbenefcant, interaccant, transftotcant, cheqtotcant), na.rm = TRUE)]
    dataset[, saldoyvolumen := mcuentas_saldo * volumen]
    dataset[, saldoytotcuentas := mcuentas_saldo * tcuentas]
    dataset[, saldofactores := rowSums(cbind(saldoyantig,saldoyedad, saldoydescubierto,saldoytotcuentas), na.rm = TRUE)]
    dataset[, vm_mtot_transacciones_deb_cred := ctarjeta_debito_transacciones + ctarjeta_visa_transacciones + ctarjeta_master_transacciones ]
    cat( "\n","Fin variables combinadas agregadas iteración 1")
    
    if (PARAM$variables_iter2) {
      cat( "\n","Inicio variables combinadas agregadas iteración 2")
      dataset[, solidez := rowSums(cbind(saldoyvolumen, saldoytotcuentas), na.rm = TRUE)]
      dataset[, volumenponderado:= as.integer(rowSums(cbind(totalpayrollpesos, totalgastospesos), na.rm = TRUE)/solidez)]
      cat( "\n","Fin variables combinadas agregadas iteración 2")
    }

    
    
    # divide columna por la mediana de columna para ese foto_mes
    cat( "\n","Inicio variables sobre mediana")
    dataset[, mcaja_ahorro_sobre_mediana := mcaja_ahorro / median(mcaja_ahorro, na.rm = TRUE), by = foto_mes]
    dataset[, mcuentas_saldo_sobre_mediana := mcuentas_saldo / median(mcuentas_saldo, na.rm = TRUE), by = foto_mes]
    dataset[, mpasivos_margen_sobre_mediana := mpasivos_margen / median(mpasivos_margen, na.rm = TRUE), by = foto_mes]
    dataset[, mpayroll_sobre_mediana := mpayroll / median(mpayroll, na.rm = TRUE), by = foto_mes]
    dataset[, mprestamos_personales_sobre_mediana := mprestamos_personales / median(mprestamos_personales, na.rm = TRUE), by = foto_mes]
    dataset[, mrentabilidad_annual_sobre_mediana := mrentabilidad_annual / median(mrentabilidad_annual, na.rm = TRUE), by = foto_mes]
    dataset[, mtarjeta_visa_consumo_sobre_mediana := mtarjeta_visa_consumo / median(mtarjeta_visa_consumo, na.rm = TRUE), by = foto_mes]
    dataset[, mtarjeta_master_consumo_sobre_mediana := mtarjeta_master_consumo / median(mtarjeta_master_consumo, na.rm = TRUE), by = foto_mes]
    dataset[, mcuenta_corriente_sobre_mediana := mcuenta_corriente / median(mcuenta_corriente, na.rm = TRUE), by = foto_mes]
    dataset[, mactivos_margen_sobre_mediana := mactivos_margen / median(mactivos_margen, na.rm = TRUE), by = foto_mes]
    dataset[, mrentabilidad_sobre_mediana := mrentabilidad / median(mrentabilidad, na.rm = TRUE), by = foto_mes]
    dataset[, mtransferencias_recibidas_sobre_mediana := mtransferencias_recibidas / median(mtransferencias_recibidas, na.rm = TRUE), by = foto_mes]
    dataset[, mtransferencias_emitidas_sobre_mediana := mtransferencias_emitidas / median(mtransferencias_emitidas, na.rm = TRUE), by = foto_mes]
    
    cat( "\n","Fin variables sobre mediana")
  }
  
  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )

  infinitos_qty <- sum(unlist(infinitos))
  if (infinitos_qty > 0) {
    cat("\n",
      "ATENCION, hay", infinitos_qty,
      "valores infinitos en tu dataset. Seran pasados a NA\n"
    )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )

  nans_qty <- sum(unlist(nans))
  if (nans_qty > 0) {
    cat(
      "ATENCION, hay", nans_qty,
      "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
    )

    cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }
}
#------------------------------------------------------------------------------
# deflaciona por IPC
# momento 1.0  31-dic-2020 a las 23:59

drift_deflacion <- function(campos_monetarios) {
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )

  vIPC <- c(
    1.9903030878, 1.9174403544, 1.8296186587,
    1.7728862972, 1.7212488323, 1.6776304408,
    1.6431248196, 1.5814483345, 1.4947526791,
    1.4484037589, 1.3913580777, 1.3404220402,
    1.3154288912, 1.2921698342, 1.2472681797,
    1.2300475145, 1.2118694724, 1.1881073259,
    1.1693969743, 1.1375456949, 1.1065619600,
    1.0681100000, 1.0370000000, 1.0000000000,
    0.9680542110, 0.9344152616, 0.8882274350,
    0.8532444140, 0.8251880213, 0.8003763543,
    0.7763107219, 0.7566381305, 0.7289384687
  )

  tb_IPC <- data.table(
    paste0(PARAM$dataset_metadata$periodo) := vfoto_mes,
    "IPC" = vIPC
  )

  dataset[tb_IPC,
    on = c(PARAM$dataset_metadata$periodo),
    (campos_monetarios) := .SD * i.IPC,
    .SDcols = campos_monetarios
  ]
}

#------------------------------------------------------------------------------

drift_rank_simple <- function(campos_drift) {
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_rank") :=
      (frank(get(campo), ties.method = "random") - 1) / (.N - 1), by = eval(PARAM$dataset_metadata$periodo)]
    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------
# El cero se transforma en cero
# los positivos se rankean por su lado
# los negativos se rankean por su lado

drift_rank_cero_fijo <- function(campos_drift) {
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[get(campo) == 0, paste0(campo, "_rank") := 0]
    dataset[get(campo) > 0, paste0(campo, "_rank") :=
      frank(get(campo), ties.method = "random") / .N, by = eval(PARAM$dataset_metadata$periodo)]

    dataset[get(campo) < 0, paste0(campo, "_rank") :=
      -frank(-get(campo), ties.method = "random") / .N, by = eval(PARAM$dataset_metadata$periodo)]
    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------

drift_estandarizar <- function(campos_drift) {
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_normal") := 
      (get(campo) -mean(campo, na.rm=TRUE)) / sd(get(campo), na.rm=TRUE),
      by = eval(PARAM$dataset_metadata$periodo)]

    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
OUTPUT$PARAM <- PARAM
OUTPUT$time$start <- format(Sys.time(), "%Y%m%d %H%M%S")

# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
PARAM$dataset <- paste0( "./", PARAM$input, "/dataset.csv.gz" )
PARAM$dataset_metadata <- read_yaml( paste0( "./", PARAM$input, "/dataset_metadata.yml" ) )

dataset <- fread(PARAM$dataset)


GrabarOutput()
write_yaml(PARAM, file = "parametros.yml") # escribo parametros utilizados

# primero agrego las variables manuales
if (PARAM$variables_intrames) AgregarVariables_IntraMes(dataset)

# ordeno dataset
setorderv(dataset, PARAM$dataset_metadata$primarykey)

# por como armé los nombres de campos,
#  estos son los campos que expresan variables monetarias
campos_monetarios <- colnames(dataset)
campos_monetarios <- campos_monetarios[campos_monetarios %like%
  "^(m|Visa_m|Master_m|vm_m)"]

# aqui aplico un metodo para atacar el data drifting
# hay que probar experimentalmente cual funciona mejor
switch(PARAM$metodo,
  "ninguno"        = cat("No hay correccion del data drifting"),
  "rank_simple"    = drift_rank_simple(campos_monetarios),
  "rank_cero_fijo" = drift_rank_cero_fijo(campos_monetarios),
  "deflacion"      = drift_deflacion(campos_monetarios),
  "estandarizar"   = drift_estandarizar(campos_monetarios)
)


#------------------------------------------------------------------------------
# grabo el dataset

fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)

# copia la metadata sin modificar
write_yaml( PARAM$dataset_metadata, 
  file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
  file = "dataset.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
OUTPUT$dataset$ncol <- ncol(dataset)
OUTPUT$dataset$nrow <- nrow(dataset)
OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

# dejo la marca final
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
  file = "z-Rend.txt",
  append = TRUE
)
