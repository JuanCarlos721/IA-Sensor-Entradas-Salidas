# ============================================================
#  Lector Serial — Contador de Personas
#  Juan Carlos Perez Hernandez
#  UABC — Análisis de Tecnologías Emergentes
#
#  Corre este script ANTES de abrir la app Shiny.
#  Lee el puerto serial del Arduino y escribe los datos
#  a live.csv para que Shiny los lea en tiempo real.
# ============================================================
#install.packages("serial")


library(serial)
con <- serialConnection(
  name    = "test",
  port    = "COM3",
  mode    = "9600,n,8,1",
  newline = 1
)
open(con)
Sys.sleep(8)  # pasa la mano varias veces durante estos 8 segundos
datos <- read.serialConnection(con)
cat("Datos:\n", datos, "\n")
close(con)

# --- CONFIGURACIÓN ---
PUERTO  <- "COM3"   # cambia si tu Arduino está en otro puerto
BAUDIOS <- 9600
ARCHIVO <- "C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/data/live.csv"

# --- CREAR ARCHIVO CON ENCABEZADO ---
write.csv(
  data.frame(
    id       = integer(),
    dist_a   = numeric(),
    dist_b   = numeric(),
    tiempo_a = numeric(),
    tiempo_b = numeric(),
    delta_ms = numeric(),
    primero  = character(),
    etiqueta = character()
  ),
  ARCHIVO,
  row.names = FALSE
)
cat("Archivo live.csv creado.\n")

# --- CONECTAR AL ARDUINO ---
con <- serialConnection(
  name    = "arduino",
  port    = PUERTO,
  mode    = paste0(BAUDIOS, ",n,8,1"),
  newline = 1
)
open(con)
cat("Conectado al Arduino en", PUERTO, "\n")
cat("Escuchando... (presiona STOP para detener)\n\n")

# --- LEER Y ESCRIBIR EN LOOP ---
repeat {
  Sys.sleep(0.5)
  
  datos <- tryCatch(
    read.serialConnection(con),
    error = function(e) ""
  )
  
  if (is.null(datos) || nchar(datos) == 0) next
  
  cat("RAW:", datos, "\n")  # debug temporal
  
  lineas <- unlist(strsplit(datos, "\r\n|\n|\r"))
  
  for (linea in lineas) {
    linea <- gsub('"', '', trimws(linea))
    if (nchar(linea) == 0 || grepl("id,dist", linea)) next
    
    partes <- strsplit(linea, ",")[[1]]
    if (length(partes) >= 8) {
      cat("Evento:", partes[8], "\n")
      nueva_fila <- data.frame(
        id=as.integer(partes[1]), dist_a=as.numeric(partes[2]),
        dist_b=as.numeric(partes[3]), tiempo_a=as.numeric(partes[4]),
        tiempo_b=as.numeric(partes[5]), delta_ms=as.numeric(partes[6]),
        primero=trimws(partes[7]), etiqueta=trimws(partes[8])
      )
      write.table(nueva_fila, ARCHIVO, append=TRUE, sep=",",
                  row.names=FALSE, col.names=FALSE)
    }
  }
}