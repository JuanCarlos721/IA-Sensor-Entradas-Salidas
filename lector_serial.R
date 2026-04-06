library(serial)

# Configuracion del puerto serial y archivo de salida
PUERTO  <- "COM3"
BAUDIOS <- 9600
ARCHIVO <- "C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/data/live.csv"

# Crear el archivo CSV vacio con encabezado
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

# Conectar al Arduino por el puerto serial
con <- serialConnection(
  name    = "arduino",
  port    = PUERTO,
  mode    = paste0(BAUDIOS, ",n,8,1"),
  newline = 1
)
open(con)
cat("Conectado al Arduino en", PUERTO, "\n")
cat("Escuchando... (presiona STOP para detener)\n\n")

# Loop principal: lee datos del serial y los escribe al CSV.
# read.serialConnection lee todo lo que haya en el buffer del puerto.
# strsplit separa el texto en lineas individuales para procesarlas una por una.
repeat {
  Sys.sleep(0.5)

  datos <- tryCatch(
    read.serialConnection(con),
    error = function(e) ""
  )

  if (is.null(datos) || nchar(datos) == 0) next

  cat("RAW:", datos, "\n")

  lineas <- unlist(strsplit(datos, "\r\n|\n|\r"))

  for (linea in lineas) {
    linea <- gsub('"', '', trimws(linea))
    if (nchar(linea) == 0 || grepl("id,dist", linea)) next

    # Separar la linea CSV en partes
    partes <- strsplit(linea, ",")[[1]]
    if (length(partes) >= 8) {
      cat("Evento:", partes[8], "\n")
      nueva_fila <- data.frame(
        id       = as.integer(partes[1]),
        dist_a   = as.numeric(partes[2]),
        dist_b   = as.numeric(partes[3]),
        tiempo_a = as.numeric(partes[4]),
        tiempo_b = as.numeric(partes[5]),
        delta_ms = as.numeric(partes[6]),
        primero  = trimws(partes[7]),
        etiqueta = trimws(partes[8])
      )
      # Agregar la fila al CSV sin sobreescribir las anteriores
      write.table(nueva_fila, ARCHIVO, append = TRUE, sep = ",",
                  row.names = FALSE, col.names = FALSE)
    }
  }
}
