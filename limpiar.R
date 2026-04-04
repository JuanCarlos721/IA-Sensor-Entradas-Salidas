library(dplyr)

# --- CARGAR DATOS ---
lineas <- readLines("C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/data/dataset.csv")

datos <- read.csv(
  text = gsub('"', '', paste(lineas, collapse = "\n")),
  header = FALSE,
  stringsAsFactors = FALSE
)

colnames(datos) <- c("id", "dist_a", "dist_b", "tiempo_a", "tiempo_b", "delta_ms", "primero", "etiqueta")

# --- ELIMINAR CONSECUTIVOS ---
# entrada, salida, salida, salida, entrada → entrada, salida, entrada
limpios <- datos %>%
  arrange(tiempo_a) %>%
  filter(etiqueta != lag(etiqueta, default = ""))

# Reindexar
limpios$id <- seq_len(nrow(limpios))

# --- RESUMEN ---
conteo <- table(limpios$etiqueta)
cat("Total registros:", nrow(limpios), "\n")
cat("Entradas:", conteo["entrada"], "\n")
cat("Salidas: ", conteo["salida"],  "\n")

# --- GUARDAR ---
write.csv(limpios, "C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/data/dataset_limpio.csv", row.names = FALSE)
cat("Guardado como dataset_limpio.csv\n")