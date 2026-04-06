library(dplyr)

# Leer el archivo como texto puro para manejar las comillas del monitor serial
lineas <- readLines("C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/data/dataset.csv")

# Quitar comillas dobles y convertir a data frame
datos <- read.csv(
  text = gsub('"', '', paste(lineas, collapse = "\n")),
  header = FALSE,
  stringsAsFactors = FALSE
)

colnames(datos) <- c("id", "dist_a", "dist_b", "tiempo_a", "tiempo_b", "delta_ms", "primero", "etiqueta")

# Eliminar eventos consecutivos del mismo tipo.
# lag() compara cada fila con la anterior.
# Si dos entradas o dos salidas aparecen seguidas, se queda solo la primera.
limpios <- datos %>%
  arrange(tiempo_a) %>%
  filter(etiqueta != lag(etiqueta, default = ""))

# Reindexar los IDs
limpios$id <- seq_len(nrow(limpios))

conteo <- table(limpios$etiqueta)
cat("Total registros:", nrow(limpios), "\n")
cat("Entradas:", conteo["entrada"], "\n")
cat("Salidas: ", conteo["salida"],  "\n")

write.csv(limpios, "C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/data/dataset_limpio.csv", row.names = FALSE)
cat("Guardado como dataset_limpio.csv\n")
