# ============================================================
#  Limpieza de Dataset — Contador de Personas
#  Juan Carlos Perez Hernandez
#  UABC — Análisis de Tecnologías Emergentes
#
#  Problema: los brazos al caminar activan los sensores
#  generando registros duplicados consecutivos.
#  Solución: si dos etiquetas iguales aparecen seguidas
#  dentro de una ventana de tiempo, se elimina la segunda.
# ============================================================

library(dplyr)

# --- CONFIGURACIÓN ---
ARCHIVO_ENTRADA <- "dataset.csv"   # tu archivo CSV del monitor serial
ARCHIVO_SALIDA  <- "dataset_limpio.csv"
VENTANA_MS      <- 3000            # si dos eventos iguales ocurren en menos
                                   # de 3 segundos, el segundo es falso

# --- CARGAR DATOS ---
datos <- read.csv(ARCHIVO_ENTRADA, stringsAsFactors = FALSE)

cat("Registros originales:", nrow(datos), "\n")

# --- LIMPIEZA ---
# Ordenar por tiempo_a por si acaso quedaron desordenados
datos <- datos %>% arrange(tiempo_a)

# Detectar y eliminar duplicados consecutivos
limpios <- datos %>%
  mutate(
    # Etiqueta del registro anterior
    etiqueta_anterior = lag(etiqueta),
    # Tiempo del registro anterior
    tiempo_anterior   = lag(tiempo_a),
    # Diferencia de tiempo con el registro anterior
    diff_tiempo       = tiempo_a - tiempo_anterior
  ) %>%
  filter(
    # Eliminar si: misma etiqueta que el anterior Y ocurrió muy rápido
    !(etiqueta == etiqueta_anterior & diff_tiempo < VENTANA_MS)
  ) %>%
  select(-etiqueta_anterior, -tiempo_anterior, -diff_tiempo)

cat("Registros después de limpieza:", nrow(limpios), "\n")
cat("Registros eliminados:", nrow(datos) - nrow(limpios), "\n\n")

# --- BALANCE DE CLASES ---
conteo <- table(limpios$etiqueta)
cat("Entradas:", conteo["entrada"], "\n")
cat("Salidas: ", conteo["salida"],  "\n\n")

# --- GUARDAR ---
write.csv(limpios, ARCHIVO_SALIDA, row.names = FALSE)
cat("Dataset limpio guardado como:", ARCHIVO_SALIDA, "\n")
