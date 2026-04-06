library(e1071)
library(caret)
library(dplyr)

# Cargar el dataset ya limpio
datos <- read.csv(
  "C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/data/dataset_limpio.csv",
  stringsAsFactors = FALSE
)

# Eliminar fila con encabezado colado si existe
datos <- datos %>% filter(etiqueta != "etiqueta")

# Convertir la columna objetivo a factor (requerido por SVM)
datos$etiqueta <- as.factor(datos$etiqueta)

# Forzar columnas numericas y eliminar filas con valores invalidos
datos <- datos %>%
  mutate(
    dist_a      = as.numeric(dist_a),
    dist_b      = as.numeric(dist_b),
    delta_ms    = as.numeric(delta_ms),
    primero_num = ifelse(primero == "A", 1, 0)
  ) %>%
  filter(!is.na(dist_a), !is.na(dist_b), !is.na(delta_ms),
         dist_a != 999, dist_b != 999)

cat("Total registros:", nrow(datos), "\n")
print(table(datos$etiqueta))
cat("\n")

# Separar features (variables de entrada) y etiqueta (variable a predecir)
features <- datos %>% select(dist_a, dist_b, delta_ms, primero_num)
etiqueta  <- datos$etiqueta

# Dividir en 80% entrenamiento y 20% prueba.
# set.seed garantiza que la division sea siempre la misma.
# createDataPartition hace la division manteniendo el balance de clases.
set.seed(42)
indices_train <- createDataPartition(etiqueta, p = 0.8, list = FALSE)

x_train <- features[indices_train, ]
x_test  <- features[-indices_train, ]
y_train <- etiqueta[indices_train]
y_test  <- etiqueta[-indices_train]

cat("Registros de entrenamiento:", nrow(x_train), "\n")
cat("Registros de prueba:       ", nrow(x_test),  "\n\n")

# Entrenar el modelo SVM con kernel lineal.
# scale=TRUE normaliza las variables para que tengan el mismo peso.
cat("Entrenando modelo SVM...\n")
modelo <- svm(
  x      = x_train,
  y      = y_train,
  kernel = "linear",
  cost   = 1,
  scale  = TRUE
)
cat("Modelo entrenado.\n\n")

# Evaluar el modelo con el conjunto de prueba
predicciones <- predict(modelo, x_test)

# confusionMatrix muestra cuantos aciertos y errores hubo por clase
cm <- confusionMatrix(predicciones, y_test)
print(cm)

accuracy   <- cm$overall["Accuracy"]
f1_entrada <- cm$byClass["F1"]

cat("\n=== Metricas del modelo ===\n")
cat("Accuracy:", round(accuracy * 100, 2), "%\n")
cat("F1-score:", round(f1_entrada, 4), "\n")

# Guardar el modelo entrenado para usarlo en la app Shiny
saveRDS(modelo, "C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/modelo/modelo_svm.rds")
cat("\nModelo guardado como modelo_svm.rds\n")
