# ============================================================
#  Entrenamiento del Modelo SVM — Contador de Personas
#  Juan Carlos Perez Hernandez
#  UABC — Análisis de Tecnologías Emergentes
# ============================================================

library(e1071)
library(caret)
library(dplyr)

# --- CARGAR DATASET LIMPIO ---
datos <- read.csv(
  "C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/data/dataset_limpio.csv",
  stringsAsFactors = FALSE
)

# Convertir etiqueta a factor
datos$etiqueta <- as.factor(datos$etiqueta)

cat("=== Dataset cargado ===\n")
cat("Total registros:", nrow(datos), "\n")
print(table(datos$etiqueta))
cat("\n")

# --- FEATURES A USAR ---
# dist_a, dist_b: distancias de cada sensor
# delta_ms: diferencia de tiempo entre sensores (clave del clasificador)
# primero: qué sensor se activó antes
datos$primero_num <- ifelse(datos$primero == "A", 1, 0)

features <- datos %>% select(dist_a, dist_b, delta_ms, primero_num)
etiqueta  <- datos$etiqueta

# --- DIVIDIR EN ENTRENAMIENTO Y PRUEBA (80/20) ---
set.seed(42)
indices_train <- createDataPartition(etiqueta, p = 0.8, list = FALSE)

x_train <- features[indices_train, ]
x_test  <- features[-indices_train, ]
y_train <- etiqueta[indices_train]
y_test  <- etiqueta[-indices_train]

cat("Registros de entrenamiento:", nrow(x_train), "\n")
cat("Registros de prueba:       ", nrow(x_test),  "\n\n")

# --- ENTRENAR MODELO SVM ---
cat("Entrenando modelo SVM...\n")
modelo <- svm(
  x      = x_train,
  y      = y_train,
  kernel = "linear",
  cost   = 1,
  scale  = TRUE
)
cat("Modelo entrenado.\n\n")

# --- EVALUACIÓN ---
predicciones <- predict(modelo, x_test)

# Matriz de confusión
cm <- confusionMatrix(predicciones, y_test)
print(cm)

# Métricas individuales
accuracy <- cm$overall["Accuracy"]
f1_entrada <- cm$byClass["F1"]

cat("\n=== Métricas del modelo ===\n")
cat("Accuracy:", round(accuracy * 100, 2), "%\n")
cat("F1-score:", round(f1_entrada, 4), "\n")

# --- GUARDAR MODELO ---
saveRDS(modelo, "C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/modelo/modelo_svm.rds")
cat("\nModelo guardado como modelo_svm.rds\n")
