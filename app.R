library(shiny)
library(shinydashboard)
library(e1071)
library(dplyr)

# Rutas del modelo entrenado y del archivo de datos en tiempo real
MODELO_PATH <- "C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/modelo/modelo_svm.rds"
LIVE_CSV    <- "C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/data/live.csv"

# Cargar el modelo SVM al iniciar la app
modelo_svm <- readRDS(MODELO_PATH)

# --- INTERFAZ ---
ui <- dashboardPage(
  skin = "green",

  dashboardHeader(title = "Contador de Personas"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Monitor en vivo", tabName = "monitor",  icon = icon("users")),
      menuItem("Historial",       tabName = "historial", icon = icon("clock-rotate-left"))
    )
  ),

  dashboardBody(
    tabItems(

      tabItem(tabName = "monitor",
        fluidRow(
          valueBoxOutput("box_adentro", width = 4),
          valueBoxOutput("box_ultimo",  width = 4),
          valueBoxOutput("box_total",   width = 4)
        ),
        fluidRow(
          box(title = "Ultima lectura del sensor", status = "warning",
              solidHeader = TRUE, width = 6,
            verbatimTextOutput("ultima_lectura")
          ),
          box(title = "Estado", status = "primary",
              solidHeader = TRUE, width = 6,
            verbatimTextOutput("estado")
          )
        ),
        fluidRow(
          box(title = "Eventos recientes", status = "success",
              solidHeader = TRUE, width = 12,
            tableOutput("tabla_reciente")
          )
        )
      ),

      tabItem(tabName = "historial",
        fluidRow(
          box(title = "Historial completo", status = "primary",
              solidHeader = TRUE, width = 12,
            tableOutput("tabla_historial")
          )
        ),
        fluidRow(
          box(title = "Entradas vs Salidas", status = "success",
              solidHeader = TRUE, width = 6,
            plotOutput("grafica_conteo")
          ),
          box(title = "Metricas del modelo", status = "info",
              solidHeader = TRUE, width = 6,
            h4("Modelo: SVM kernel lineal"),
            h4("Accuracy: 100%"),
            h4("F1-score: 1.0"),
            h4("Dataset: 206 registros (103 entrada / 103 salida)"),
            br(),
            h4("Features utilizadas:"),
            tags$ul(
              tags$li("dist_a - distancia sensor A (cm)"),
              tags$li("dist_b - distancia sensor B (cm)"),
              tags$li("delta_ms - diferencia de tiempo entre sensores"),
              tags$li("primero_num - sensor que se activo primero (A=1, B=0)")
            )
          )
        )
      )
    )
  )
)

# --- SERVIDOR ---
server <- function(input, output, session) {

  # Variables reactivas: se actualizan automaticamente cuando cambian
  contador_adentro <- reactiveVal(0)
  filas_procesadas <- reactiveVal(0)
  ultimo_evento    <- reactiveVal("Esperando...")

  # Data frame vacio donde se guardara el historial de eventos
  historial <- reactiveVal(data.frame(
    tiempo     = character(),
    dist_a     = numeric(),
    dist_b     = numeric(),
    delta_ms   = numeric(),
    primero    = character(),
    prediccion = character(),
    stringsAsFactors = FALSE
  ))

  # Revisar el archivo live.csv cada segundo buscando filas nuevas.
  # invalidateLater hace que este bloque se ejecute cada 1000ms.
  observe({
    invalidateLater(1000, session)

    if (!file.exists(LIVE_CSV)) return()

    datos <- tryCatch(
      read.csv(LIVE_CSV, stringsAsFactors = FALSE),
      error = function(e) NULL
    )

    if (is.null(datos) || nrow(datos) == 0) return()
    if (nrow(datos) <= filas_procesadas()) return()

    # Procesar solo las filas que no se han visto antes
    nuevas <- datos[(filas_procesadas() + 1):nrow(datos), ]
    filas_procesadas(nrow(datos))

    for (i in seq_len(nrow(nuevas))) {
      fila <- nuevas[i, ]

      tryCatch({
        dist_a      <- as.numeric(fila$dist_a)
        dist_b      <- as.numeric(fila$dist_b)
        delta_ms    <- as.numeric(fila$delta_ms)
        primero     <- trimws(fila$primero)
        primero_num <- ifelse(primero == "A", 1, 0)

        if (is.na(dist_a) || is.na(dist_b) || is.na(delta_ms)) return()

        # Clasificar el evento con el modelo SVM
        nueva_lectura <- data.frame(
          dist_a      = dist_a,
          dist_b      = dist_b,
          delta_ms    = delta_ms,
          primero_num = primero_num
        )
        prediccion <- as.character(predict(modelo_svm, nueva_lectura))

        # Actualizar el contador segun la prediccion
        if (prediccion == "entrada") {
          contador_adentro(contador_adentro() + 1)
        } else {
          contador_adentro(max(0, contador_adentro() - 1))
        }

        ultimo_evento(toupper(prediccion))

        nuevo_reg <- data.frame(
          tiempo     = format(Sys.time(), "%H:%M:%S"),
          dist_a     = dist_a,
          dist_b     = dist_b,
          delta_ms   = delta_ms,
          primero    = primero,
          prediccion = toupper(prediccion),
          stringsAsFactors = FALSE
        )
        historial(rbind(nuevo_reg, historial()))

      }, error = function(e) NULL)
    }
  })

  # Outputs del dashboard
  output$box_adentro <- renderValueBox({
    valueBox(contador_adentro(), "Personas adentro", icon = icon("users"), color = "green")
  })

  output$box_ultimo <- renderValueBox({
    color <- if (ultimo_evento() == "ENTRADA") "green" else "red"
    valueBox(ultimo_evento(), "Ultimo evento", icon = icon("person-walking"), color = color)
  })

  output$box_total <- renderValueBox({
    valueBox(nrow(historial()), "Total de eventos", icon = icon("list"), color = "blue")
  })

  output$estado <- renderText({
    h <- historial()
    if (nrow(h) == 0) return("Esperando eventos del Arduino...\nAsegurate de que lector_serial.R esta corriendo.")
    paste0("Sistema activo.\nUltima actualizacion: ", h[1, "tiempo"])
  })

  output$ultima_lectura <- renderText({
    h <- historial()
    if (nrow(h) == 0) return("Sin lecturas aun.")
    u <- h[1, ]
    paste0(
      "Evento:   ", u$prediccion, "\n",
      "Sensor A: ", u$dist_a, " cm\n",
      "Sensor B: ", u$dist_b, " cm\n",
      "Delta:    ", u$delta_ms, " ms\n",
      "Primero:  ", u$primero
    )
  })

  output$tabla_reciente <- renderTable({
    h <- historial()
    if (nrow(h) == 0) return(data.frame(Mensaje = "Sin eventos aun."))
    head(h, 10)
  })

  output$tabla_historial <- renderTable({
    h <- historial()
    if (nrow(h) == 0) return(data.frame(Mensaje = "Sin eventos aun."))
    h
  })

  output$grafica_conteo <- renderPlot({
    h <- historial()
    if (nrow(h) == 0) return(NULL)
    conteo <- table(h$prediccion)
    barplot(conteo, col = c("#2ecc71", "#e74c3c"),
            main = "Entradas vs Salidas", ylab = "Cantidad",
            border = NA, ylim = c(0, max(conteo) + 2))
  })
}

shinyApp(ui, server)
