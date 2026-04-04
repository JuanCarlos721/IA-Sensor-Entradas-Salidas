# ============================================================
#  Contador de Personas — App Shiny
#  Juan Carlos Perez Hernandez
#  UABC — Análisis de Tecnologías Emergentes
#
#  Lee datos del Arduino por serial, clasifica con SVM
#  y muestra el contador en tiempo real
# ============================================================

library(shiny)
library(shinydashboard)
library(e1071)
library(dplyr)
library(serial)

# --- CONFIGURACIÓN ---
PUERTO_SERIAL <- "COM3"      # cambia si tu Arduino está en otro puerto
BAUDIOS       <- 9600
MODELO_PATH   <- "C:/Users/Juan Carlos/Downloads/UNI/Analisis tecnoligias emergentes/Proyecto/modelo/modelo_svm.rds"

# --- CARGAR MODELO ---
modelo_svm <- readRDS(MODELO_PATH)

# ============================================================
# INTERFAZ
# ============================================================
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Contador de Personas"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Monitor en vivo", tabName = "monitor", icon = icon("person-walking")),
      menuItem("Historial",       tabName = "historial", icon = icon("clock-rotate-left"))
    )
  ),
  
  dashboardBody(
    
    # --- Tab Monitor ---
    tabItems(
      tabItem(tabName = "monitor",
              
              fluidRow(
                # Contador personas adentro
                valueBoxOutput("box_adentro", width = 4),
                # Último evento
                valueBoxOutput("box_ultimo",  width = 4),
                # Total de eventos
                valueBoxOutput("box_total",   width = 4)
              ),
              
              fluidRow(
                box(title = "Estado del sistema", status = "primary", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("estado_serial")
                ),
                box(title = "Última lectura del sensor", status = "warning", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("ultima_lectura")
                )
              ),
              
              fluidRow(
                box(title = "Eventos recientes", status = "success", solidHeader = TRUE, width = 12,
                    tableOutput("tabla_reciente")
                )
              )
      ),
      
      # --- Tab Historial ---
      tabItem(tabName = "historial",
              fluidRow(
                box(title = "Historial completo de eventos", status = "primary", solidHeader = TRUE, width = 12,
                    tableOutput("tabla_historial")
                )
              ),
              fluidRow(
                box(title = "Entradas vs Salidas", status = "success", solidHeader = TRUE, width = 6,
                    plotOutput("grafica_conteo")
                ),
                box(title = "Métricas del modelo", status = "info", solidHeader = TRUE, width = 6,
                    h4("Modelo: SVM kernel lineal"),
                    h4("Accuracy: 100%"),
                    h4("F1-score: 1.0"),
                    h4("Dataset: 206 registros (103 entrada / 103 salida)"),
                    br(),
                    h4("Features utilizadas:"),
                    tags$ul(
                      tags$li("dist_a — distancia sensor A (cm)"),
                      tags$li("dist_b — distancia sensor B (cm)"),
                      tags$li("delta_ms — diferencia de tiempo entre sensores"),
                      tags$li("primero_num — qué sensor se activó primero (A=1, B=0)")
                    )
                )
              )
      )
    )
  )
)

# ============================================================
# SERVIDOR
# ============================================================
server <- function(input, output, session) {
  
  # --- ESTADO REACTIVO ---
  contador_adentro <- reactiveVal(0)
  historial        <- reactiveVal(data.frame(
    tiempo     = character(),
    dist_a     = numeric(),
    dist_b     = numeric(),
    delta_ms   = numeric(),
    primero    = character(),
    prediccion = character(),
    stringsAsFactors = FALSE
  ))
  ultimo_evento  <- reactiveVal("Esperando...")
  estado_conexion <- reactiveVal("Conectando al Arduino...")
  
  # --- CONEXIÓN SERIAL ---
  con <- tryCatch({
    puerto <- serialConnection(
      name     = "arduino",
      port     = PUERTO_SERIAL,
      mode     = paste0(BAUDIOS, ",n,8,1"),
      newline  = 1
    )
    open(puerto)
    estado_conexion("Conectado al Arduino en COM3 a 9600 baudios.")
    puerto
  }, error = function(e) {
    estado_conexion(paste("Error al conectar:", e$message))
    NULL
  })
  
  # --- LECTURA EN TIEMPO REAL (cada 200ms) ---
  observe({
    invalidateLater(200, session)
    
    if (is.null(con)) return()
    
    linea <- tryCatch(readLines(con, n = 1), error = function(e) NULL)
    if (is.null(linea) || length(linea) == 0 || linea == "") return()
    
    # Ignorar encabezado
    if (grepl("id,dist", linea)) return()
    
    # Parsear CSV
    partes <- strsplit(trimws(linea), ",")[[1]]
    if (length(partes) < 7) return()
    
    tryCatch({
      dist_a   <- as.numeric(partes[2])
      dist_b   <- as.numeric(partes[3])
      delta_ms <- as.numeric(partes[6])
      primero  <- trimws(partes[7])
      primero_num <- ifelse(primero == "A", 1, 0)
      
      # Ignorar lecturas con 999 en ambos sensores
      if (is.na(dist_a) || is.na(dist_b)) return()
      
      # Clasificar con SVM
      nueva_lectura <- data.frame(
        dist_a      = dist_a,
        dist_b      = dist_b,
        delta_ms    = delta_ms,
        primero_num = primero_num
      )
      prediccion <- as.character(predict(modelo_svm, nueva_lectura))
      
      # Actualizar contador
      if (prediccion == "entrada") {
        contador_adentro(contador_adentro() + 1)
      } else {
        contador_adentro(max(0, contador_adentro() - 1))
      }
      
      # Actualizar último evento
      ultimo_evento(toupper(prediccion))
      
      # Agregar al historial
      nuevo_registro <- data.frame(
        tiempo     = format(Sys.time(), "%H:%M:%S"),
        dist_a     = dist_a,
        dist_b     = dist_b,
        delta_ms   = delta_ms,
        primero    = primero,
        prediccion = toupper(prediccion),
        stringsAsFactors = FALSE
      )
      historial(rbind(nuevo_registro, historial()))
      
    }, error = function(e) NULL)
  })
  
  # --- OUTPUTS ---
  output$box_adentro <- renderValueBox({
    valueBox(
      value    = contador_adentro(),
      subtitle = "Personas adentro",
      icon     = icon("users"),
      color    = "green"
    )
  })
  
  output$box_ultimo <- renderValueBox({
    color <- if (ultimo_evento() == "ENTRADA") "green" else "red"
    valueBox(
      value    = ultimo_evento(),
      subtitle = "Último evento",
      icon     = icon("person-walking"),
      color    = color
    )
  })
  
  output$box_total <- renderValueBox({
    valueBox(
      value    = nrow(historial()),
      subtitle = "Total de eventos",
      icon     = icon("list"),
      color    = "blue"
    )
  })
  
  output$estado_serial <- renderText({
    estado_conexion()
  })
  
  output$ultima_lectura <- renderText({
    h <- historial()
    if (nrow(h) == 0) return("Sin lecturas aún.")
    ultima <- h[1, ]
    paste0(
      "Evento:   ", ultima$prediccion, "\n",
      "Sensor A: ", ultima$dist_a, " cm\n",
      "Sensor B: ", ultima$dist_b, " cm\n",
      "Delta:    ", ultima$delta_ms, " ms\n",
      "Primero:  ", ultima$primero
    )
  })
  
  output$tabla_reciente <- renderTable({
    h <- historial()
    if (nrow(h) == 0) return(data.frame(Mensaje = "Sin eventos aún."))
    head(h, 10)
  })
  
  output$tabla_historial <- renderTable({
    h <- historial()
    if (nrow(h) == 0) return(data.frame(Mensaje = "Sin eventos aún."))
    h
  })
  
  output$grafica_conteo <- renderPlot({
    h <- historial()
    if (nrow(h) == 0) return(NULL)
    conteo <- table(h$prediccion)
    barplot(
      conteo,
      col    = c("green", "red"),
      main   = "Entradas vs Salidas",
      ylab   = "Cantidad",
      border = NA
    )
  })
  
  # --- CERRAR SERIAL AL SALIR ---
  session$onSessionEnded(function() {
    if (!is.null(con)) tryCatch(close(con), error = function(e) NULL)
  })
}

# ============================================================
shinyApp(ui, server)