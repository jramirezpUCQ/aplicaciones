# ==========================================================
#                   PRUEBA DE DOMINANCIA
# ==========================================================

################################################################################
#LIBRERIAS
################################################################################

library(shiny)
library(fmsb)
library(shinyjs)
library(shinyscreenshot)

################################################################################
#INTERFAS
################################################################################

ui <- fluidPage(
  #añadimos de fondo el fondo de la universidad
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('fondo_dashboards.jpg');
        background-size: cover;
        background-repeat: repeat;
        background-attachment: fixed;
      }
    "))
  ),
  
  titlePanel("PRUEBA DE DOMINANCIA CEREBRAL"),
  h1("La Universidad Cuauhtémoc, Plantel Querétaro, A.C. con domicilio en Blvd. 
     Bernardo Quintana Arrioja No. 229 A, Fraccionamiento Los Arcos, Santiago de 
     Querétaro, Qro. México, utilizara sus datos personales y sensibles para la 
     identificación, operacion, administración y análogos, que sean necesarios 
     para la prestación de los servicios académicos y administrativos de la 
     Universidad Cuauhtémoc Plantel QUerétaro, A.C. Para mayor información usted 
     puede acceder al aviso de privacidad en https://www.ucq.edu.mx/uc ",
     style="font-size:10px;"),
  
  #encabezados previos a los datos personales
  h1("Al día de hoy, ¿qué parte del cerebro estas utilizando más",
     style = "font-size:16px;"),
  h1("De acuerdo a esa parte de tu cerebro, ¿qué carrera UCQ te hará más exitoso?",
     style = "font-size:16px;"),
  
  #donde van los datos personales del que esta contestando
  fluidRow(column(6,textInput("nombre", "Nombre completo")),
           column(6,numericInput("edad", "Edad",value = NA, min = 1, max = 120))),
  fluidRow(column(6,textInput("carrera", "Carrera a la que aspiras")),
           column(6,dateInput("f_nacimiento", "Fecha de nacimiento"))),
  fluidRow(column(6,textInput("escuela", "Escuela de procedencia")),
           column(6,textInput("grado", "Grado"))),
  fluidRow(column(6,textInput("correo", "Correo electrónico")),
           column(6,numericInput("telefono", "Teléfono",value = NA, min = 0, max = 999999999999))),
  fluidRow(column(6,textInput("tutor", "Nombre completo (tutor)"))),
  fluidRow(column(6,textInput("correo_tutor", "Correo electrónico (tutor)")),
           column(6,numericInput("telefono_tutor", "Télefono (tutor)",value = NA, min = 0, max = 999999999999))),
  
  #Aqui van a ir las tres diferentes partes de la prueba en una columna lateral
  sidebarLayout(
    sidebarPanel(
      # Etapa 1: seleccionar 8 de los 16 elementos
      conditionalPanel(
        condition = "output.stage == 1",
        #una instrucción que se imprime en la side bar
        h3("Analiza los siguientes 16 elementos laborales listados y selecciona 
           ocho elementos que consideras Realizar mejor.",
           style = "font-size:16px;"),
        #el nombre del output de esta etapa
        uiOutput("etapa1_ui"),
        #los botones que va a haber
        actionButton("next1", "Continuar parte 2"),
      ),
      
      # Etapa 2: Seleccionar 1 de las 5 anteriores
      conditionalPanel(
        condition = "output.stage == 2",
        h3("De los ocho elementos seleccionados, selecciona aquel que
           te Agrade/guste más hacer", style = "font-size:16px;"),
        uiOutput("etapa2_ui"),  # Cambiado a uiOutput para evitar el error
        br(),
        actionButton("next2", "Continuar parte 3"),
        actionButton("back1", "Volver a la parte 1")
      ),
      
      # Etapa 3: Mostrar las 5 no seleccionadas al inicio
      conditionalPanel(
        condition = "output.stage == 3",
        h3("De los ocho elementos que no seleccionaste, selecciona aquel elemento que
           menos te agrade o menos te guste hacer", style = "font-size:16px;"),
        uiOutput("etapa3_ui"),
        br(),
        actionButton("back2", "Volver a Etapa 2"),
        actionButton("next3", "Ver Resultados Finales")
      ),
      
      # Etapa 4: Resultados finales
      conditionalPanel(
        condition = "output.stage == 4",
        h3("Resultados Finales"),
        br(),
        actionButton("back3", "Volver a Etapa 3"),
        actionButton("reset", "Comenzar de nuevo"),
        downloadButton("descargar", "Descargar resultados"),
        actionButton("descargar_pdf", "Descargar resultados (PDF)")
      )
    ),
    
    mainPanel(
      h3("Progreso"),
      textOutput("progreso"),
      br(),
      h3("Selecciones Actuales"),
      verbatimTextOutput("summary"),
      h4("Gráfico de resultados:"),
      plotOutput("grafica_final",height = "600px",width  = "100%") 
    )
  )
)

################################################################################
#SERVER
################################################################################

server <- function(input, output, session) {
  # Aquí se definen las abreviaciones y el nombre completpo de los elementos
  checkbox_ids <- c(
    "ana","admin","concep","expre",
    "inte","redac","aspt","imp",
    "plan","aspi","res","ino",
    "entf","org","aspc","aspf"
  )
  
  checkbox_labels <- c(
    ana = "Análisis",
    admin = "Administración",
    concep = "Conceptualización",
    expre = "Expresión de ideas",
    inte = "Integración",
    redac = "Redacción",
    aspt = "Aspectos técnicos",
    imp = "Implantación",
    plan = "Planeación",
    aspi = "Aspectos interpersonales",
    res = "Resolución de problemas lógicos",
    ino = "Innovación",
    entf = "Entrenamiento / Formación",
    org = "Organización",
    aspc = "Aspectos creativos",
    aspf = "Aspectos financieros"
  )
  
  #definimos las áreas de dominancia y que ids pertenecen a dicha área
  supIzq<-c("ana","aspt","res","aspf")
  supDer<-c("concep","inte","ino","aspc")
  infIzq<-c("admin","imp","plan","org")
  infDer<-c("expre","redac","aspi","entf")
  
  #tabla para las sumatorias (ahora es una reactive value)
  data <- reactiveVal(
    data.frame(
      zona = c(rep("supIzq",4), rep("supDer",4), rep("infIzq",4), rep("infDer",4)),
      id = c(supIzq, supDer, infIzq, infDer),
      puntaje = rep(0, 16),
      stringsAsFactors = FALSE
    )
  )
  
  # Resultados de zonas (reactive value para que esté disponible en toda la app)
  resultados_zonas <- reactiveVal(NULL)
  
  # Crear opciones combinadas
  opciones_combinadas <- setNames(checkbox_ids, checkbox_labels[checkbox_ids])
  
  stage <- reactiveVal(1)
  
  selections <- reactiveValues(
    etapa1_ids = character(0),
    etapa1_labels = character(0),
    etapa2_id = character(0),
    etapa2_label = character(0),
    etapa3_id = character(0),
    etapa3_label = character(0),
    no_seleccionadas_ids = character(0),
    no_seleccionadas_labels = character(0)
  )
  
  shinyjs::useShinyjs()
  
  # UI para etapa 1
  output$etapa1_ui <- renderUI({
    checkboxGroupInput(
      "etapa1",
      label = "Selecciona exactamente 8 habilidades:",
      choices = opciones_combinadas,
      selected = character(0)
    )
  })
  
  # Validación etapa 1
  observe({
    if(!is.null(input$etapa1) && length(input$etapa1) == 8) {
      shinyjs::enable("next1")
    } else {
      shinyjs::disable("next1")
    }
  })
  
  # Avanzar a etapa 2
  observeEvent(input$next1, {
    if(length(input$etapa1) == 8) {
      selections$etapa1_ids <- input$etapa1
      selections$etapa1_labels <- checkbox_labels[input$etapa1]
      selections$no_seleccionadas_ids <- setdiff(checkbox_ids, input$etapa1)
      selections$no_seleccionadas_labels <- checkbox_labels[selections$no_seleccionadas_ids]
      stage(2)
    }
  })
  
  # UI para etapa 2
  output$etapa2_ui <- renderUI({
    if(stage() == 2 && length(selections$etapa1_ids) > 0) {
      radioButtons(
        "etapa2_input",
        label = "Selecciona tu habilidad principal:",
        choices = setNames(selections$etapa1_ids, selections$etapa1_labels)
      )
    } else {
      p("Cargando opciones...")
    }
  })
  
  # Avanzar a etapa 3
  observeEvent(input$next2, {
    if(!is.null(input$etapa2_input) && nchar(input$etapa2_input) > 0) {
      selections$etapa2_id <- input$etapa2_input
      selections$etapa2_label <- checkbox_labels[input$etapa2_input]
      stage(3)
    }
  })
  
  # UI para etapa 3
  output$etapa3_ui <- renderUI({
    if(stage() == 3 && length(selections$no_seleccionadas_ids) > 0) {
      radioButtons(
        "etapa3_input",
        label = "Selecciona 1 habilidad de las no elegidas:",
        choices = setNames(selections$no_seleccionadas_ids, selections$no_seleccionadas_labels)
      )
    } else {
      p("Cargando opciones...")
    }
  })
  
  # Avanzar a etapa 4
  observeEvent(input$next3, {
    if(!is.null(input$etapa3_input) && nchar(input$etapa3_input) > 0) {
      selections$etapa3_id <- input$etapa3_input
      selections$etapa3_label <- checkbox_labels[input$etapa3_input]
      
      # Calcular resultados al llegar a la etapa 4
      calcularResultados()
      
      stage(4)
    }
  })
  
  # Función para calcular resultados
  calcularResultados <- function() {
    # Reiniciar la tabla de datos
    datos_tabla <- data.frame(
      zona = c(rep("supIzq",4), rep("supDer",4), rep("infIzq",4), rep("infDer",4)),
      id = c(supIzq, supDer, infIzq, infDer),
      puntaje = rep(0, 16),
      stringsAsFactors = FALSE
    )
    
    # Sumar puntos según selecciones
    datos_tabla$puntaje[datos_tabla$id %in% selections$etapa1_ids] <- 1
    datos_tabla$puntaje[datos_tabla$id == selections$etapa2_id] <- datos_tabla$puntaje[datos_tabla$id == selections$etapa2_id] + 2
    datos_tabla$puntaje[datos_tabla$id == selections$etapa3_id] <- datos_tabla$puntaje[datos_tabla$id == selections$etapa3_id] - 1
    
    # Actualizar la reactive value
    data(datos_tabla)
    
    # Calcular suma por zona
    lista_dominancias <- unique(datos_tabla$zona)
    resul_zonas <- data.frame()
    
    for (i in 1:length(lista_dominancias)) {
      suma <- sum(datos_tabla$puntaje[datos_tabla$zona == lista_dominancias[i]])
      temprow <- data.frame(zona = lista_dominancias[i], puntaje = suma)
      resul_zonas <- rbind(resul_zonas, temprow)
    }
    
    # Ordenar las zonas
    zonas_orden <- c("supIzq", "supDer", "infIzq", "infDer")
    resul_zonas <- resul_zonas[match(zonas_orden, resul_zonas$zona), ]
    
    # Asignar nombres completos
    nombres_zonas <- c(
      "supIzq" = "Superior izquierdo",
      "supDer" = "Superior derecho", 
      "infIzq" = "Inferior izquierdo",
      "infDer" = "Inferior derecho"
    )
    
    resul_zonas$zona_nombre <- nombres_zonas[resul_zonas$zona]
    
    # Guardar en reactive value
    resultados_zonas(resul_zonas)
  }
  
  # Botones para retroceder
  observeEvent(input$back1, {
    stage(1)
  })
  
  observeEvent(input$back2, {
    stage(2)
  })
  
  observeEvent(input$back3, {
    stage(3)
  })
  
  # Reiniciar
  observeEvent(input$reset, {
    stage(1)
    for(n in names(selections)) selections[[n]] <- character(0)
    if(!is.null(input$etapa1)) updateCheckboxGroupInput(session, "etapa1", selected = character(0))
    resultados_zonas(NULL)
  })
  
  output$stage <- reactive({
    stage()
  })
  
  outputOptions(output, "stage", suspendWhenHidden = FALSE)
  
  # Gráfica de telaraña
  output$grafica_final <- renderPlot({
    req(stage() == 4)
    req(!is.null(resultados_zonas()))
    
    resul_zonas <- resultados_zonas()
    
    data2 <- as.data.frame(t(resul_zonas$puntaje))
    colnames(data2) <- c("Superior \nizquierdo","Superior \nderecho",
                         "Inferior \nizquierdo","Inferior \nderecho")
    
    data2 <- rbind(rep(8,4), rep(0,4), data2)
    
    radarchartcirc(data2,
                   pcol = "#2A9DF4",
                   pfcol = adjustcolor("#2A9DF4", 0.1),
                   plwd = 3,
                   cglcol = "grey30",
                   caxislabels = c("0","2","4","6","8"),
                   cglwd = 2,
                   cglty = 1.5,
                   axislabcol = "grey20",
                   maxmin = TRUE,
                   vlcex = 1.2)
  })
  
  # Descarga de los resultados
  output$descargar <- downloadHandler(
    filename = function() {
      # Crear nombre de archivo seguro
      nombre_base <- ifelse(is.null(input$nombre) || input$nombre == "", 
                            "resultado", 
                            input$nombre)
      
      # Limpiar caracteres no válidos
      nombre_limpio <- gsub("[^[:alnum:]._-]", "_", nombre_base)
      paste0(nombre_limpio, "_dominancia_cerebral.csv")
    },
    
    contentType = "text/csv",
    
    content = function(file) {
      req(stage() == 4)
      req(!is.null(resultados_zonas()))
      
      # Obtener resultados
      resul_zonas <- resultados_zonas()
      
      # Crear reporte
      reporte <- data.frame(
        Nombre_aspirante = ifelse(!is.null(input$nombre), input$nombre, ""),
        Edad = ifelse(!is.null(input$edad), input$edad, NA),
        Carrera_deseada = ifelse(!is.null(input$carrera), input$carrera, ""),
        F_nacimiento = ifelse(!is.null(input$f_nacimiento), 
                              as.character(input$f_nacimiento), ""),
        Escuela = ifelse(!is.null(input$escuela), input$escuela, ""),
        Grado = ifelse(!is.null(input$grado), input$grado, ""),
        Correo = ifelse(!is.null(input$correo), input$correo, ""),
        Telefono = ifelse(!is.null(input$telefono), input$telefono, NA),
        Nombre_tutor = ifelse(!is.null(input$tutor), input$tutor, ""),
        Correo_tutor = ifelse(!is.null(input$correo_tutor), input$correo_tutor, ""),
        Telefono_tutor = ifelse(!is.null(input$telefono_tutor), input$telefono_tutor, NA),
        SupIzq = resul_zonas$puntaje[resul_zonas$zona == "supIzq"],
        SupDer = resul_zonas$puntaje[resul_zonas$zona == "supDer"],
        InfIzq = resul_zonas$puntaje[resul_zonas$zona == "infIzq"],
        InfDer = resul_zonas$puntaje[resul_zonas$zona == "infDer"],
        Fecha_prueba = as.character(Sys.Date()),
        Hora_prueba = format(Sys.time(), "%H:%M:%S")
      )
      
      # Escribir archivo
      write.csv(reporte, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
      
    }
  )
  
  #descargar el pdf del resultado
  observeEvent(input$descargar_pdf, {
    req(stage() == 4)
    
    nombre_base <- ifelse(is.null(input$nombre) || input$nombre == "",
                          "resultado",
                          input$nombre)
    
    nombre_limpio <- gsub("[^[:alnum:]._-]", "_", nombre_base)
    
    
    shinyscreenshot::screenshot(
      selector="body", 
      #id=c("nombre","edad","carrera","f_nacimiento","escuela","grado","correo",
      #     "telefono","tutor","correo_tutor","telefono_tutor","summary","grafica_final")
      ,
      scale = 3,
      filename = paste0(nombre_limpio, "_dominancia_cerebral.pdf")
      
    )
    
    
  })
  
  # Progreso
  output$progreso <- renderText({
    etapas <- c(
      "1: Seleccionar 8 de 16 habilidades",
      "2: Elegir tu habilidad principal",
      "3: Seleccionar 1 de las 8 no elegidas",
      "4: Ver resultados finales"
    )
    paste("Etapa", stage(), "de 4 -", etapas[stage()])
  })
  
  # Resumen actual
  output$summary <- renderText({
    text <- ""
    
    if(stage() >= 2 && length(selections$etapa1_labels) > 0) {
      text <- paste0(text, 
                     "Tus habilidades:\n",
                     paste("•", selections$etapa1_labels, collapse = "\n"), "\n\n")
    }
    
    if(stage() >= 3 && nchar(selections$etapa2_label) > 0) {
      text <- paste0(text,
                     "Tu habilidad principal:\n",
                     "• ", selections$etapa2_label, "\n\n")
    }
    
    if(stage() >= 4 && nchar(selections$etapa3_label) > 0) {
      text <- paste0(text,
                     "Tu peor habilidad:\n",
                     "• ", selections$etapa3_label, "\n")
    }
    
    # Agregar resultados de dominancia si están disponibles
    if(stage() == 4 && !is.null(resultados_zonas())) {
      resul_zonas <- resultados_zonas()
      text <- paste0(text,
                     "\n RESULTADOS DE DOMINANCIA CEREBRAL:\n",
                     "• Superior izquierdo: ", resul_zonas$puntaje[resul_zonas$zona == "supIzq"], "\n",
                     "• Superior derecho: ", resul_zonas$puntaje[resul_zonas$zona == "supDer"], "\n",
                     "• Inferior izquierdo: ", resul_zonas$puntaje[resul_zonas$zona == "infIzq"], "\n",
                     "• Inferior derecho: ", resul_zonas$puntaje[resul_zonas$zona == "infDer"])
    }
    
    text
  })
}

shinyApp(ui = ui, server = server)