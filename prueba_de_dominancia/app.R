# ==========================================================
#                   PRUEBA DE DOMINANCIA
# ==========================================================

################################################################################
#LIBRERIAS
################################################################################

library(shiny)
library(fmsb)
library(shinyjs)

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
        h3("Analiza los siguientes 16 elementos laborales listados y marca la 
           casilla de los ocho elementos que consideras Realizar mejor.",
           style = "font-size:16px;"),
        #el nombre del output de esta etapa
        uiOutput("etapa1_ui"),
        #los botones que va a haber
        actionButton("next1", "Continuar parte 2"),
      ),
      
      # Etapa 2: Seleccionar 1 de las 5 anteriores
      conditionalPanel(
        condition = "output.stage == 2",
        h3("De los ocho elementos seleccionados, marca con un circulo aquel que
           te Agrade/guste más hacer", style = "font-size:16px;"),
        uiOutput("etapa2_ui"),  # Cambiado a uiOutput para evitar el error
        br(),
        actionButton("next2", "Continuar parte 3"),
        actionButton("back1", "Volver a la parte 1")
      ),
      
      # Etapa 3: Mostrar las 5 no seleccionadas al inicio
      conditionalPanel(
        condition = "output.stage == 3",
        h3("De los ocho elementos que no seleccionaste, marca aquel elemento que
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
        #h4("Resumen de todas tus selecciones:"),
        #verbatimTextOutput("resumen_final"),
        br(),
        actionButton("back3", "Volver a Etapa 3"),
        actionButton("reset", "Comenzar de nuevo"),
        downloadButton("descargar", "Descargar resultados")
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
      #h4("IDs Internos:"),
      #verbatimTextOutput("valores_internos")
    )
  )
)

################################################################################
#SERVER
################################################################################

server <- function(input, output, session) {
  # Aquí se definen las abreviaciones y el nombre completpo de los elementos
  #laborales que es lo que vera el usuario
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
  
  #tabla para las sumatorias
  data<-as.data.frame(cbind(c(rep("supIzq",4),rep("supDer",4),rep("infIzq",4),rep("infDer",4)),
                            c(supIzq,supDer,infIzq,infDer),
                            rep(0,16)))
  data$V3<-as.numeric(data$V3)
  # Crear opciones combinadas
  #aquie se hace match de los ids con el nombre de las selcciones
  opciones_combinadas <- setNames(checkbox_ids, checkbox_labels[checkbox_ids])
  
  #crea una variable reactiva que almacena un valor (en este caso, el número 1) 
  #y que puede cambiar durante la ejecución de la app, disparando reactividad en 
  #todo lo que dependa de ella.
  stage <- reactiveVal(1)
  #aquí se definen las variables que van a ir definiendo durante la ejecución
  #de la prueba y todas están almacenadas en selections
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
  
  #aquí indicamos que se va inicializar useShinyjs para poder regresar a cambiar
  #respuestas
  shinyjs::useShinyjs()
  
  # UI para etapa 1
  #aqui se activa la primer parte de la prueba 
  #se renderiza la salida de la etapa 1
  #el etapa1_ui es el nombre de la variable donde se guardaran las selecciones
  #de la etapa 1
  output$etapa1_ui <- renderUI({
    checkboxGroupInput(
      #se va a llamar etapa 1 primer grupo de cajas
      "etapa1",
      #una pequeña instruccion
      label = "Selecciona exactamente 8 habilidades:",
      #aqui se indica que las checkbox que aparecen sean todas las opciones que
      #estan guardadas en la variable opciones combinadas
      choices = opciones_combinadas,
      #se indica que no hay nada seleccionado porque el usuario es quien las seleccionara
      selected = character(0)
    )
  })
  
  # Validación etapa 1
  #observe se refiere a que revisa el input del usuario constantemente durante 
  #la etapa 1
  observe({
    #si si se lecciono algo y esto es igual a 8 se habilita el boton de next
    if(!is.null(input$etapa1) && length(input$etapa1) == 8) {
      shinyjs::enable("next1")
      #si no, lo deshabilita
    } else {
      shinyjs::disable("next1")
    }
  })
  
  # Avanzar a etapa 2
  #una vez seleccionados los 8 con observe event se indica que al hacer click en
  #next 1 ejecute el siguiente codigo, practicamente se define
  #en que momento se va a valorar los datos de entrada que será cuando se de enter
  #a next1 el botón y eso desencadenara que se ejecuten las siguientes instrucciones
  observeEvent(input$next1, {
    #si el tamaño de lo que se selecciono en la etapa 1 es igual a 8
    if(length(input$etapa1) == 8) {
      #se guarda en etapa1_ids lo que se selecciono en dicha estapa
      selections$etapa1_ids <- input$etapa1
      #ahora se guarda el nombre completo de los checbox escogidos 
      selections$etapa1_labels <- checkbox_labels[input$etapa1]
      #ahora en la varable de ids no seleccionados guardamos los que no se
      #escogieron 
      selections$no_seleccionadas_ids <- setdiff(checkbox_ids, input$etapa1)
      #lo mismo pero para las etiquetas completas
      selections$no_seleccionadas_labels <- checkbox_labels[selections$no_seleccionadas_ids]
      #se pasa la etapa 2 y se pueda actualizar la pantalla
      stage(2)
    }
  })
  ##############################################################################
  ##############################################################################
  ##############################################################################
  # UI para etapa 2
  #con renderUI se define que se va a modificar lo que vera el usuario al apretar
  #next
  #se va a guardar en la variable etapa2_ui de la etapa 2 lp siguiente
  output$etapa2_ui <- renderUI({
    #si la etapa es la 2 y el tamaño de las selecciones de la etapa 1 es mayor a 0
    if(stage() == 2 && length(selections$etapa1_ids) > 0) {
      #se definen más chckbox
      radioButtons(
        #se define la variable donde se guardara lo que escoja el usuario
        "etapa2_input",
        label = "Selecciona tu habilidad principal:",
        #se definen los checkbox
        choices = setNames(selections$etapa1_ids, selections$etapa1_labels)
      )
    } else {
      p("Cargando opciones...")
    }
  })
  ##############################################################################
  ##############################################################################
  ##############################################################################
  # Avanzar a etapa 3
  #cuando se apriete el next de la etapa 2
  observeEvent(input$next2, {
    #si el input de la etapada dos no es nulo y es mayor a 2 la seleccion
    if(!is.null(input$etapa2_input) && nchar(input$etapa2_input) > 0) {
      #se guarda la selección
      selections$etapa2_id <- input$etapa2_input
      #se guarda los nombres que ve el usuario que estan asociados al id
      selections$etapa2_label <- checkbox_labels[input$etapa2_input]
      #se habilita la etapa 3
      stage(3)
    }
  })
  ##############################################################################
  ##############################################################################
  ##############################################################################
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
      stage(4)
    }
  })
  
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
  #Aqui se define que pasa cuando se presiona el botón de reset
  observeEvent(input$reset, {
    #se regresa a la etapa 1
    stage(1)
    #y para cada uno de los seleccionados se resetea su valor a sin caracter
    for(n in names(selections)) selections[[n]] <- character(0)
    #se resetea las checkbos de la etapa 1
    if(!is.null(input$etapa1)) updateCheckboxGroupInput(session, "etapa1", selected = character(0))
  })
  
  output$stage <- reactive({
    stage()
  })
  outputOptions(output, "stage", suspendWhenHidden = FALSE)
  
  # Resultados finales
  output$resumen_final <- renderText({
    if(stage() == 4) {
      paste(
        "RESUMEN FINAL:\n",
        "=", rep("=", 30), "\n\n",
        
        "ETAPA 1 - 8 Habilidades seleccionadas:\n",
        paste("•", selections$etapa1_labels, collapse = "\n"), "\n\n",
        
        "ETAPA 2 - Habilidad principal:\n",
        paste("•", selections$etapa2_label), "\n\n",
        
        "ETAPA 3 - De las no elegidas:\n",
        paste("•", selections$etapa3_label), "\n\n"
        
        #"IDENTIFICADORES INTERNOS:\n",
        #"Etapa 1: ", paste(selections$etapa1_ids, collapse = ", "), "\n",
        #"Etapa 2: ", selections$etapa2_id, "\n",
        #"Etapa 3: ", selections$etapa3_id
      )
    }
  })
  
  #grafica de telaraña
  output$grafica_final <- renderPlot({
    req(stage() == 4)
    
    #vamos a sumarle a la tabla que contendra las puntuaciones 1 en los ids que 
    #corresponden a lo seleccionado en la etapa 1
    data[which(data$V2 %in% selections$etapa1_ids),3]<-1
    #vamos a sumarle 2 a la tabla en el id que corresponda al seleccionado en la etapa
    #2
    data[which(data$V2 %in% selections$etapa2_id),3]<-data[which(data$V2 %in% selections$etapa2_id),3]+2
    #vamos a restarle 1 a la tabla en el id que corresponda al seleccionado en la etapa
    #3
    data[which(data$V2 %in% selections$etapa3_id),3]<-data[which(data$V2 %in% selections$etapa3_id),3]-1
    #ahora asemos la suma de cada zona de dominancia
    lista_dominancias<-unique(data$V1)
    resul_zonas<-c()
    for (i in 1:length(lista_dominancias)) {
      suma<-sum(data[which(data$V1==lista_dominancias[i]),3])
      temprow<-cbind(lista_dominancias[i],suma)
      resul_zonas<-rbind(resul_zonas,temprow)
    }
    resul_zonas<-as.data.frame(resul_zonas)
    colnames(resul_zonas)<-c("zona","puntaje")
    resul_zonas$puntaje<-as.numeric(resul_zonas$puntaje)
    
    data2 <- as.data.frame(t(resul_zonas$puntaje))
    colnames(data2) <- c("Superior \nizquierdo","Superior \nderecho",
                         "Inferior \nizquierdo","Inferior \nderecho")
    # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    data2 <- rbind(rep(8,1) , rep(0,8) , data2)
    #graficamos
    radarchartcirc(data2,pcol = "#2A9DF4",pfcol = adjustcolor("#2A9DF4", 0.1),plwd = 3,
                   cglcol = "grey30",caxislabels = c("0","2","4","6","8"),
                   cglwd = 2,
                   cglty = 1.5,
                   axislabcol = "grey20",
                   maxmin = TRUE,
                   vlcex = 1.2)
    
    
  })
  
  #la descarga de los resultados
  output$stage <- renderText({
    stage()
  })
  
  outputOptions(output, "stage", suspendWhenHidden = FALSE)
  
  output$descargar <- downloadHandler(
    
    filename = function() {
      paste0(gsub(input$nombre),"_resultado.csv")
    },
    contentType = "text/csv",
    content = function(file) {
      
      reporte <- data.frame(
        Nombre_aspirante = input$nombre,
        Edad = input$edad,
        Carrera_deseada = input$carrera,
        F_nacimiento = input$f_nacimiento,
        Escuela = input$escuela,
        Grado = input$grado,
        Correo = input$correo,
        Telefono = input$telefono,
        Nombre_tutor = input$tutor,
        Correo_tutor = input$correo_tutor,
        Telefono_tutor = input$telefono_tutor,
        SupIzq = resul_zonas$puntaje[1],
        SupDer = resul_zonas$puntaje[2],
        InfIzq = resul_zonas$puntaje[3],
        InfDer = resul_zonas$puntaje[4]
      )
      
      write.csv(reporte, file, row.names = FALSE)
    }
  )
  
  
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
                     "Etapa 1 (8 seleccionadas):\n",
                     paste("•", selections$etapa1_labels, collapse = "\n"), "\n\n")
    }
    
    if(stage() >= 3 && nchar(selections$etapa2_label) > 0) {
      text <- paste0(text,
                     "Etapa 2 (Habilidad principal):\n",
                     "• ", selections$etapa2_label, "\n\n")
    }
    
    if(stage() >= 4 && nchar(selections$etapa3_label) > 0) {
      text <- paste0(text,
                     "Etapa 3 (De las no elegidas):\n",
                     "• ", selections$etapa3_label, "\n")
    }
    
    text
  })
  
  # Valores internos
  #output$valores_internos <- renderText({
  #  text <- ""
  #  
  #  if(stage() >= 2 && length(selections$etapa1_ids) > 0) {
  #    text <- paste0(text, "IDs Etapa 1: ", paste(selections$etapa1_ids, collapse = ", "), "\n")
  #  }
  #  
  #  if(stage() >= 3 && nchar(selections$etapa2_id) > 0) {
  #    text <- paste0(text, "ID Etapa 2: ", selections$etapa2_id, "\n")
  #  }
  #  
  #  if(stage() >= 4 && nchar(selections$etapa3_id) > 0) {
  #    text <- paste0(text, "ID Etapa 3: ", selections$etapa3_id)
  #  }
  
  #  text
  #})
}

shinyApp(ui = ui, server = server)