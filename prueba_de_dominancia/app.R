# ==========================================================
#                   PRUEBA DE DOMINANCIA
# ==========================================================

#librerias necesarias
library(shiny)
library(shinyjs)

#definicion de variables
# Lista de todos los checkbox (sus inputId)
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
# ----------------------------------------------------------
# UI
# ----------------------------------------------------------

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
  
  # Application title
  titlePanel("PRUEBA DE DOMINANCIA"),
  
  #encabezados previos a los datos personales
  h2("Al día de hoy, ¿qué parte del cerebro estas utilizando más",
     style = "font-size:16px;"),
  h2("De acuerdo a esa parte de tu cerebro, ¿qué carrera UCQ te hará más exitoso?",
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
  
  #Mensaje de que se contesten las preguntas
  h2("Instrucciones"),
  h2("Analiza los siguientes 16 elementos laborales listados y marca la casilla 
     de los ocho elementos que consideras Realizar mejor.",
     style = "font-size:16px;"),
  
  #Aqui se contesta la primer ronda de preguntas
  fluidRow(
    column(4,
           checkboxInput("ana", "Análisis", FALSE),
           checkboxInput("admin", "Administración", FALSE),
           checkboxInput("concep", "Conceptualización", FALSE),
           checkboxInput("expre", "Expresión de ideas", FALSE)
    ),
    column(4,
           checkboxInput("inte", "Integración", FALSE),
           checkboxInput("redac", "Redacción", FALSE),
           checkboxInput("aspt", "Aspectos técnicos", FALSE),
           checkboxInput("imp", "Implantación", FALSE)
    ),
    column(4,
           checkboxInput("plan", "Planeación", FALSE),
           checkboxInput("aspi", "Aspectos interpersonales", FALSE),
           checkboxInput("res", "Resolución de problemas lógicos", FALSE),
           checkboxInput("ino", "Innovación", FALSE)
    )),
  fluidRow(
    column(4, checkboxInput("entf", "Entrenamiento / Formación", FALSE)),
    column(4, checkboxInput("org", "Organización", FALSE)),
    column(4, checkboxInput("aspc", "Aspectos creativos", FALSE))
  ),
  fluidRow(
    column(4, checkboxInput("aspf", "Aspectos financieros", FALSE))
  ),
  
  hr(),
  #pasamos a la segunda seccion de la prueba de dominancia
  uiOutput("segunda_ronda")
  
  
)


# Define server logic 
server <- function(input, output, session) {
  
  observe({
    
    seleccionados <- names(which(unlist(lapply(checkbox_ids, function(id) input[[id]])) == TRUE))
    
    if (length(seleccionados) == 8) {
      
      output$segunda_ronda <- renderUI({
        tagList(
          fluidRow(
            h2("Segunda parte")
          ),
          
          fluidRow(
            lapply(seleccionados, function(id) {
              column(
                width = 4,
                checkboxInput(
                  inputId = paste0("r2_", id),
                  label   = id
                )
              )
            })
          )
        )
      })
      
    } else {
      output$segunda_ronda <- renderUI(NULL)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

