library(shiny)

genero_ej4 <- function(n, semilla)
{
  lambda <- 0.25
  set.seed(semilla)
  salida <- rep(0, n)
  for (i in 1:n)
  {
    salida[i] <-round(rexp(1, lambda),2)
  }
  dd <- data.frame(salida)
  colnames(dd) <- c("lamps")
  return(dd)
}

resuelvo_ej4 <- function(datos){
  ans <- list()
  ans$media <- mean(datos)
  ans$varianza <- mean(datos^2)-mean(datos)^2
  ans
}

chequeo <- function(nosotros, ellos){
  if(abs(nosotros-ellos)< 0.001){ans <- "¡Muy bien!"}else{
    ans <- "Intentalo de nuevo"
  } 
  ans
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  salida <- reactive({
    dd <- genero_ej4(input$obs, input$libreta)
  })
  
  
  output$datos <- renderTable({
    salida()
  })
  
  
  # 
  # 
  output$chequeo_media <- renderText({
    dif<-as.numeric(try(abs(input$media-mean(salida()[,1])),TRUE))
    if(is.na(dif)){ans <- "Escribí el resultado"}else{
      if(dif<0.1){ans <- "¡Excelente!"}
      else {ans <- "Intentalo de nuevo"}}
    ans
    
  })
  
  
  
  output$chequeo_varianza <- renderText({
    dif<-as.numeric(try(abs(input$varianza-(mean(salida()[,1]^2)-mean(salida()[,1])^2)),TRUE))
    if(is.na(dif)){ans <- "Escribí el resultado"}else{
      if(dif<0.1){ans <- "¡Excelente!"}
      else {ans <- "Intentalo de nuevo"}}
    ans
  })
  
  
  
  output$media_nos <- renderTable({
    mean(salida()[,1])
  })
  output$varianza_nos <- renderTable({
    mean(salida()[,1]^2)-mean(salida()[,1])^2
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("lamps_id",
            input$libreta,
            "_n_",
            input$obs,
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(
        salida(),
        file,
        row.names = FALSE,
        col.names = TRUE,
        sep = ""
      )
    }
  )
  
  
  
}

ui <-fluidPage(
  # titlePanel("Exercise: Duration of lamps"
  #            # app title/description
  # ),
  h1("Ejercicio: duración de lámparas"),
  fluidPage(
    mainPanel(h5( div(" Querés estudiar la distribución del tiempo de vida (en días) de las lámparas producidas por cierta compañia.
Con este objetivo, seleccionás n lámparas de la producción, las probás y registrás su tiempo de vida. 
Introducí tu número de libreta y un valor de n para obtener tus datos.")))),
  fluidRow(
    column(4, 
           headerPanel(h4("Obteniendo los datos: Elegí un tamaño muestral y escribí tu número de libreta")),
           numericInput("obs", "Tamaño muestral (entre 1 y 1000):", min = 1, max = 1000, value = 50),
           numericInput("libreta", "Número de libreta:", min = 1, max = 100000, value = 24292),
           downloadButton("downloadData", "Descargar"),
    ),
    column(2,  
           headerPanel(h4("TUS datos")),
           tableOutput("datos")
    ),
    column(4,
           headerPanel(h4("Tu turno: Escribí y chequeá las respuestas")),
           numericInput("media", "Media:", min = -100000, max =100000, value = "?"),
           textOutput("chequeo_media"),
           #tableOutput("media_nos"),
           numericInput("varianza", "Varianza:", min = -100000, max =100000, value = "?"),
           textOutput("chequeo_varianza"),
           #tableOutput("mediana_nos")
           
           
           
    ) 
  )
  
    )


shinyApp(ui = ui, server = server)

