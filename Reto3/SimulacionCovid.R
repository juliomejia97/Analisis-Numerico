#Modelo SI,SIR,SIS del coronavirus en España
#Cargar los datos del modelo
library (shiny) 
library (shinydashboard) 
library (deSolve)
library (phaseR)
library (pracma)
library(EpiDynamics)

ui <- dashboardPage ( 
  dashboardHeader (), 
  dashboardSidebar (), 
  dashboardBody () 
) 

header <- dashboardHeader(title = "COVID Simulator")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("S.I.", tabName = "SI", icon = icon("circle")    ),
    menuItem("S.I.R.", tabName = "SIR", icon = icon("circle") ),
    menuItem("S.I.S.", tabName = "SIS", icon = icon("circle") )
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

TabSI <- fluidRow( 
  
  box(
    title = "Datos iniciales S.I.", 
    width = 4,
    shinyjs::useShinyjs(),
    (numericInput("poblacionInicialSI", "Población: ", 20, min = 1, max = 46000000)),
    sliderInput("infectadosInicialesSI", "Infectados: ", 1e-6, min = 0, max = 1,step = 1e-6),
    sliderInput("suceptiblesInicialesSI", "Personas suceptibles iniciales:", 300, 10000, 999),
    sliderInput("tiempoLimiteSI", "Tiempo limite:", 5, 20, 10),
    sliderInput("bethaSI", "Betha:", 0.1, 1, 0.2),
    sliderInput("gammaSI", "Gamma:", 1, 10, 5),
    actionButton("botonCalcularSI", "Calcular")
  ),
  tabBox(
    title = "Metodo 1",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficas", height = "300px",
    tabPanel("Modelo 1",
             width= 200,
             plotOutput("plotM1SI", height = 200)
    ),
    tabPanel("Pendientes",
             width = 200,
             plotOutput("plotM1PendSI", height = 250, width = 400)
    ),
    tabPanel("Error",
             width = 200,
             plotOutput("plotM1ErrorSI", height = 250, width = 500)
    )
  ),
  
  tabBox(
    
    title = "Metodo 2",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficasM1SI", height = "300px",
    tabPanel("Modelo",
             width= 200,
             plotOutput("plotM2SI", height = 200)
             
    ),
    tabPanel("Pendientes",
             width = 200,
             plotOutput("plotM2PendSI", height = 250, width = 400)
    ),
    tabPanel("Error",
             width = 200,
             plotOutput("plotM2ErrorSI", height = 250, width = 500)
    )
  )
  
)


TabSIR <- fluidRow( 
  box(
    title = "Datos iniciales S.I.R.", 
    width = 4,
    shinyjs::useShinyjs(),
    numericInput("poblacionInicialSIR", "Población: ", 20, min = 1, max = 46000000),
    sliderInput("infectadosInicialesSIR", "Infectados iniciales:", 1e-6, min = 0, max = 1,step = 1e-6),
    sliderInput("suceptiblesInicialesSIR", "Personas suceptibles iniciales:", 1-1e-6, min = 0, max = 1, step = 1e-6 ),
    sliderInput("tiempoLimiteSIR", "Tiempo limite:", 40, 70, 56),
    sliderInput("bethaSIR", "Betha:", 0.6, 1.5, 1.4247,step = 0.1),
    sliderInput("gammaSIR", "Gamma:", 0, 0.2, 0.14286, step = 0.01),
    actionButton("botonCalcularSIR", "Calcular")
  ),
  tabBox(
    title = "Metodo 1",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficasM1SIR", height = "300px",
    tabPanel("Modelo",
             width= 200,
             plotOutput("plotM1SIR", height = 200)
    ),
    tabPanel("Pendientes",
             width = 200,
             plotOutput("plotM1PendSIR", height = 250, width = 400)
    ),
    tabPanel("Error",
             width = 200,
             plotOutput("plotM1ErrorSIR", height = 250, width = 500)
    )
  ),
  
  tabBox(
    
    title = "Metodo 2",
    width = 8,
    id = "tabGraficasM2SIR", height = "300px",
    tabPanel("Modelo",
             width= 200,
             plotOutput("plotM2SIR", height = 200)
    ),
    tabPanel("Pendientes",
             width = 200,
             plotOutput("plotM2PendSIR", height = 250, width = 400)
    ),
    tabPanel("Error",
             width = 200,
             plotOutput("plotM2ErrorSIR", height = 250, width = 500)
    )
  )
)
TabSIS <- fluidRow( 
  box(
    title = "Datos iniciales S.I.S.", 
    width = 4,
    shinyjs::useShinyjs(),
    numericInput("poblacionInicialSIS", "Población: ", 20, min = 1, max = 46000000),
    sliderInput("infectadosInicialesSIS", "Infectados iniciales:", 1e-6, min = 0, max = 1,step = 1e-6),
    sliderInput("suceptiblesInicialesSIS", "Personas suceptibles iniciales:", 1-1e-6, min = 0, max = 1, step = 1e-6 ),
    sliderInput("tiempoLimiteSIR", "Tiempo limite:", 40, 70, 56),
    sliderInput("bethaSIR", "Betha:", 0.6, 1.5, 1.4247,step = 0.1),
    sliderInput("gammaSIR", "Gamma:", 0, 0.2, 0.14286, step = 0.01),
    actionButton("botonCalcularSIS", "Calcular")
  ),
  tabBox(
    title = "Metodo 1",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficasM1SIS", height = "300px",
    tabPanel("Modelo",
             width= 200,
             plotOutput("plotM1SIS", height = 200)
    ),
    tabPanel("Pendientes",
             width = 200,
             plotOutput("plotM1PendSIS", height = 250, width = 400)
    ),
    tabPanel("Error",
             width = 200,
             plotOutput("plotM1ErrorSIS", height = 250, width = 500)
    )
  ),
  
  tabBox(
    
    title = "Metodo 2",
    width = 8,
    id = "tabGraficasM2SIS", height = "300px",
    tabPanel("Modelo",
             width= 200,
             plotOutput("plotM2SIS", height = 200)
    ),
    tabPanel("Pendientes",
             width = 200,
             plotOutput("plotM2PendSIS", height = 250, width = 400)
    ),
    tabPanel("Error",
             width = 200,
             plotOutput("plotM2ErrorSIS", height = 250, width = 500)
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("SI",frow1, TabSI),
    tabItem("SIR",TabSIR),
    tabItem("SIS",TabSIS)
  )
  
)

ui <- dashboardPage(title = 'Analisis Numerico', header, sidebar, body, skin='blue')

server <- function (input, output,session) 
{
  CalcularSI <- function(){
    output$plotM1SI <- renderPlot({
      #Tamaño Población
      N = input$poblacionInicialSI
      #Tiempo simulación
      tf = input$tiempoLimiteSI
      
      #estado inicial de los compartimentos
      init <- c(S = input$suceptiblesInicialesSI , # Suceptibles ->>  input$suceptiblesInicialesSI
                I = input$infectadosInicialesSI)  # Infectados ->>  input$infectadosInicialesSI
      
      #parametros del modelo (coeficientes de las variables)
      param <- c( beta = input$bethaSI , # Tasa de infeccion ( contagiados por unidad de tiempo) input$bethaSI
                  gamma = input$gammaSI ) # Probabilidad de transmitir la infeccion  input$gammaSI
      #crear la funcion con las ODE
      si <- function(times, init, param) 
      {
        with(as.list(c(init, param)), 
             {
               #ecuaciones diferenciales   
               dS <- - gamma*beta*S*I/(S+I)
               dI <- + gamma*beta*S*I/(S+I)
               #resultados de las tasas de cambio    
               return(list(c(dS, dI)))
             })
      }
      
      #intervalo de tiempo y resolucion
      times <- seq(0, tf, by = 1)
      #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
      simulacionM1SI.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "rk4"))
      attach(simulacionM1SI.si)
      #Calculamos el tamanio de la poblacion
      N <- sum(init)
      updateTextInput(session, "poblacionInicialSI", value =N)
      
      #Representamos graficamente los resultados obtenidos
      plot(times, S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en horas)", ylab="Numero de Personas",main = "Metodo 1: Runge Kutta 4")
      lines(times, I, type="l", col="red")
      legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2))
    })
  }
    observeEvent(input$botonCalcularSI,{
        CalcularSI() 
    })
}
shinyApp (ui,server)