#Modelo SI,SIR,SIS del coronavirus en España
#Cargar los datos del modelo
library (shiny) 
library (shinydashboard) 
library (deSolve)
library (phaseR)
library (pracma)
library(readr)
Datos <- read_delim("Analisis-Numerico/Reto3/Datos.csv", 
                    ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                    trim_ws = TRUE)

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
    (numericInput("poblacionInicialSI", "Población: ", 20, min = 1, max = 466)),
    sliderInput("infectadosInicialesSI", "Infectados: ", 0, min = 0, max = 0.2,step = 0.001),
    sliderInput("suceptiblesInicialesSI", "Personas suceptibles iniciales:", 0, 300, 5),
    sliderInput("tiempoLimiteSI", "Tiempo limite:", 30, 107, 5),
    sliderInput("bethaSI", "Betha:", 0, 0.92, 1e-3),
    sliderInput("gammaSI", "Gamma:",  0, 0.95, 1e-3),
    actionButton("botonCalcularSI", "Calcular")
  ),
  tabBox(
    title = "Metodo 1",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficas", height = "400px",
    tabPanel("Modelo 1",
             width= 350,
             plotOutput("plotM1SI", height = 350)
    ),
    tabPanel("Pendientes",
             width = 350,
             plotOutput("plotM1PendSI", height = 250, width = 400)
    ),
    tabPanel("Error",
             width = 350,
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
    numericInput("poblacionInicialSIR", "Población: ", 20, min = 1, max = 466),
    sliderInput("infectadosInicialesSIR", "Infectados: ", 1, min = 0, max = 10,step = 1),
    sliderInput("suceptiblesInicialesSIR", "Personas suceptibles iniciales:", 0, 88, 5),
    sliderInput("tiempoLimiteSIR", "Tiempo limite:", 30, 107, 5),
    sliderInput("bethaSIR", "Betha:", 0, 0.092, 1e-3),
    sliderInput("gammaSIR","Gamma:",  0, 0.095, 1e-3),
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
    numericInput("poblacionInicialSIS", "Población: ", 20, min = 1, max = 466),
    sliderInput("infectadosInicialesSIS", "Infectados: ", 1, min = 0, max = 10,step = 1),
    sliderInput("suceptiblesInicialesSIS", "Personas suceptibles iniciales:", 0, 88, 5),
    sliderInput("tiempoLimiteSIS", "Tiempo limite:", 30, 107, 5),
    sliderInput("bethaSIS", "Betha:", 0, 0.092, 1e-3),
    sliderInput("gammaSIS","Gamma:",  0, 0.095, 1e-3),
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
    tabItem("SIS", TabSIS)
  )
  
)

ui <- dashboardPage(title = 'Analisis Numerico', header, sidebar, body, skin='blue')

server <- function (input, output,session) 
{
  CalcularSI <- function(){
    #Tamaño Población
    N = input$poblacionInicialSI
    #Tiempo simulación
    tf = input$tiempoLimiteSI
    #estado inicial de los compartimentos
    init <- c(S = input$suceptiblesInicialesSI , # Suceptibles ->>  input$suceptiblesInicialesSI
              I = input$infectadosInicialesSI)  # Infectados ->>  input$infectadosInicialesSI
    
    print(input$infectadosInicialesSI)
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
    errorM1SI <- c()
    output$plotM1SI <- renderPlot({
      N <- sum(init)
      #intervalo de tiempo y resolucion
      times <- seq(0, tf, by = 1)
      #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
      simulacionM1SI.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "rk4"))
      attach(simulacionM1SI.si)
      #Calculamos el tamanio de la poblacion
      updateTextInput(session, "poblacionInicialSI", value =N)
      #Representamos graficamente los resultados obtenidos
      plot(times, S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en dias)", ylab="Numero de Personas",main = "Metodo 1: Runge Kutta 4")
      lines(times, I, type="l", col="red")
      legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2))
      
    })
    output$plotM1PendSI <- renderPlot({
      scopeField <- function(t, p, parameters){
        k <- parameters[1]
        n <- parameters[2]
        dp <- k*(p*(n-p))
        list(dp)
      }
      scopeField.flowField <- flowField(scopeField, xlim = c(0,input$tiempoLimiteSI),
                                        ylim = c(0,input$suceptiblesInicialesSI), parameters = c(0.3,1),
                                        system = "one.dim",
                                        add = FALSE, xlab = "Tiempo (en dias)", ylab = "Numero de personas", 
                                        main = "Campo de pendientes")
      
    })
    output$plotM1ErrorSI <- renderPlot({
      
      for(i in 1:input$tiempoLimiteSI){
        errorM1SI<- c(errorM1SI,(abs(Datos$`Cum. cases`[i]- simulacionM1SI.si$I[i])/Datos$`Cum. cases`[i])/100)
      }
      print(errorM1SI)
      plot(times, errorM1SI, type="l", col="blue", ylim=c(0,4), xlab="Tiempo (en dias)", ylab="Error Relativo",main = "Metodo 1: Runge Kutta 4")
      
    })
    
  }
    observeEvent(input$botonCalcularSI,{
        CalcularSI() 
    })
}
shinyApp (ui,server)