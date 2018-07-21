library(highcharter)
shinyUI(navbarPage(
  "Tableros",
  tabPanel("Predicción en Fertilidad",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput(
                   "date_range",
                   label = "Rango de Datos",
                   min = "01-01-1960",
                   max = "01-12-2012",
                   start = "06-06-1960",
                   end = "01-01-2012",
                   format = "dd-mm-yyyy",
                   startview = "decade"
                 ),
                 sliderInput(
                   "forecast_n_months",
                   min = 12,
                   max = 12 * 5,
                   value = 48,
                   step = 12,
                   label = "Meses de Predicción"
                 )
               ),
               mainPanel(highchartOutput("forecastPlot"))
             )
           )),
  navbarMenu(
    "Interactivos",
    tabPanel("Histograma Interactivo",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   sliderInput(
                     "no_data",
                     label = "Cantidad de Datos",
                     min = 1000,
                     max = 5000,
                     value = 1000
                   ),
                   sliderInput(
                     "mean",
                     label = "Promedio",
                     min = 0,
                     max = 8,
                     value = 3
                   ),
                   sliderInput(
                     "sd",
                     label = "Desviación Estandard",
                     min = 1,
                     max = 10,
                     value = 2
                   ),
                   uiOutput("xlim_ui")
                 ),
                 mainPanel(highchartOutput("histogram"))
               )
             )),
    
    tabPanel("Muertes por Enfermedades Contagiosas por Estado, USA",
              radioButtons(
                     "enfermedad", "Enfermedades:",
                     c("Hepatitis A" = "Hepatitis A",
                          "Sarampión" = "Measles",
                          "Paperas" = "Mumps",
                          "Tos Ferina" = "Pertussis",
                          "Polio" = "Polio",
                          "Rubéola" = "Rubella",
                          "Viruela"  = "Smallpox"
                       #"Hepatitis A"
                       )),
                   plotOutput(outputId = "Enfermedades_Contagiosas")
                 ),
    tabPanel("Muertes por Enfermedades Contagiosas Totales, USA",
             radioButtons(
               "enfermedad_Total", "Enfermedades:",
               c("Hepatitis A" = "Hepatitis A",
                 "Sarampión" = "Measles",
                 "Paperas" = "Mumps",
                 "Tos Ferina" = "Pertussis",
                 "Polio" = "Polio",
                 "Rubéola" = "Rubella",
                 "Viruela"  = "Smallpox"
                 #"Hepatitis A"
               )),
             plotOutput(outputId = "Enfermedades_Contagiosas_Total")
    ),
             
    tabPanel("Diagrama de Dispersión Interactivo",
             fluidPage(
               highchartOutput("scatterchart")
             )) 
    
    ),
  
  navbarMenu(
    "Machine Learning", 
    tabPanel("Analisis de Sentimientos", 
             fluidPage(
               plotOutput(outputId = "MachineLearning")
             ))
  ),
  navbarMenu(
    "Sondeos",
    tabPanel("Trump Vrs. Clinton 2016", 
             fluidPage(
               plotOutput(outputId = "SondeosPoliticos")
            )),
    
    tabPanel("Muertes por enfermedades contagiosas USA", 
             fluidPage(
               plotOutput(outputId = "MuertesContagios")
             ))
    
  ),
  collapsible = TRUE
))
