library(WDI)
library(highcharter)
library(forecast)
library(lubridate)
library(slam)
library(sentiment)
library(twitteR)
library(RColorBrewer)
library(ggplot2)
library(dslabs)
library(tidyverse)

#Funcion para limpiar los datos


clean.data = function(text) {
  
  # delete re-tweet entries
  text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
  # remove @ word
  text = gsub("@\\w+", "", text)
  # delete punctuation
  text = gsub("[[:punct:]]", "", text )
  # remove Digits: 0 1 2 3 4 5 6 7 8 9
  text = gsub("[[:digit:]]", "", text)
  # delete html links
  text = gsub("http\\w+", "", text)
  # delete unnecessary spaces like tab and space
  text = gsub("[ \t]{2,}", "", text)
  text = gsub("^\\s+|\\s+$", "", text)
  
  return(text)
}

#Funcion para manejo de errores


handle.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

#Conexion a Twitter
api_key <- "xgBgAyuBo8Fpmpj9n1W47jQEW"
api_secret <- "0dc8sIs1ExWltVnXC20j5Y225Qa81tS4x4oyj1fBiicOkEHSEC"
access_token <- "1127738809-i11uTmZrRHRjHMQk5MhIG1nWloX9zDc7GKal0bA"
access_token_secret <- "Oy7AJ0DcQqT8wprLIzLw2aMBL9aouXvSxOd5JiMBpOPtV"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


### Importando Mensajes los ultimos 500

MachineLearning_tweets = searchTwitter("machinelearning", n=500, lang="en") 
MachineLearning_text = sapply(MachineLearning_tweets, function(x) x$getText())

###Limpiando los datos

MachineLearning_text= clean.data(MachineLearning_text)

####Calculando y agregando los datos

MachineLearning_text = sapply(MachineLearning_text, handle.error)
MachineLearning_text = MachineLearning_text [!is.na(MachineLearning_text)]
names(MachineLearning_text) = NULL
class_emo = classify_emotion(MachineLearning_text, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown" 

class_pol = classify_polarity(MachineLearning_text, algorithm="bayes")

polarity = class_pol[,4]
sent_df = data.frame(text= MachineLearning_text, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))



shinyServer(function(input, output) {
  output$xlim_ui <- renderUI({
    if (is.null(input$mean)) {
      return()
    }
    sliderInput(
      "xlim",
      label = "xlim",
      min = input$mean,
      max = 10,
      value = input$mean,
      step = 1
    )
  })
  
  histogram_data <- reactive({
    rnorm(input$no_data, mean = input$mean, sd = input$sd)
  })
  
  output$histogram <- renderHighchart({
    hchart(histogram_data(), name = "Measure") %>%
      hc_title(text = "Histograma Interactivo") %>%
      hc_tooltip(valuePrefix = "foobar", valueSuffix = 'USD')
  })
  wdi_time_series <- reactive({
    start_year <- year(as.POSIXct(input$date_range[1]))
    end_year <- year(as.POSIXct(input$date_range[2]))
    
    wdi_data <- WDI(
      country = "US",
      indicator = "SP.DYN.TFRT.IN",
      start = start_year,
      end = end_year
    )
    ts(
      data = wdi_data$SP.DYN.TFRT.IN,
      start = start_year,
      end = end_year,
      frequency = 1
    )
  })
  
  output$forecastPlot <- renderHighchart({
    forecast(ets(wdi_time_series()),
             h = input$forecast_n_months / 12,
             level = 95) %>% hchart %>%
      hc_tooltip(
        valuePrefix = "Hubo  ",
        valueSuffix = " nacimientos por mujer",
        valueDecimals = 2
      ) %>%
      hc_title(text = "Pronóstico de fertilidad para los EE. UU.")
  })
  
  output$scatterchart <- renderHighchart({
    highchart() %>% 
      hc_title(text = "Diagrama de dispersión con tamaño y color") %>% 
      hc_add_series_scatter(iris$Sepal.Length, iris$Sepal.Width,
                            iris$Petal.Width, iris$Species)
  })
  
  
  
  output$MachineLearning <- renderPlot({
    
    ggplot(sent_df, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) +scale_fill_brewer(palette="Set3") + labs(x="categorías de polaridad", y="número de tweets") + labs(title = "Análisis de Sentimiento a través de Tweets")
  }
    
  )
  
  output$SondeosPoliticos <- renderPlot({
    
      polls_us_election_2016 %>% 
      filter(state == "U.S." & enddate >= "2016-07-01") %>% 
      select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
      rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>% 
      gather(candidate, percentage, -enddate, -pollster) %>%
      mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>% 
      group_by(pollster) %>% 
      filter(n()>= 10) %>% 
      ungroup() %>% 
      ggplot(aes(enddate, percentage, color = candidate)) +
      geom_point(show.legend = FALSE, alpha = 0.4) + 
      geom_smooth(method = "loess", span = 0.15) +
      scale_y_continuous(limits = c(30,50))
    
    
  }
    
  )
 
  output$Enfermedades_Contagiosas <- renderPlot({
    
    the_disease <- switch(input$enfermedad,
                          "Hepatitis A" = "Hepatitis A",
                          "Measles"="Measles",
                          "Mumps"="Mumps",
                          "Pertussis"="Pertussis",
                          "Polio"="Polio",
                          "Rubella"="Rubella",
                          "Smallpox"="Smallpox"	)
                          
      
    
    dat <- us_contagious_diseases %>% 
      filter(!state%in%c("Hawaii","Alaska") & disease == the_disease ) %>% 
      mutate(rate = count / population * 10000) %>% 
      mutate(state = reorder(state, rate))
    
    dat %>% ggplot(aes(year, state, fill = rate)) + 
      geom_tile(color = "grey50") + 
      scale_x_continuous(expand=c(0,0)) + 
      scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
      theme_minimal() + 
      theme(panel.grid = element_blank()) + 
      ggtitle(the_disease) + 
      ylab("") + 
      xlab("") 
    
  })
  
  output$Enfermedades_Contagiosas_Total <- renderPlot({
    the_disease <- switch(input$enfermedad_Total,
                          "Hepatitis A" = "Hepatitis A",
                          "Measles"="Measles",
                          "Mumps"="Mumps",
                          "Pertussis"="Pertussis",
                          "Polio"="Polio",
                          "Rubella"="Rubella",
                          "Smallpox"="Smallpox"	)
    
    
    dat <- us_contagious_diseases %>%
      filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>%
      mutate(rate = count / population * 10000) %>%
      mutate(state = reorder(state, rate))
    
    avg <- us_contagious_diseases %>%
      filter(disease==the_disease) %>% group_by(year) %>%
      summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)
    
    dat %>% ggplot() +
      geom_line(aes(year, rate, group = state),  color = "grey50", 
                show.legend = FALSE, alpha = 0.2, size = 1) +
      geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
      scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
      ggtitle("No. de contagios por cada 10,000 habitantes por estado, en 1963 obligatorio vacunarse") + 
      xlab("") + 
      ylab("") +
      geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
      geom_vline(xintercept=1963, col = "blue")
    
    })
  
  output$MuertesContagios <- renderPlot({
    
    us_contagious_diseases %>%  
         group_by(year, disease) %>%
         summarize(rate = sum(count)/sum(!is.na(population))*10000) %>%
         ggplot(aes(year, rate, color = disease)) + 
         geom_line() + 
         geom_vline(xintercept=1963, col = "blue")
  })
  
  }
)
