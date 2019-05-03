library(shiny) 
library(readr)
library(dplyr)
library(plotly)
library(ggplot2)
require(moments)
require(e1071)


data = read_csv("crimes.csv")

getCrimesTable <- function (data) {
  a <- as.data.frame(table(data$TYPE))
  a <- a %>% group_by(Var1) %>% summarize(sum = sum(Freq)) %>% arrange(desc(sum))
  return(a)
}

getTotalOfCrimes <- function(data) {
  return(count(data))
}

getTotalOfCrimesInYear <- function(year_crime, data) {
  return(count(data %>% filter(YEAR==year_crime)))
}

getModeTotal <- function(data) {
  return(head(data,1))
}

getMedianFromTotal <- function(data){
  return(median(data$sum))
}

getVarianciaTotal <- function(data, media) {
  disvquad = sum((data$sum-media)^2)
  variancia = disvquad/count(data)
  return(disvquad)
}

getAmplitude <- function(data) {
  maximus <- max(data$sum)
  minimum <- min(data$sum)
  return(maximus-minimum)
}

getCoeficienteDeVariancia <- function(desvioPadrao, media) {
  cv <- (desvioPadrao/media)*100
  return(cv)
}

getAssimetria <- function(data) {
  p <- skewness(base[,2])
  hist(base[,2])
  return(p)
} 

getDataBaseCurtose <- function(data) {
  return(base_curtose <- data %>%
    group_by(TYPE,DAY) %>%
    summarise(Curtose = kurtosis(YEAR,
                                 na.rm = TRUE, type = 3))
  )
}

getCurtoses <- function(type, base){
  temp_base <- base%>%filter(as.character(TYPE)==type)
  temp_base$Curtose[is.nan(temp_base$Curtose)] <- 0
  
  return(temp_base$Curtose)
}

dados <- getCrimesTable(data)

myMode <- getModeTotal(dados)
myMedia <- getTotalOfCrimes(data)/14
myMedian <- getMedianFromTotal(dados)

medidasTendenciaCentral = list("moda" = "crime", "media" = "", "median" = "")
medidasTendenciaCentral[["moda"]] <- myMode$Var1 
medidasTendenciaCentral[["media"]] <- paste(round(myMedia, digits = 0), " crimes/ano")
medidasTendenciaCentral[["median"]] <- myMedian

myAmplitude <- getAmplitude(dados)
myVariancia <- getVarianciaTotal(dados, myMedia)
myDesvioPadrao <- getVarianciaTotal(dados, myMedia)/2
myCoeficiente <- getCoeficienteDeVariancia(myDesvioPadrao, myMedia)

myAssimetria <- getAssimetria(data)

print(myCoeficiente$n)

medidasDeDispersao = list("amplitude"="0.0", "variancia"="0.0", "desvio_padrao" = "0.0", "coeficiente_de_variancia"="0.0")
medidasDeDispersao[["amplitude"]] = myAmplitude
medidasDeDispersao[["variancia"]] = myVariancia
medidasDeDispersao[["desvio_padrao"]] = myDesvioPadrao
medidasDeDispersao[["coeficiente_de_variancia"]] = myCoeficiente$n

assimetriaeCurtose = list("assimetria"="0.0")
assimetriaeCurtose[["assimetria"]] = myAssimetria

xData = as.character(dados$Var1)
yData = as.numeric(dados$sum)

base_curtose <- getDataBaseCurtose(data)

breakAndEnterCommercial <- getCurtoses("Break and Enter Commercial", base_curtose)
breakAndEnterResidential <- getCurtoses("Break and Enter Residential/Other", base_curtose)
homicide <- getCurtoses("Homicide", base_curtose)
mischief <- getCurtoses("Mischief", base_curtose)
offenceAgainstaPerson <- getCurtoses("Offence Against a Person", base_curtose)
otherTheft <- getCurtoses("Other Theft", base_curtose)
theftFromVehicle <- getCurtoses("Theft from Vehicle", base_curtose)
theftOfBicycle <- getCurtoses("Theft of Bicycle", base_curtose)
theftOfVehicle <- getCurtoses("Theft of Vehicle", base_curtose)
vehicleCollisionOrPedestrian <- getCurtoses("Vehicle Collision or Pedestrian Struck (with Fatality)", base_curtose)
vehicleCollisionOrPedestrianWithInjury <- getCurtoses("Vehicle Collision or Pedestrian Struck (with Injury)", base_curtose)

dias <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)

dados <- data.frame(
  dias, 
  breakAndEnterCommercial, 
  breakAndEnterResidential, 
  homicide, 
  mischief, 
  offenceAgainstaPerson, 
  otherTheft,
  theftFromVehicle,
  theftOfBicycle,
  theftOfVehicle,
  vehicleCollisionOrPedestrian,
  vehicleCollisionOrPedestrianWithInjury)

print(homicide)

dados$dias <- factor(dados$dias, levels = dias)


shinyServer(
  function(input, output, session)({
    output$table <- renderTable(medidasTendenciaCentral)
    output$table1 <- renderTable(medidasDeDispersao)
    output$table2 <- renderTable(assimetriaeCurtose)
    output$plot1 <- renderPlotly({
      p <- plot_ly(
        x = xData,
        y = yData,
        name = "Nº de Crimes de 2003 a 2017",
        type = "bar"
      )
    })
    output$plot2 <- renderPlotly({
      p1 <- plot_ly(dados, x = ~dias,
                    y = ~breakAndEnterCommercial, name = 'Break and Enter Commercial', type = 'scatter', mode = 'lines',line = list(color = 'rgb(6,138,158 )', width = 1)) %>%
        add_trace(y = ~breakAndEnterResidential, name = 'Break and Enter Residentiall/Other', dash = 'dot', line = list(color = 'rgb(6,138,158 )', width = 1)) %>%
        add_trace(y = ~homicide, name = 'Homicide', line = list(color = 'rgb(216,105,164)', width = 1, mode = 'lines')) %>%
        add_trace(y = ~mischief, name = 'Mischief', line = list(color = 'rgb(44,123,51)', width = 1, mode = 'lines')) %>%
        add_trace(y = ~offenceAgainstaPerson, name = 'Offence Against a Person', line = list(color = 'rgb(29,170,250)', width = 1,mode = 'lines')) %>%
        add_trace(y = ~otherTheft, name = 'Other Theft', line = list(color = 'rgb(237,141,126)', width = 1, mode = 'lines')) %>%
        add_trace(y = ~theftFromVehicle, name = 'Theft from Vehicle', line = list(color = 'rgb(22, 96, 167)', width = 1, mode = 'lines')) %>%
        add_trace(y = ~theftOfBicycle, name = 'Theft of Bicycle', line = list(color = 'rgb(142,20,95)', width = 1, mode = 'lines')) %>%
        add_trace(y = ~theftOfVehicle, name = 'Theft of Vehicle', line = list(color = 'rgb(188,191,35)', width = 1, mode = 'lines')) %>%
        add_trace(y = ~vehicleCollisionOrPedestrian, name = 'Vehicle Collision or Pedestrian (with Fatality)', line = list(color = 'rgb(215,47,3)', width = 1, mode = 'lines')) %>%
        add_trace(y = ~vehicleCollisionOrPedestrianWithInjury, name = 'Vehicle Collision or Pedestrian (with Injury)', line = list(color = 'rgb(17,42,76)', width = 1, mode = 'lines')) %>%
        layout(title = "Crimes por dia e por tipo",
               xaxis = list(title = "Dias do Mês"),
               yaxis = list (title = "Tipos de crime"))
      
    })
  })
  
)
