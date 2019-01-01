library(shiny)
library("retistruct")
library("raster")
library("spatstat")
source('C:/R/Drones/new version/functions.R')

shinyServer(function(input, output) {
  HTx <- 60
  Lmin <- 0
  Lmax <- 20
  R <- 100
  NumberOfIteration <- 8
  APPoint <- data.frame(x = 50, y = 50)
  HBuild <- 30
  rv <- reactive( as.integer( input$n ) )
  # runif(input$n, 0, 1)
  observe({
    n <<- rv()
    print(runif(n, 0, 1))
  })
  print(n)
  # output$TestText <- renderText({
  #   paste("n = ", value$n)
  # })
  
})