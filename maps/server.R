library(shiny)
library("retistruct")
library("raster")
library("spatstat")
library('RJSONIO')
library('DT')
library('network')
library('dichromat')
library('sp')
source('functions.R')

# Функция shinyServer() определяет логику отображения элементов
# графического интерфейса и обновления значений различных переменных:
shinyServer(function(input, output) {
  observeEvent(input$Simulation, {
    react_n <- reactive(as.integer(input$n))
    react_NumberOfIteraion <- reactive(as.integer(input$NumberOfIteration))
    react_HTxMin <- reactive(as.double(input$HTxMin))
    react_HTxMax <- reactive(as.double(input$HTxMax))
    react_APPointX <- reactive(as.double(input$APPointX))
    react_APPointY <- reactive(as.double(input$APPointY))
    react_gridSizeX <- reactive(as.double(input$gridSizeX))
    react_gridSizeY <- reactive(as.double(input$gridSizeY))
    react_dataFromJson <- reactive(fromJSON(input$file$datapath))
    react_R <- reactive(as.double(input$R))
    # глобалы: 
    # R - дальность действия передатчика
    # NumberOfIteration - количество итераций
    # APPoint - координаты для начальной точки {x, y}
    # n - количество разыгрываемых точек для UP
    # HTxMin - минимальная высота дрона
    # HTxMax - максимальная высота дрона
    # gridSize - размер сетки
    # coordinatesForUP - фрейм для хранения координат пользовательских устройств
    # с указанием высоты, флагом (FALSE по умолчанию) и координатами точки пересечения
    # coordinatesForUPAtStart - то же, что и coordinatesForUP, только в самый первый момент. Т.е.
    # присутствуют только координаты, остальное - NA 
    # coordinatesForUPWithTrueFlag - тоже самое, что и (выше), но с флагом = TRUE (заблокирован)
    # BuildNumber - количество зданий
    # buildingInfo - информационный фрейм об объектах
    # pDisconnect - вероятность дисконнекта
    # pConnect - вероятность los
    

    n <- react_n()
    HTxMin <- react_HTxMin()
    HTxMax <- react_HTxMax()
    R <- react_R()
    NumberOfIteration <- react_NumberOfIteraion()
    APPoint <- data.frame(x = react_APPointX(), y = react_APPointY())
    gridSize <- data.frame(x = react_gridSizeX(), y = react_gridSizeY())
    dataFromJson <- react_dataFromJson()
    
    coordinatesForBuildings <- data.frame("Level" = array(NA, 1), "Number" = array(NA, 1), 
                                          "x" = array(NA, 1), "y" = array(NA, 1), H = NA, L = array(NA, 1))
    
    coordinatesForUP <- data.frame("x" = array(NA, n), "y" = array(NA, n),
                                   "H" = array(2, n), "flag" = array(FALSE, n), "xInter" = array(NA, n),
                                   "yInter" = array(NA, n), "l" = array(NA, n), 
                                   "l1" = array(NA, n), "l2" = array(NA, n))
    coordinatesForUPAtStart <- coordinatesForUP
    pDisconnect <- array(NA, NumberOfIteration)
    pConnect <- array(NA, NumberOfIteration)
    
    observe({
      isolate({
        # основная программа
        # подгрузка фрейма со зданиями из JSON
        # coordinatesForBuildings <- PointsForBuildingFromJson(dataFromJson, coordinatesForBuildings)
        # подгрузка фрейма с координатами пользователей
        # coordinatesForUP <- PointsForUP(coordinatesForUP, n, coordinatesForBuildings)
        # количество зданий
        # countOfBuildings <- coordinatesForBuildings$Number[which.max(coordinatesForBuildings$Number)]
        # высота HTx
        # HTx <- seq(HTxMin, HTxMax, NumberOfIteration)
        # for (k in 1:NumberOfIteration) {
          # coordinatesForUP <- coordinatesForUPAtStart
          # координаты для UP с флагом true
          # for (i in 1:n) {
            # for (j in 1:BuildNumber) {
              # coordinatesForUP <- VerificationUP(i, j, APPoint, coordinatesForUP, buildingInfo)
            # }
          # }
          # для каждой заблокированной точки вычисляем H и исправляем флаг, если нужно
          # for (i in 1:n) {
            # if ( (coordinatesForUP$flag[i] == TRUE) && IsNotLOS(i, HTx[k], coordinatesForUP, HBuild) == FALSE) {
        #       coordinatesForUP$flag[i] <- FALSE
        #     }
        #     if (IsLOSWithoutBuilding(i, HTx[k], coordinatesForUP, R) == TRUE) {
        #       coordinatesForUP$flag[i] <- TRUE
        #     }
        #   }
        #   pDisconnect[k] <- length(coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,1])/n
        #   pConnect[k] <- 1 - pDisconnect[k]
        # }
        # 
        # # oX <- c(2:8)
        # # oX <- oX * 10^-420
        # # sm <- smooth.spline(oX, pConnect[2:8], spar = 0.35)
        # # plot(sm, type = 'l', ylab = "connection probability", xlab = "building density")
        # #
        # # plot(0,0,xlim = c(0,100), ylim = c(0,100), col = "white")
        # # for (i in 1:BuildNumber) {
        # #   lines(x = c(buildingInfo$x1[i], buildingInfo$x2[i]), y = c(buildingInfo$y1[i], buildingInfo$y2[i]))
        # # }
        # # for (i in 1:n) {
        # #   points(coordinatesForUP$x[coordinatesForUP[,"flag"] == TRUE],
        # #          coordinatesForUP$y[coordinatesForUP[,"flag"] == TRUE], pch = 19, col = "red")
        # #   points(coordinatesForUP$x[coordinatesForUP[,"flag"] == FALSE],
        # #          coordinatesForUP$y[coordinatesForUP[,"flag"] == FALSE], pch = 19, col = "green")
        # #   points(APPoint$x, APPoint$y, pch = 19, col = "black")
        # # }
        output$EndPlot <- renderPlot({
          plot(0, 0, xlim = c( (coordinatesForBuildings$x[which.min(coordinatesForBuildings$x)]), 
                               coordinatesForBuildings$x[which.max(coordinatesForBuildings$x)]), 
               ylim = c( (coordinatesForBuildings$y[which.min(coordinatesForBuildings$y)]),
                         coordinatesForBuildings$y[which.max(coordinatesForBuildings$y)]) )
          for (i in 1:coordinatesForBuildings$Number[which.max(coordinatesForBuildings$Number)]) {
            lines(x = coordinatesForBuildings$x[which(coordinatesForBuildings$Number == i)],
                  y = coordinatesForBuildings$y[which(coordinatesForBuildings$Number == i)])
          }
          points(coordinatesForUP$x, coordinatesForUP$y)
        })
      })
    })
  })
})