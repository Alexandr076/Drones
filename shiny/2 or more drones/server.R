library(shiny)
library("retistruct")
library("raster")
library("spatstat")
source('functions.R')

# Функция shinyServer() определяет логику отображения элементов
# графического интерфейса и обновления значений различных переменных:
shinyServer(function(input, output) {
  observeEvent(input$Simulation, {
    react_n <- reactive(as.integer(input$n))
    react_NumberOfIteraion <- reactive(as.integer(input$NumberOfIteration))
    react_HTx <- reactive(as.double(input$HTx))
    react_HBuild <- reactive(as.double(input$HBuild))
    react_gridSizeX <- reactive(as.double(input$gridSizeX))
    react_gridSizeY <- reactive(as.double(input$gridSizeY))
    react_lmbda <- reactive(as.double(input$lmbda))
    react_R <- reactive(as.double(input$R))
    # глобалы: 
    # Lmin - минимально возможная длина здания
    # Lmax - максимально возможная длина здания
    # R - дальность действия передатчика
    # NumberOfIteration - количество итераций
    # APPointAll - координаты для начальных точкек {x, y}. Задаются в файле Coordinates.csv
    # HBuild - высота (для зданий и UP)
    # n - количество разыгрываемых точек для UP
    # HTxMin - минимальная высота дрона
    # HTxMax - максимальная высота дрона
    # gridSize - размер сетки
    # coordinatesForUP - фрейм для хранения координат пользовательских устройств
    # с указанием высоты, флагом (FALSE по умолчанию) и координатами точки пересечения
    # coordinatesForUPAtStart - то же, что и coordinatesForUP, только в самый первый момент. Т.е.
    # присутствуют только координаты, остальное - NA 
    # lmbd - простность для распределения зданий 
    # coreOfCoordinates - центральная координата для зданий
    # coordinatesForUPWithTrueFlag - тоже самое, что и (выше), но с флагом = TRUE (заблокирован)
    # BuildNumber - количество зданий
    # buildingInfo - информационный фрейм об объектах
    # pDisconnect - вероятность дисконнекта
    # pConnect - вероятность los
    # HTx - высотка передатчика
    # angle - угол в плоскости YoZ между Tx и Rx
    # D - расстояние между Tx и Rx в плоскости XoY
    
    # CoordinatesForAPFromFile <- input$file
    n <<- react_n()
    HTx <- react_HTx()
    Lmin <- 0
    Lmax <- 20
    R <- react_R()
    NumberOfIteration <<- react_NumberOfIteraion()
    APPointAll <<- read.csv(input$file$datapath,
                         header = TRUE,
                         sep = ",")
    HBuild <- react_HBuild()
    HTxMin <- 30
    HTxMax <- 30
    gridSize <<- data.frame(x = react_gridSizeX(), y = react_gridSizeY())
    coordinatesForUPCache <<- data.frame(x = runif(n, 0, gridSize$x), y = runif(n, 0, gridSize$y),
                                        H = array(2, n), flag = array(FALSE, n), xInter = array(NA, n),
                                        yInter = array(NA, n), l = array(NA, n), 
                                        l1 = array(NA, n), l2 = array(NA, n))
    coordinatesForUPAll <<- coordinatesForUPCache
    lmbd <<- react_lmbda()
    while (TRUE) {
      coreOfCoordinates <- rpoispp(lmbd, win = owin(c(0,gridSize$x), c(0,gridSize$y)))
      BuildNumber <<- coreOfCoordinates$n
      if (BuildNumber > 1) {break}
    }
    buildingInfo <<- data.frame(x1 = array(NA, BuildNumber), y1 = array(NA, BuildNumber), x2 = array(NA, BuildNumber),
                                y2 = array(NA, BuildNumber), H = HBuild, L = array(NA, BuildNumber), angle = array(NA, BuildNumber))
    # инициация постройки зданий с общей информацией
    buildingInfo <<- PointsForBuilding(lmbd, Lmin, Lmax, buildingInfo, BuildNumber, coreOfCoordinates)
    pDisconnect <- array(NA, length(APPointAll[,1]))
    pConnect <<- array(NA, length(APPointAll[,1]))
    
    
    observe({
      isolate({
        withProgress(message = 'Simulation progress', value = 0, {
          # основная программа
          for (k in 1:length(APPointAll[,1])) {
            coordinatesForUP <- coordinatesForUPCache
            n <- length(coordinatesForUP[,1])
            APPoint <<- APPointAll[k,]
            HTx <- array(HTx, dim = length(APPointAll[,1])) # нужно нормально запилить, а не массивом (я верю в себя: лень
              # не может быть так сильна!)
            
            for (i in 1:n) {
              for (j in 1:BuildNumber) {
                coordinatesForUP <- VerificationUP(i, j, APPoint, coordinatesForUP, buildingInfo)
              }
            }
            
            # координаты для UP с флагом true
            # coordinatesForUPWithTrueFlag <- coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,]
            # для каждой заблокированной точки вычисляем H и усправляем флаг, если нужно
            for (i in 1:n) {
              if ( (coordinatesForUP$flag[i] == TRUE) && IsNotLOS(i, HTx[k], coordinatesForUP, HBuild) == FALSE) {
                coordinatesForUP$flag[i] <- FALSE
              }
              # тут нужно добавить флаг, ибо еще подбираю зайцев с флагом true
              if (IsLOSWithoutBuilding(i, HTx[k], coordinatesForUP, R) == TRUE) {
                coordinatesForUP$flag[i] <- TRUE
              }
            }
            pDisconnect[k] <- length(coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,1])/length(coordinatesForUPAll[,1])
            pConnect[k] <<- 1 - pDisconnect[k]
            incProgress(1/k, detail = paste("Doing iteration", k))
            coordinatesForUPCache <<- coordinatesForUP[which(coordinatesForUP$flag == TRUE), ]
            if (length(coordinatesForUPCache[,1]) == 0) {
              coordinatesForUPBlock <<- data.frame(x = -100, y = -100)
              break
            }
            if (k == length(APPointAll[,1])) {
              coordinatesForUPBlock <<- coordinatesForUPCache[which(coordinatesForUPCache$flag == TRUE), ]
            }
            coordinatesForUPCache <<- within(coordinatesForUPCache, {flag[] <- FALSE})
          }
        })
      })
    })
  })
  observeEvent(input$FigureOne, {
    output$Figure <- renderPlot({
      oX <- c(1:length(APPointAll[,1]))
      sm <- smooth.spline(oX, pConnect[1:length(APPointAll[,1])], spar = 0)
      plot(sm, type = 'l', ylab = "Вероятность подключения UP к AP", xlab = "Количество AP")
      title("Зависимость возможности подключения от количества AP")
    })
  })
  observeEvent(input$FigureTwo, {
    output$Figure <- renderPlot({
      plot(0, 0, xlim = c(0, gridSize$x), ylim = c(0,gridSize$y), col = "white")
      for (i in 1:BuildNumber) {
        lines(x = c(buildingInfo$x1[i], buildingInfo$x2[i]), y = c(buildingInfo$y1[i], buildingInfo$y2[i]))
      }
      points(coordinatesForUPAll$x, coordinatesForUPAll$y, pch = 19, col = "green")
      if (length(coordinatesForUPBlock[,1] != 0)) {
        points(coordinatesForUPBlock$x, coordinatesForUPBlock$y, pch = 19, col = "red")  
      }
      points(APPointAll$x, APPointAll$y, pch = 19, cex = 1.5, col = "black")
    })
  })
})