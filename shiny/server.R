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
    react_APPointX <- reactive(as.double(input$APPointX))
    react_APPointY <- reactive(as.double(input$APPointY))
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
    # APPoint - координаты для начальной точки {x, y}
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
    

    n <- react_n()
    HTx <- react_HTx()
    # HTx <- 50
    Lmin <- 0
    Lmax <- 20
    R <- react_R()
    # R <- 100
    NumberOfIteration <- react_NumberOfIteraion()
    # NumberOfIteration <- 12
    APPoint <- data.frame(x = react_APPointX(), y = react_APPointY())
    # APPoint <- data.frame(x = 50, y = 50)
    HBuild <- react_HBuild()
    # HBuild <- 30
    HTxMin <- 30
    HTxMax <- 30
    gridSize <- data.frame(x = react_gridSizeX(), y = react_gridSizeY())
    coordinatesForUP <- data.frame(x = runif(n, 0, gridSize$x), y = runif(n, 0, gridSize$y),
                                   H = array(2, n), flag = array(FALSE, n), xInter = array(NA, n),
                                   yInter = array(NA, n), l = array(NA, n), 
                                   l1 = array(NA, n), l2 = array(NA, n))
    lmbda <- react_lmbda()
    coordinatesForUPAtStart <- coordinatesForUP
    pDisconnect <- array(NA, NumberOfIteration)
    pConnect <- array(NA, NumberOfIteration)
    
    observe({
      isolate({ 
        # основная программа
        for (k in 2:NumberOfIteration) {
          coordinatesForUP <- coordinatesForUPAtStart
          lmbd <- k * lmbda
          while (TRUE) {
            coreOfCoordinates <- rpoispp(lmbd, win = owin(c(0,gridSize$x), c(0,gridSize$y)))
            BuildNumber <- coreOfCoordinates$n
            if (BuildNumber > 1) {break}
          }
          buildingInfo <- data.frame(x1 = array(NA, BuildNumber), y1 = array(NA, BuildNumber), x2 = array(NA, BuildNumber),
                                     y2 = array(NA, BuildNumber), H = HBuild, L = array(NA, BuildNumber), angle = array(NA, BuildNumber))
          
          HTx <- array(HTx, dim = NumberOfIteration)
          
          
          # инициация постройки зданий с общей информацией
          buildingInfo <- PointsForBuilding(lmbd, Lmin, Lmax, buildingInfo, BuildNumber, coreOfCoordinates)
          for (i in 1:n) {
            for (j in 1:BuildNumber) {
              coordinatesForUP <- VerificationUP(i, j, APPoint, coordinatesForUP, buildingInfo)
            }
          }
          
          # координаты для UP с флагом true
          # coordinatesForUPWithTrueFlag <- coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,]
          # для каждой заблокированной точки вычисляем H и усправляем флаг, если нужно
          for (i in 1:length(coordinatesForUP[,1])) {
            if ( (coordinatesForUP$flag[i] == TRUE) && IsNotLOS(i, HTx[k], coordinatesForUP, HBuild) == FALSE) {
              coordinatesForUP$flag[i] <- FALSE
            }
            if (IsLOSWithoutBuilding(i, HTx[k], coordinatesForUP, R) == TRUE) {
              coordinatesForUP$flag[i] <- TRUE
            }
          }
          pDisconnect[k] <- length(coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,1])/n
          pConnect[k] <- 1 - pDisconnect[k]
        }
        
        # oX <- c(2:8)
        # oX <- oX * 10^-420
        # sm <- smooth.spline(oX, pConnect[2:8], spar = 0.35)
        # plot(sm, type = 'l', ylab = "connection probability", xlab = "building density")
        # 
        # plot(0,0,xlim = c(0,100), ylim = c(0,100), col = "white")
        # for (i in 1:BuildNumber) {
        #   lines(x = c(buildingInfo$x1[i], buildingInfo$x2[i]), y = c(buildingInfo$y1[i], buildingInfo$y2[i]))
        # }
        # for (i in 1:n) {
        #   points(coordinatesForUP$x[coordinatesForUP[,"flag"] == TRUE],
        #          coordinatesForUP$y[coordinatesForUP[,"flag"] == TRUE], pch = 19, col = "red")
        #   points(coordinatesForUP$x[coordinatesForUP[,"flag"] == FALSE],
        #          coordinatesForUP$y[coordinatesForUP[,"flag"] == FALSE], pch = 19, col = "green")
        #   points(APPoint$x, APPoint$y, pch = 19, col = "black")
        # }
        output$EndPlot <- renderPlot({
          oX <- c(2:NumberOfIteration)
          oX <- oX * lmbda
          sm <- smooth.spline(oX, pConnect[2:NumberOfIteration], spar = 0.5)
          plot(sm, type = 'l', ylab = "connection probability", xlab = "building density")
          title("График зависимости плотности распределения зданий от вероятности подключения")
        })
      })
    })
  })
})