library("foreach")
library("retistruct")
library("doParallel")
library("raster")
library("spatstat")
source('C:/R/drones/header.R')

{
  # параллельный процесс
  # cl <- parallel::makeCluster(4)
  # doParallel::registerDoParallel(cl)
  # clusterEvalQ(cl, library("retistruct"))
  # parallel::stopCluster(cl)
  
  {  
    # # Функция для установки N зданий ручками
    # PointsForBuilding <- function(N) {
    #   # Массив для записи координат
    #   mass <- data.frame(x1 = array(NA, N), y1 = array(NA, N), x2 = array(NA, N), y2 = array(NA, N))
    #   # foreach(i = 1:N) %do%
    #   # mass$x1[i] = 
    #   mass$x1[1] = 7
    #   mass$y1[1] = 15
    #   mass$x2[1] = 9
    #   mass$y2[1] = 20
    #   mass$x1[2] = 1
    #   mass$y1[2] = 25
    #   mass$x2[2] = 15
    #   mass$y2[2] = 35
    #   mass$x1[3] = 30
    #   mass$y1[3] = 2
    #   mass$x2[3] = 33
    #   mass$y2[3] = 30
    #   
    #   # Вызов функции для записи информации о зданиях
    #   buildingInfo <- BuildingCreator(mass, APPoint)
    # }
    
    
    # функция для автоматического формирования зданий для заданной плотности
    PointsForBuilding <- function(N) {
      mass <- data.frame(x1 = array(NA, N), y1 = array(NA, N), x2 = array(NA, N), y2 = array(NA, N))
      # надо доделать окно 
      coreOfCoordinates <- rpoispp(lmbd, 200, win = owin(c(0,40), c(0,40)))
      foreach(i = 1:N) %do% {
        l <- runif(1, 0, L/2)
      }
    }
    # Инициализация матрицы с информацией о зданиях с высотой: {fi1, fi2, x1, y1, x2, y2, H}. 
    # На вход подается начальная координата AP и координаты точек для построек
    BuildingCreator <- function(M, APPoint) {
      # Фрейм с данными об объекте buildingInfo
      # Записываются все данные о здании: две крайние точки прямой и углы между прямыми, соединяющими AP
      # и эти точки относительно oX
      for (i in 1:N) {
        buildingInfo$x1[i] <<- M$x1[i]
        buildingInfo$y1[i] <<- M$y1[i]
        buildingInfo$x2[i] <<- M$x2[i]
        buildingInfo$y2[i] <<- M$y2[i]
        listWithInfoAboutAngle <- edit_angle(APPoint$x, APPoint$y, M$x1[i], M$y1[i])
        buildingInfo$fiF[i] <<- listWithInfoAboutAngle$angle
        listWithInfoAboutAngle <- edit_angle(APPoint$x, APPoint$y, M$x2[i], M$y2[i])
        buildingInfo$fiS[i] <<- listWithInfoAboutAngle$angle
      }
    }
    
    
    # функция проверки, что точка лежит за пределами здания (проверка на fi). На вход подается номер UP 
    # точки и номер здания. Если пересечение между двумя прямыми, построенными между точками (AP,UP) и 
    # прямой здания существует, то записывается координата пересечения, иначе - false 
    VerificationUP <- function(n, N) {
      coordinates <- line.line.intersection(c(APPoint$x,APPoint$y), c(coordinatesForUP$x[n], 
                                                                      coordinatesForUP$y[n]), c(buildingInfo$x1[N],buildingInfo$y1[N]),
                                            c(buildingInfo$x2[N],buildingInfo$y2[N]), interior.only = "true")
      coordinatesForUP$l[n] <<- pointDistance(c(APPoint$x,APPoint$y), 
                                              c(coordinatesForUP$x[n],coordinatesForUP$y[n]), 
                                              lonlat = FALSE)
      if (typeof(coordinates) != "logical") { 
        # координаты точки пересечения и просчет длин l, l1, l2, проставление флага
        coordinatesForUP$flag[n] <<- "TRUE"
        coordinatesForUP$xInter[n] <<- coordinates[1]
        coordinatesForUP$yInter[n] <<- coordinates[2]
        coordinatesForUP$l1[n] <<- pointDistance(c(APPoint$x,APPoint$y), 
                                                 c(coordinatesForUP$xInter[n],coordinatesForUP$yInter[n]),
                                                 lonlat = FALSE)
        coordinatesForUP$l2[n] <<- pointDistance(c(coordinatesForUP$xInter[n],coordinatesForUP$yInter[n]), 
                                                 c(coordinatesForUP$x[n],coordinatesForUP$y[n]), 
                                                 lonlat = FALSE)
      }
    }
    
    # функция для проверки LOS без учета максимального действия AP
    IsLOS <- function(i, k) {
      tgAlpha <- (HTx[k] - coordinatesForUP$H[i])/coordinatesForUP$l[i]
      H_res <- tgAlpha*coordinatesForUP$l2[i] + coordinatesForUP$H[i]
      if ( H_res > H ) {
        return(FALSE)
      }
      else {
        return(TRUE)
      }
    }
    
    # функция для проверки расстояние от AP До UP
    IsLOSWithoutBuilding <- function(i, k) {
      d <- sqrt( (HTx[k] - coordinatesForUP$H[i])^2 + (coordinatesForUP$l[i])^2 )
      if ( d > R) {
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    }
  } 
  {
    # глобалы: 
    # n - количество разыгрываемых точек для UP
    # N - количество построек (зданий)
    # APPoint - координаты для начальной точки {x, y}
    # buildingInfo - информационный фрейм об объектах
    # coordinatesForUP - фрейм для хранения координат пользовательских устройств
    # рандомной высотой, флагом (FALSE по умолчанию) и координатами точки пересечения
    # coordinatesForUPWithTrueFlag - тоже самое, что и (выше), но с флагом = TRUE (заблокирован)
    # UPblockingInfo - информация о зданиях, блокирующих i-й пользовательский девайс 
    # (в долгосрочной перспективе)
    # gridSize - размер сетки
    # R - радиус действия AP
    # H - высота (для зданий и UP)
    # HTx - высотка передатчика
    # w - угол в плоскости YoZ между Tx и Rx
    # D - расстояние между Tx и Rx в плоскости XoY
    # l - расстояние между точкой пересечения прямой здания и точкой Tx в плоскости YoZ
    # pConnect - вероятность los
    # NumberOfIteration - количество итераций
    # R - дальность действия передатчика
    # lmbd - плотность распределения для зданий
    # L - длина зданий
    
    L <<- 10
    R <<- 35
    NumberOfIteration <- 50
    APPoint <<- data.frame(x = 20, y = 20)
    H <<- 8  # build
    N <<- 3
    n <<- 100
    # не глобал, но определяет минимальную и максимальную высоту передатчика
    HTxMin <- 30
    HTxMax <- 30
    gridSize <<- data.frame(x = 40, y = 40)
    coordinatesForUP <<- data.frame(x = runif(n, 0, gridSize$x), y = runif(n, 0, gridSize$y),
                                    H = runif(n, 0.01, 2), flag = array(FALSE, n), xInter = array(NA, n),
                                    yInter = array(NA,n), l = array(NA, n), l1 = array(NA, n), l2 = array(NA, n))
    # кэш
    coordinatesForUPCache <<- coordinatesForUP
    # coordinatesForUP <<- data.frame(x = 6, y = 8,
    #                                 H = 1, flag = array(FALSE, n), xInter = array(NA, n),
    #                                 yInter = array(NA,n), l = array(NA, n), l1 = array(NA, n), l2 = array(NA, n))
    
    buildingInfo <<- data.frame(fiF = array(NA, N), fiS = array(NA, N), x1 = array(NA, N), y1 = array(NA, N),
                                x2 = array(NA, N), y2 = array(NA, N), H = H)
    
    pDisconnect <<- array(NA, NumberOfIteration)
    pConnect <<- array(NA, NumberOfIteration)
  }
  # clusterExport(cl, varlist = c('buildingInfo','edit_angle',
  #                               '%do%','foreach', 'VerificationUP','determinant',
  #                               'determinant.matrix', 'det','retistruct',
  #                               'line.line.intersection'), envir=environment())
  # par(mfrow=c(2,2))
  
  
  for (k in 1:NumberOfIteration) {
    # распределение для зданий 
    lmbd <<- k*10^-3
    coordinatesForUP <<- coordinatesForUPCache
    # coordinatesForUP[,"x"] <- runif(n, 0, gridSize$x)
    # coordinatesForUP[,"y"] <- runif(n, 0, gridSize$y)
    HTx <<- array(HTx, dim = NumberOfIteration)
    # main
    # инициация постройки зданий с общей информацией 
    PointsForBuilding(N)
    # по всем точкам UP (1:n) проверка для каждого здания (1:N) 
    foreach(i = 1:n) %do% {
      foreach(j = 1:N) %do% {
        VerificationUP(i, j)
      }
    }
    # координаты для UP с флагом true
    # coordinatesForUPWithTrueFlag <<- coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,]
    # для каждой заблокированной точки вычисляем H и усправляем флаг, если нужно
    for (i in 1:length(coordinatesForUP[,1])) {
      if ( (coordinatesForUP$flag[i] == TRUE) && IsLOS(i, k) == FALSE) {
        coordinatesForUP$flag[i] <- FALSE
      }
      if (IsLOSWithoutBuilding(i, k) == TRUE) {
        coordinatesForUP$flag[i] <- TRUE
      }
    }
    
    # вычисляем вероятность успешного коннекта
    pDisconnect[k] <- length(coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,1])/n
    
    
    # графики
    # if (k %% 12 == 0) {
    plot(APPoint$x[3], APPoint$y[4], xlim = c(0,40), ylim = c(0,40), col = "black", pch = 19, xlab = 'x', ylab = 'y')
    lines(x = c(buildingInfo$x1[1],buildingInfo$x2[1]), y = c(buildingInfo$y1[1],buildingInfo$y2[1]))
    lines(x = c(buildingInfo$x1[2],buildingInfo$x2[2]), y = c(buildingInfo$y1[2],buildingInfo$y2[2]))
    lines(x = c(buildingInfo$x1[3],buildingInfo$x2[3]), y = c(buildingInfo$y1[3],buildingInfo$y2[3]))
    points(coordinatesForUP$x[coordinatesForUP[,"flag"] == TRUE],
           coordinatesForUP$y[coordinatesForUP[,"flag"] == TRUE], pch = 19, col = "red")
    points(coordinatesForUP$x[coordinatesForUP[,"flag"] == FALSE],
           coordinatesForUP$y[coordinatesForUP[,"flag"] == FALSE], pch = 19, col = "green")
    # }
  }
  
}

# на всякий случай, вдруг пригодится
# points(coordinatesForUP$x[9], 
#        coordinatesForUP$y[9], pch = 19, col = "red")
# points(coordinatesForUP$x[1], coordinatesForUP$y[1], col = 'green', pch = 19)
# points(coordinatesForUP$x[15], coordinatesForUP$y[15], col = 'red', pch = 19)


