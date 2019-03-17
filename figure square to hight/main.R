library("retistruct")
library("raster")
library("spatstat")
source('C:/R/drones/header.R')
{  
  # Функция для установки N зданий.
  PointsForBuilding <- function(N) {
    # Массив для записи координат
    mass <- data.frame(x1 = array(NA, N), y1 = array(NA, N), x2 = array(NA, N), y2 = array(NA, N))
    # foreach(i = 1:N) %do%
    # mass$x1[i] = 
    mass$x1[1] = 65+2.12132
    mass$y1[1] = 50+2.12132
    mass$x2[1] = 65-2.12132
    mass$y2[1] = 50-2.12132
    # mass$x1[2] = 1*2.5
    # mass$y1[2] = 25*2.5
    # mass$x2[2] = 15*2.5
    # mass$y2[2] = 35*2.5
    # mass$x1[3] = 30*2.5
    # mass$y1[3] = 2*2.5
    # mass$x2[3] = 33*2.5
    # mass$y2[3] = 30*2.5
    # mass$x1[4] = 10*2.5
    # mass$y1[4] = 10*2.5
    # mass$x2[4] = 25*2.5
    # mass$y2[4] = 5*2.5
    # mass$x1[5] = 20*2.5
    # mass$y1[5] = 32*2.5
    # mass$x2[5] = 30*2.5
    # mass$y2[5] = 30*2.5
    
    # Вызов функции для записи информации о зданиях
    buildingInfo <- BuildingCreator(mass, APPoint)
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
  
  # функция для проверки LOS без учета максимального действия AP (true - если заблокировано)
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
  
  # функция для проверки расстояние от AP До UP (true - если незаблокирован)
  IsLOSWithoutBuilding <- function(i, k) {
    d <- sqrt( (HTx[k] - coordinatesForUP$H[i])^2 + (coordinatesForUP$l[i])^2 )
    if ( d < R) {
      return(TRUE)
    }
    else {
      return(FALSE)
    }
  }
} 
{
  # глобалы: 
  # n - количество разыгрываемых точек для UP.
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
  
  R <<- 100
  NumberOfIteration <- 20
  APPoint <<- data.frame(x = 90, y = 50.5)
  H <<- 30  # build
  N <<- 1
  n <<- 10000
  # не глобал, но определяет минимальную и максимальную высоту передатчика
  HTxMin <- 55
  HTxMax <- 95
  gridSize <<- data.frame(x = 100, y = 100)
  coordinatesForUP <<- data.frame(x = array(NA,n), y = array(NA,n),
                                  H = 2, flag = array(FALSE, n), xInter = array(NA, n),
                                  yInter = array(NA,n), l = array(NA, n), l1 = array(NA, n), l2 = array(NA, n))
  # много точек для вывода площади
  for (i in 1:100) {
    for (j in 1:100) {
      coordinatesForUP[((i-1)*100+j),2] <- j
    }
    coordinatesForUP[((i-1)*100+1):(i*100),1] <- i
  }
  # кэш
  # coordinatesForUP <<- data.frame(x = 6, y = 8,
  #                                 H = 1, flag = array(FALSE, n), xInter = array(NA, n),
  #                                 yInter = array(NA,n), l = array(NA, n), l1 = array(NA, n), l2 = array(NA, n))
  
  buildingInfo <<- data.frame(fiF = array(NA, N), fiS = array(NA, N), x1 = array(NA, N), y1 = array(NA, N),
                              x2 = array(NA, N), y2 = array(NA, N), H = H)
  
  pDisconnect <<- array(NA, NumberOfIteration)
  pConnect <<- array(NA, NumberOfIteration)
  S <<- array(NA, NumberOfIteration)
  coordinatesForUP$x <- coordinatesForUP$x
  coordinatesForUP$y <- coordinatesForUP$y
  coordinatesForUPCache <<- coordinatesForUP
  AllPointWithLOSInCicle <<- array(NA, NumberOfIteration)
  AllBlockages <<- array(NA, NumberOfIteration)
}

# par(mfrow=c(2,2))


for (k in 1:NumberOfIteration) {
  coordinatesForUP <<- coordinatesForUPCache
  # coordinatesForUP[,"x"] <- runif(n, 0, gridSize$x)
  # coordinatesForUP[,"y"] <- runif(n, 0, gridSize$y)
  HTx <<- seq(HTxMin, HTxMax, (HTxMax - HTxMin) / (NumberOfIteration - 1))
  # main
  # инициация постройки зданий с общей информацией 
  PointsForBuilding(N)
  # по всем точкам UP (1:n) проверка для каждого здания (1:N), что есть пересечение отрезка здания и {AP,UP}
  for (i in 1:n) {
    for (j in 1:N) {
      VerificationUP(i, j)
    }
  }
  # количество заблокированных UP 
  blockCountAll <- length(coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,1])
  # координаты для UP с флагом true
  # coordinatesForUPWithTrueFlag <<- coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,]
  # для каждой заблокированной точки вычисляем H и исправляем флаг, если нужно
  M <<- 0
  Blockage <<- 0
  AllPointWithLOSInCicle <<- 0
  for (i in 1:length(coordinatesForUP[,1])) {
    # if (IsLOSWithoutBuilding(i, k) == TRUE) {
    #   PointsWithLOSInCicle <- PointsWithLOSInCicle + 1       
    # }
    if ( (coordinatesForUP$flag[i] == TRUE)  && (IsLOSWithoutBuilding(i, k) == TRUE) && (IsLOS(i, k) == TRUE) )
    {
      Blockage <- Blockage + 1
    }
    if ( (coordinatesForUP$flag[i] == TRUE) && IsLOS(i, k) == FALSE) {
      coordinatesForUP$flag[i] <- FALSE
      if (IsLOSWithoutBuilding(i, k) == FALSE) {
        coordinatesForUP$flag[i] <- TRUE
      }
      else {
        M <- M + 1
      }
    }
    if ( (coordinatesForUP$flag[i] == FALSE) && (IsLOSWithoutBuilding(i, k) == FALSE) ) {
      coordinatesForUP$flag[i] <- TRUE
    }
  }
  # количество незаблокированных UP в пространстве
  # AllPointWithLOSInCicle[k] <- PointsWithLOSInCicle
  NonBlocks <- M
  
  # вычисляем вероятность успешного коннекта
  pDisconnect[k] <- length(coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,1])/n
  pConnect[k] <- 1 - pDisconnect[k]
  S[k] <- NonBlocks
  AllBlockages[k] <- Blockage
  
  
  # графики
  # if (k %% 12 == 0) {
  # plot(APPoint$x, APPoint$y, xlim = c(0,100), ylim = c(0,100), col = "black", pch = 19, xlab = 'x', ylab = 'y')
  # lines(x = c(buildingInfo$x1[1],buildingInfo$x2[1]), y = c(buildingInfo$y1[1],buildingInfo$y2[1]))
  # lines(x = c(buildingInfo$x1[2],buildingInfo$x2[2]), y = c(buildingInfo$y1[2],buildingInfo$y2[2]))
  # lines(x = c(buildingInfo$x1[3],buildingInfo$x2[3]), y = c(buildingInfo$y1[3],buildingInfo$y2[3]))
  # lines(x = c(buildingInfo$x1[4],buildingInfo$x2[4]), y = c(buildingInfo$y1[4],buildingInfo$y2[4]))
  # lines(x = c(buildingInfo$x1[5],buildingInfo$x2[5]), y = c(buildingInfo$y1[5],buildingInfo$y2[5]))
  # points(coordinatesForUP$x[coordinatesForUP[,"flag"] == TRUE],
  #        coordinatesForUP$y[coordinatesForUP[,"flag"] == TRUE], pch = 19, col = "red")
  # points(coordinatesForUP$x[coordinatesForUP[,"flag"] == FALSE],
  #        coordinatesForUP$y[coordinatesForUP[,"flag"] == FALSE], pch = 19, col = "green")
  # }
}
# нужно реализовать параллельный процесс
cl <- makeCluster(4)
registerDoParallel(cl)
ptime <- system.time({
  foreach(i = 1:10^9) %dopar% {
    ttt <- runif(1, 0, 100)
  }
})
stopCluster(cl)
ptime


plot(APPoint$x, APPoint$y, xlim = c(0,100), ylim = c(0,100), col = "black", pch = 19, xlab = 'x', ylab = 'y')
lines(x = c(buildingInfo$x1[1],buildingInfo$x2[1]), y = c(buildingInfo$y1[1],buildingInfo$y2[1]))
# lines(x = c(buildingInfo$x1[2],buildingInfo$x2[2]), y = c(buildingInfo$y1[2],buildingInfo$y2[2]))
# lines(x = c(buildingInfo$x1[3],buildingInfo$x2[3]), y = c(buildingInfo$y1[3],buildingInfo$y2[3]))
# lines(x = c(buildingInfo$x1[4],bulidingInfo$x2[4]), y = c(buildingInfo$y1[4],buildingInfo$y2[4]))
# lines(x = c(buildingInfo$x1[5],buildingInfo$x2[5]), y = c(buildingInfo$y1[5],buildingInfo$y2[5]))
points(coordinatesForUP$x[coordinatesForUP[,"flag"] == TRUE],
       coordinatesForUP$y[coordinatesForUP[,"flag"] == TRUE], pch = 19, col = "red")
points(coordinatesForUP$x[coordinatesForUP[,"flag"] == FALSE],
       coordinatesForUP$y[coordinatesForUP[,"flag"] == FALSE], pch = 19, col = "green")

datat <- smooth.spline(HTx, S, spar = 0.5)
plot(datat,type ='l', xlab = 'Высота, м', ylab = expression('Площадь S'[gain]))

#PBlockage <- AllBlockages/(3.141592*(sqrt(R^2 - (HTx - 2)^2))^2)
PNonBlockage <- S/SAll
dataP <- smooth.spline(HTx, PNonBlockage, spar = 0.5)
plot(dataP,type ='l', xlab = 'Высота, м', ylab = expression('Площадь "теневой" области'), lwd = 2, col = 'red')

# на всякий случай, вдруг пригодится
points(coordinatesForUP$x[9], 
       coordinatesForUP$y[9], pch = 19, col = "red")
points(coordinatesForUP$x[1], coordinatesForUP$y[1], col = 'green', pch = 19)
points(coordinatesForUP$x[15], coordinatesForUP$y[15], col = 'red', pch = 19)


newHTx <- HTx - 2
RProjection <- sqrt(100^2-newHTx^2)
SAll <- 3.141592*RProjection^2
 
grid(nx = NULL, , lty = 1, lwd = 2)
plot(HTx, dataP$y, type ='l', col = 'red', lwd = 2, xlab = 'Высота точки доступа, м',
     ylab = 'Вероятность подключения за зданием')


plot(x = HTx, y = pConnect,  type = 'l')


