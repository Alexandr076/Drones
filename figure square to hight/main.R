library("foreach")
library("retistruct")
library("doParallel")
library("doSNOW")
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
    mass$x1[1] = 7
    mass$y1[1] = 15
    mass$x2[1] = 9
    mass$y2[1] = 20
    mass$x1[2] = 1
    mass$y1[2] = 25
    mass$x2[2] = 15
    mass$y2[2] = 35
    mass$x1[3] = 30
    mass$y1[3] = 2
    mass$x2[3] = 33
    mass$y2[3] = 30
    mass$x1[4] = 10
    mass$y1[4] = 10
    mass$x2[4] = 25
    mass$y2[4] = 5
    mass$x1[5] = 20
    mass$y1[5] = 32
    mass$x2[5] = 30
    mass$y2[5] = 30
    
    # Вызов функции для записи информации о зданиях
    buildingInfo <- BuildingCreator(mass, APPoint)
  }
  
  
  # Инициализация матрицы с информацией о зданиях с высотой: {fi1, fi2, x1, y1, x2, y2, H}. 
  # На вход подается начальная координата AP и координаты точек для построек
  BuildingCreator <- function(M, APPoint) {
    # Фрейм с данными об объекте buildingInfo
    # Записываются все данные о здании: две крайние точки прямой и углы между прямыми, соединяющими AP
    # и эти точки относительно oX
    foreach(i=1:N) %do% {
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
  
  R <<- 55
  NumberOfIteration <- 50
  APPoint <<- data.frame(x = 20, y = 20)
  H <<- 8  # build
  N <<- 5
  n <<- 10000
  # не глобал, но определяет минимальную и максимальную высоту передатчика
  HTxMin <- 40
  HTxMax <- 55
  gridSize <<- data.frame(x = 40, y = 40)
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
  coordinatesForUP$x <- coordinatesForUP$x/2.5
  coordinatesForUP$y <- coordinatesForUP$y/2.5
  coordinatesForUPCache <<- coordinatesForUP
}

par(mfrow=c(2,2))


for (k in 1:NumberOfIteration) {
  coordinatesForUP <<- coordinatesForUPCache
  # coordinatesForUP[,"x"] <- runif(n, 0, gridSize$x)
  # coordinatesForUP[,"y"] <- runif(n, 0, gridSize$y)
  HTx <<- seq(HTxMin, HTxMax, (HTxMax - HTxMin) / (NumberOfIteration - 1))
  # main
  # инициация постройки зданий с общей информацией 
  PointsForBuilding(N)
  # по всем точкам UP (1:n) проверка для каждого здания (1:N), что есть пересечение отрезка здания и {AP,UP}
  foreach(i = 1:n) %do% {
    foreach(j = 1:N) %do% {
      VerificationUP(i, j)
    }
  }
  # количество заблокированных UP 
  blockCountAll <- length(coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,1])
  # координаты для UP с флагом true
  # coordinatesForUPWithTrueFlag <<- coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,]
  # для каждой заблокированной точки вычисляем H и исправляем флаг, если нужно
  M <<- 0
  for (i in 1:length(coordinatesForUP[,1])) {
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
  # количество заблокированных UP в пространстве
  Blocks <- M
  
  # вычисляем вероятность успешного коннекта
  pDisconnect[k] <- length(coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,1])/n
  pConnect[k] <- 1 - pDisconnect[k]
  S[k] <- Blocks
  
  
  # графики
  # if (k %% 12 == 0) {
  plot(APPoint$x, APPoint$y, xlim = c(0,40), ylim = c(0,40), col = "black", pch = 19, xlab = 'x', ylab = 'y')
  lines(x = c(buildingInfo$x1[1],buildingInfo$x2[1]), y = c(buildingInfo$y1[1],buildingInfo$y2[1]))
  lines(x = c(buildingInfo$x1[2],buildingInfo$x2[2]), y = c(buildingInfo$y1[2],buildingInfo$y2[2]))
  lines(x = c(buildingInfo$x1[3],buildingInfo$x2[3]), y = c(buildingInfo$y1[3],buildingInfo$y2[3]))
  lines(x = c(buildingInfo$x1[4],buildingInfo$x2[4]), y = c(buildingInfo$y1[4],buildingInfo$y2[4]))
  lines(x = c(buildingInfo$x1[5],buildingInfo$x2[5]), y = c(buildingInfo$y1[5],buildingInfo$y2[5]))
  points(coordinatesForUP$x[coordinatesForUP[,"flag"] == TRUE],
         coordinatesForUP$y[coordinatesForUP[,"flag"] == TRUE], pch = 19, col = "red")
  points(coordinatesForUP$x[coordinatesForUP[,"flag"] == FALSE],
         coordinatesForUP$y[coordinatesForUP[,"flag"] == FALSE], pch = 19, col = "green")
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

# на всякий случай, вдруг пригодится
points(coordinatesForUP$x[9], 
       coordinatesForUP$y[9], pch = 19, col = "red")
points(coordinatesForUP$x[1], coordinatesForUP$y[1], col = 'green', pch = 19)
points(coordinatesForUP$x[15], coordinatesForUP$y[15], col = 'red', pch = 19)

library('spatstat')
x <- rHardcore(100, 0.00001)
mean(pConnect[14,], type = 'l')
prob <- array(NA, 25)
prob[1] <- 0
foreach (i = 2:25) %do% {
  prob[i] = mean(pConnect[i,])
}




