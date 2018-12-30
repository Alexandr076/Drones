library("retistruct")
library("raster")
library("spatstat")
source('C:/R/Drones/new version/functions.R')


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


HTx <- 50
Lmin <- 0
Lmax <- 15
R <- 100
NumberOfIteration <- 8
APPoint <- data.frame(x = 50, y = 50)
HBuild <- 30 
n <- 1000
HTxMin <- 30
HTxMax <- 30
gridSize <- data.frame(x = 100, y = 100)
coordinatesForUP <- data.frame(x = runif(n, 0, gridSize$x), y = runif(n, 0, gridSize$y),
                                H = array(2, n), flag = array(FALSE, n), xInter = array(NA, n),
                                yInter = array(NA, n), l = array(NA, n), l1 = array(NA, n), l2 = array(NA, n))
coordinatesForUPAtStart <- coordinatesForUP
pDisconnect <- array(NA, NumberOfIteration)
pConnect <- array(NA, NumberOfIteration)





# основная программа
for (k in 1:NumberOfIteration) {
  coordinatesForUP <- coordinatesForUPAtStart
  lmbd <- k * 10^-3
	while (TRUE) {
	  coreOfCoordinates <- rpoispp(lmbd, win = owin(c(0,100), c(0,100)))
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
  


plot(0,0,xlim = c(0,100), ylim = c(0,100), col = "white")
for (i in 1:BuildNumber) {
  lines(x = c(buildingInfo$x1[i], buildingInfo$x2[i]), y = c(buildingInfo$y1[i], buildingInfo$y2[i]))
}
for (i in 1:100) {
  points(coordinatesForUP$x[coordinatesForUP[,"flag"] == TRUE],
         coordinatesForUP$y[coordinatesForUP[,"flag"] == TRUE], pch = 19, col = "red")
  points(coordinatesForUP$x[coordinatesForUP[,"flag"] == FALSE],
         coordinatesForUP$y[coordinatesForUP[,"flag"] == FALSE], pch = 19, col = "green")
}

