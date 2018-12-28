library("foreach")
library("retistruct")
library("doParallel")
library("raster")
library("spatstat")
source('C:/R/drones/header.R')


# глобалы: 
# Lmin - минимально возможная длина здания
# Lmax - максимально возможная длина здания
# R - дальность действия передатчика
# NumberOfIteration - количество итераций
# APPoint - координаты для начальной точки {x, y}
# H - высота (для зданий и UP)
# n - количество разыгрываемых точек для UP
# HTxMin - минимальная высота дрона
# HTxMax - максимальная высота дрона
# gridSize - размер сетки
# coordinatesForUP - фрейм для хранения координат пользовательских устройств
	# с указанием высоты, флагом (FALSE по умолчанию) и координатами точки пересечения
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


HTx <- 20


Lmin <- 15
Lmax <- 15
R <- 35
NumberOfIteration <- 50
APPoint <- data.frame(x = 20, y = 20)
HBuild <- 8  
n <- 100
HTxMin <- 30
HTxMax <- 30
gridSize <- data.frame(x = 40, y = 40)

coordinatesForUP <- data.frame(x = runif(n, 0, gridSize$x), y = runif(n, 0, gridSize$y),
                                H = runif(n, 0.01, 2), flag = array(FALSE, n), xInter = array(NA, n),
                                yInter = array(NA, n), l = array(NA, n), l1 = array(NA, n), l2 = array(NA, n))
lmbd <- 10^-3

while (TRUE) {
	coreOfCoordinates <- rpoispp(lmbd, win = owin(c(0,40), c(0,40)))
	BuildNumber <- coreOfCoordinates$n
	if (BuildNumber > 1) {break}
}

buildingInfo <- data.frame(x1 = array(NA, BuildNumber), y1 = array(NA, BuildNumber), x2 = array(NA, BuildNumber), 
                            y2 = array(NA, BuildNumber), H = HBuild, L = array(NA, BuildNumber), angle = array(NA, BuildNumber))
pDisconnect <- array(NA, NumberOfIteration)
pConnect <- array(NA, NumberOfIteration)


# основная программа
for (k in 1:NumberOfIteration) {
	lmbd <- k * 10^-3
	HTx <- array(HTx, dim = NumberOfIteration)
	# инициация постройки зданий с общей информацией
	buildingInfo <- PointsForBuilding(lmbd, Lmin, Lmax, buildingInfo, BuildNumber, coreOfCoordinates)
	foreach(i = 1:n) %do% {
	  foreach(j = 1:BuildNumber) %do% {
	    VerificationUP(i, j, APPoint, coordinatesForUP, buildingInfo)
	  }
	}







	# координаты для UP с флагом true
	# coordinatesForUPWithTrueFlag <- coordinatesForUP[coordinatesForUP[,"flag"] == TRUE,]
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
  


# на всякий случай, вдруг пригодится
# points(coordinatesForUP$x[9], 
#        coordinatesForUP$y[9], pch = 19, col = "red")
# points(coordinatesForUP$x[1], coordinatesForUP$y[1], col = 'green', pch = 19)
# points(coordinatesForUP$x[15], coordinatesForUP$y[15], col = 'red', pch = 19)

