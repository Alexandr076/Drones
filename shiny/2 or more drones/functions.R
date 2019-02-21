# функция для автоматического формирования зданий для заданной плотности
PointsForBuilding <- function(lmbd, Lmin, Lmax, mass, BuildNumber, coreOfCoordinates) {
  for (i in 1:BuildNumber) {
    mass$L[i] <- runif(1, Lmin/2, Lmax/2)
    mass$angle[i] <- runif(1, 0, pi)
    if ( (mass$angle[i] > pi/2) && (mass$angle[i] < pi) ) {
      mass$x1[i] = coreOfCoordinates$x[i] - (mass$L[i] * cos(pi - mass$angle[i]))
      mass$y1[i] = coreOfCoordinates$y[i] + (mass$L[i] * sin(pi - mass$angle[i]))
      mass$x2[i] = coreOfCoordinates$x[i] + (mass$L[i] * cos(pi - mass$angle[i]))
      mass$y2[i] = coreOfCoordinates$y[i] - (mass$L[i] * sin(pi - mass$angle[i]))
    }
    if ( (mass$angle[i] == 0) || (mass$angle[i] == pi) ) {
      mass$x1[i] = coreOfCoordinates$x[i] + mass$L[i]
      mass$y1[i] = coreOfCoordinates$y[i]
      mass$x2[i] = coreOfCoordinates$x[i] - mass$L[i]
      mass$y2[i] = coreOfCoordinates$y[i]
    }
    if (mass$angle[i] == pi/2) {
      mass$x1[i] = coreOfCoordinates$x[i]
      mass$y1[i] = coreOfCoordinates$y[i] + mass$L[i]
      mass$x2[i] = coreOfCoordinates$x[i] 
      mass$y2[i] = coreOfCoordinates$y[i] - mass$L[i]
    }
    else {
      mass$x1[i] = coreOfCoordinates$x[i] + (mass$L[i] * cos(mass$angle[i]))
      mass$y1[i] = coreOfCoordinates$y[i] - (mass$L[i] * sin(mass$angle[i]))
      mass$x2[i] = coreOfCoordinates$x[i] - (mass$L[i] * cos(mass$angle[i]))
      mass$y2[i] = coreOfCoordinates$y[i] + (mass$L[i] * sin(mass$angle[i]))
    }
  }
  return (mass)
}

# Инициализация матрицы с информацией о зданиях с высотой: {fi1, fi2, x1, y1, x2, y2, H}. 
# На вход подается начальная координата AP и координаты точек для построек

BuildingCreator <- function(M, APPoint) {
  # Фрейм с данными об объекте buildingInfo
  # Записываются все данные о здании: две крайние точки прямой и углы между прямыми, соединяющими AP
  # и эти точки относительно oX
  for (i in 1:N) {
    buildingInfo$x1[i] <- M$x1[i]
    buildingInfo$y1[i] <- M$y1[i]
    buildingInfo$x2[i] <- M$x2[i]
    buildingInfo$y2[i] <- M$y2[i]
    listWithInfoAboutAngle <- edit_angle(APPoint$x, APPoint$y, M$x1[i], M$y1[i])
    buildingInfo$fiF[i] <- listWithInfoAboutAngle$angle
    listWithInfoAboutAngle <- edit_angle(APPoint$x, APPoint$y, M$x2[i], M$y2[i])
    buildingInfo$fiS[i] <- listWithInfoAboutAngle$angle
  }
}


# функция проверки, что точка лежит за пределами здания. На вход подается номер UP 
# точки и номер здания. Если пересечение между двумя прямыми, построенными между точками (AP,UP) и 
# прямой здания существует, то записывается координата пересечения, иначе - false. Здесь l1 - расстояние
# от AP до точки пересечения (Inter), l2 - от Inter до UP, l - общая длина от AP До UP

VerificationUP <- function(n, N, APPoint, coordinatesForUP, buildingInfo) {
  coordinates <- line.line.intersection(c(APPoint$x,APPoint$y), c(coordinatesForUP$x[n], 
                                    coordinatesForUP$y[n]), c(buildingInfo$x1[N],buildingInfo$y1[N]),
                                        c(buildingInfo$x2[N],buildingInfo$y2[N]), interior.only = "true")
  
  coordinatesForUP$l[n] <- pointDistance(c(APPoint$x,APPoint$y), 
                                        c(coordinatesForUP$x[n],coordinatesForUP$y[n]), 
                                          lonlat = FALSE)
  
  if (typeof(coordinates) != "logical") {
    # координаты точки пересечения и просчет длин l, l1, l2, проставление флага
    coordinatesForUP$flag[n] <- "TRUE"
    coordinatesForUP$xInter[n] <- coordinates[1]
    coordinatesForUP$yInter[n] <- coordinates[2]
    l2 <- pointDistance(c(coordinatesForUP$xInter[n],coordinatesForUP$yInter[n]), 
                                    c(coordinatesForUP$x[n],coordinatesForUP$y[n]), 
                                        lonlat = FALSE) 
    if ( !is.na(coordinatesForUP$l2[n]) ) {
      
      
      if (l2 < coordinatesForUP$l2[n]) {
        coordinatesForUP$l1[n] <- pointDistance(c(APPoint$x,APPoint$y), 
                                                 c(coordinatesForUP$xInter[n],coordinatesForUP$yInter[n]),
                                                 lonlat = FALSE)
        coordinatesForUP$l2[n] <- l2
        }
      }
    else {
      coordinatesForUP$l1[n] <- pointDistance(c(APPoint$x,APPoint$y), 
                                                 c(coordinatesForUP$xInter[n],coordinatesForUP$yInter[n]),
                                                 lonlat = FALSE)
      coordinatesForUP$l2[n] <- l2
    }
    
  }
  return (coordinatesForUP)
}

# функция для проверки LOS без учета максимального действия AP

IsNotLOS <- function(i, HTx, coordinatesForUP, H) {
  tgAlpha <- (HTx - coordinatesForUP$H[i])/coordinatesForUP$l[i]
  H_res <- tgAlpha*coordinatesForUP$l2[i] + coordinatesForUP$H[i]
  if ( H_res > H ) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}

# функция для проверки расстояние от AP До UP
IsLOSWithoutBuilding <- function(i, HTx, coordinatesForUP, R) {
  d <- sqrt( (HTx - coordinatesForUP$H[i])^2 + (coordinatesForUP$l[i])^2 )
  if ( d > R) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

# функция для расчета R (https://arxiv.org/pdf/1708.02557.pdf)
ChangeRangeForDrones <- function(R, PL) {
  R <- 
}

xxx <- 32.4 + 21*log10(5000) + 20*log10(28)








