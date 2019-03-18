# координаты для пользовательских устройств, распределенных вне зданий для заданного json файла
PointsForUP <- function(coordinatesForUP, n, coordinatesForBuildings) {
  iter <- 1
  while (iter <= n) {
    coordinatesForUP$x[iter] <- runif(1, coordinatesForBuildings$x[which.min(coordinatesForBuildings$x)], 
                                    coordinatesForBuildings$x[which.max(coordinatesForBuildings$x)])
    coordinatesForUP$y[iter] <- runif(1, coordinatesForBuildings$y[which.min(coordinatesForBuildings$y)], 
                                    coordinatesForBuildings$y[which.max(coordinatesForBuildings$y)])
    for (i in 1:coordinatesForBuildings$Number[which.max(coordinatesForBuildings$Number)]) {
      if ( !point.in.polygon(coordinatesForUP$x[iter], coordinatesForUP$y[iter], 
                             coordinatesForBuildings$x[which(coordinatesForBuildings$Number == i)],
                             coordinatesForBuildings$y[which(coordinatesForBuildings$Number == i)]) ) {
        buildingiter <- i
      }
     else {
       break()
     }
    }
    if (buildingiter == coordinatesForBuildings$Number[which.max(coordinatesForBuildings$Number)]) {
      iter <- iter + 1
    }
  }
  return (coordinatesForUP)
}

# функция для формирования зданий из предложенного json файла
PointsForBuildingFromJson <- function(data, mapToData) {
  tryCatch({
  i <- 1
  while (TRUE) {
      if (data$features[[i]]$geometry$type == "Polygon") {
        j <- 1
        while (j <= length(data$features[[i]]$geometry$coordinates[[1]])) {
          x <- data$features[[i]]$geometry$coordinates[[1]][[j]][1]
          y <- data$features[[i]]$geometry$coordinates[[1]][[j]][2]
          if ( (is.na(as.double(data$features[[i]]$properties["building:levels"]))) ) {
            level <- 1
          }
          else {
            level <- as.double(data$features[[i]]$properties["building:levels"])
          }
          buf <- data.frame("Level" = level, "Number" = i, 
                                          "x" = x, "y" = y, H = NA, L = NA)
          mapToData <- rbind(mapToData, buf)
          j <- j + 1
        }
      }
    i <- i + 1
    }
  },
  error = function(e) {
  })
  return(mapToData)
}

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