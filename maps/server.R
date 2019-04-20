library("shiny")
library("retistruct")
library("raster")
library("spatstat")
library('RJSONIO')
library('DT')
library('network')
library('dichromat')
library('sp')
library('deldir')
library('tripack')
library('stats')
library('RColorBrewer')
source('functions.R')

options(warn=-1)

# Функция shinyServer() определяет логику отображения элементов
# графического интерфейса и обновления значений различных переменных:
shinyServer(function(input, output) {
  observeEvent(input$Start, {
    withProgress(message = 'Processing...', value = 0, {
      rm(list = c("n", "HTx", "R", "APPoint", "dataFromJson"))
      react_n <- reactive(as.integer(input$n))
      react_HTx <- reactive(as.double(input$HTx))
      react_APPointX <- reactive(as.double(input$APPointX))
      react_APPointY <- reactive(as.double(input$APPointY))
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
      
      
      n <<- react_n()
      HTx <<- react_HTx()
      R <<- react_R()
      APPoint <<- data.frame(x = react_APPointX(), y = react_APPointY())
      dataFromJson <<- react_dataFromJson()
      # стандартный этаж равный 3 метрам
      H <<- 3
    })
  })
  
  observeEvent(input$DataPropagation, {
    observe({
      isolate({
        withProgress(message = 'Processing...', value = 0, {
          coordinatesForBuildings <<- data.frame("Level" = array(NA, 1), "Number" = array(NA, 1), 
                                                 "x" = array(NA, 1), "y" = array(NA, 1), "H" = H, "L" = array(NA, 1))
          
          coordinatesForUP <<- data.frame("x" = array(NA, n), "y" = array(NA, n),
                                          "H" = array(2, n), "flag" = array(FALSE, n), "xInter" = array(NA, n),
                                          "yInter" = array(NA, n), "l" = array(NA, n), 
                                          "l1" = array(NA, n), "l2" = array(NA, n), "HBlockage" = array(NA, n))
          # подгрузка фрейма со зданиями из JSON
          coordinatesForBuildings <<- PointsForBuildingFromJson(dataFromJson, coordinatesForBuildings, H)
          # реиндекс для фрейма с координатами зданий
          coordinatesForBuildings <<- coordinatesForBuildings[2:length(coordinatesForBuildings[,1]),]
          row.names(coordinatesForBuildings) <- c(1:nrow(coordinatesForBuildings))
          coordinatesForBuildings <<- coordinatesForBuildings
          # подгрузка фрейма с координатами пользователей
          coordinatesForUP <<- PointsForUP(coordinatesForUP, n, coordinatesForBuildings)
          # количество зданий
          countOfBuildings <<- coordinatesForBuildings$Number[which.max(coordinatesForBuildings$Number)]
          # countOfBuildings <<- countOfBuildings - 1
          if (coordinatesForBuildings$H[which.max(coordinatesForBuildings$H)] > HTx) {
            output$TextOutput <- renderText({
              paste("Алярм, высота должна быть больше, чем", coordinatesForBuildings$H[which.max(coordinatesForBuildings$H)]) %>% 
                paste("метра(ов)")
            })
          }
          else {
            output$TextOutput <- renderText(({
              ""
            }))
          }
        })
      })
    })
  })
    
  observeEvent(input$Simulation, {
    observe({
      isolate({
        withProgress(message = 'Simulation in process...', value = 0, 
        {
          # основная программа
          # установка флага true, если проекция радиуса действия меньше,чем проекция 
            # расстояние от БПЛА до пользовательского устройства
          for (i in 1:n) {
            coordinatesForUP <- IsLOSWithoutBuilding(i, HTx, coordinatesForUP, R, APPoint)
          }
          # Установка флага true, если есть блокировка зданием
          for (i in 1:n) {
            if (coordinatesForUP$flag[i] == TRUE) {
              next()
            }
            for (j in 1:countOfBuildings) {
              coordinatesForCurrentBuildings <- coordinatesForBuildings[min(which(coordinatesForBuildings$Number == j)):
                                                                          max(which(coordinatesForBuildings$Number == j)),]
              coordinatesForUP <- VerificationUP(i, APPoint, coordinatesForUP, coordinatesForCurrentBuildings, HTx)
              if (coordinatesForUP$flag[i] == TRUE) {
                break()
              }
            }
            incProgress(1/n)
          }
          output$EndPlot <- renderPlot({
            plot(0, 0, xlim = c( (coordinatesForBuildings$x[which.min(coordinatesForBuildings$x)]),
                                 coordinatesForBuildings$x[which.max(coordinatesForBuildings$x)]),
                 ylim = c( (coordinatesForBuildings$y[which.min(coordinatesForBuildings$y)]),
                           coordinatesForBuildings$y[which.max(coordinatesForBuildings$y)]) )
            for (i in 1:coordinatesForBuildings$Number[which.max(coordinatesForBuildings$Number)]) {
              lines(x = coordinatesForBuildings$x[which(coordinatesForBuildings$Number == i)],
                    y = coordinatesForBuildings$y[which(coordinatesForBuildings$Number == i)])
            }
            points(coordinatesForUP$x[coordinatesForUP[,"flag"] == TRUE],
                   coordinatesForUP$y[coordinatesForUP[,"flag"] == TRUE], pch = 19, col = "red")
            points(coordinatesForUP$x[coordinatesForUP[,"flag"] == FALSE],
                   coordinatesForUP$y[coordinatesForUP[,"flag"] == FALSE], pch = 19, col = "green")
            points(APPoint$x, APPoint$y, col = 'black', pch = 8)
            # pts <- cbind(coordinatesForUP$x, coordinatesForUP$y)
            # km1 <- kmeans(pts, centers=3, nstart = 1, algorithm = "Lloyd")
            # CL5 <- brewer.pal(3, "Pastel1")
            # V <- voronoi.mosaic(km1$centers[,1],km1$centers[,2])
            # P <- voronoi.polygons(V)
            # points(pts,pch=19,col=CL5[km1$cluster])
            # points(km1$centers[,1],km1$centers[,2],pch=19,cex=1.5,lwd=2, col = 'black')
            # plot(V,add=TRUE)
          })
        })
      })
    })
  })
})