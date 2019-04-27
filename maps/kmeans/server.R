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
      rm(list = c("n", "HTx", "R", "APPoint", "dataFromJson", "PerOfCoverage"))
      react_n <- reactive(as.integer(input$n))
      react_HTx <- reactive(as.double(input$HTx))
      react_dataFromJson <- reactive(fromJSON(input$file$datapath))
      react_R <- reactive(as.double(input$R))
      react_PerOfCoverage <- reactive(as.integer(input$PerOfCoverage))
      react_CountOfDrones <- reactive(as.integer(input$CountOfDrones))
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
      # PerOfCoverage - процент покрытия. Если этот процент не достигнут, то проиходит моделирование с CountOfDrones + 1 дроном
      # CountOfDrones - количество АТД
      
      
      n <<- react_n()
      HTx <<- react_HTx()
      R <<- react_R()
      dataFromJson <<- react_dataFromJson()
      PerOfCoverage <<- react_PerOfCoverage()
      CountOfDrones <<- react_CountOfDrones()
      # стандартный этаж равный 3 метрам
      H <<- 3
    })
  })
  
  observeEvent(input$DataPropagation, {
    observe({
      isolate({
        withProgress(message = 'Processing...', value = 0, {
          # доделать резервирование после проверки высоты
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
          # добавляем новую колонку для записи принадлежности к конкретному дрону
          coordinatesForUP <<- cbind(coordinatesForUP, data.frame(NumberOfDrone = array(NA, n)))
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
          coordinatesForUPStart <- coordinatesForUP
          PerOfCoverageForIteration <- 0
          while (PerOfCoverageForIteration < PerOfCoverage) {
            # через k-means находим возможные координаты дронов
            if (PerOfCoverageForIteration != 0) {
              AdditionalDrones <- 2
              CountOfDrones <- AdditionalDrones + CountOfDrones
              coordinatesForUP <- coordinatesForUPStart
            } else {
              AdditionalDrones <- 0
            }
            pts <- cbind(coordinatesForUP$x, coordinatesForUP$y)
            km1 <- kmeans(pts, centers = CountOfDrones, nstart = 1, algorithm = "Lloyd")
            APPoints <- cbind(km1$centers[,1], km1$centers[,2])
            colnames(APPoints) <- c('x', 'y')
            APPoints <- as.data.frame(APPoints)
            Drones <- CountOfDrones  
            for (k in 1:Drones) {
              # фрейм для конкретной АТД за итерацию  
              APPoint <- data.frame("x" = APPoints$x[k], "y" = APPoints$y[k])
              # индексы массива UP у которых еще нет принадлежности к конкретной АТД
              index <- as.array(as.integer(rownames(coordinatesForUP[which(is.na(coordinatesForUP$NumberOfDrone)),])))
              # если нет UP, которые не принадлежат никакому дрону, то выход из цикла
              if (length(index) == 0) {
                break()
              }
              # основная программа
              # установка флага true, если проекция радиуса действия меньше,чем проекция 
                # расстояния от БПЛА до пользовательского устройства
              for (i in index) {
                coordinatesForUP <- IsLOSWithoutBuilding(i, HTx, coordinatesForUP, R, APPoint)
              }
              # Установка флага true, если есть блокировка зданием. Функция next() используется в случае, когда 
                # радиус действия АТД меньше, чем расстояние до пользователя => пропускаем шаг и идем далее по циклу
              for (i in index) {
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
                if (coordinatesForUP$flag[i] == FALSE) {
                  coordinatesForUP$NumberOfDrone[i] <- k
                }
              }
              incProgress(1/CountOfDrones)
            }
            PerOfCoverageForIteration <- (length(coordinatesForUP$x[which(coordinatesForUP$flag == FALSE)])/n)*100
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
            CL5 <- brewer.pal(CountOfDrones, "Set1")
            CL4 <- brewer.pal(CountOfDrones, "Set1")
            for (i in 1:CountOfDrones) {
              poi <- cbind(coordinatesForUP$x[which(coordinatesForUP$flag == FALSE & coordinatesForUP$NumberOfDrone == i)],
                            coordinatesForUP$y[which(coordinatesForUP$flag == FALSE & coordinatesForUP$NumberOfDrone == i)])
              points(poi, pch=19, col=CL5[i])
              points(APPoints$x[i], APPoints$y[i], pch = 8, cex = 3, col=CL4[i])
            }
            poi <- cbind(coordinatesForUP$x[which(coordinatesForUP$flag == TRUE)],
                            coordinatesForUP$y[which(coordinatesForUP$flag == TRUE)])
            points(poi, pch = 19, col = 'palegoldenrod')
          })
        })
      })
    })
  })
})