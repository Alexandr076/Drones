library("shiny")
library("RJSONIO")

shinyUI(
  fluidPage(
    # Название приложения
    titlePanel("Drones"),
    # Функция, определяющая структуру боковой панели приложения:
    sidebarLayout(
      sidebarPanel(
        numericInput ("n", 
          label = "Количество генерируемых точек UP", value = 1),
        numericInput ("HTxMin", 
          label = "Минимальная высота AP", value = 50),
        numericInput ("HTxMax", 
          label = "Максимальная высота AP", value = 51),
        div(style = "display:inline-block; width:200px",
          numericInput ("gridSizeX", 
            label = "Размер сетки для моделирования {x,y}", value = 100)),
        div(style = "display:inline-block; width:200px;", 
          numericInput ("gridSizeY", 
            "", value = 100)),
        numericInput ("NumberOfIteration", 
          label = "Количество итераций", value = 12),
        div(style = "display:inline-block; width:110px",
          numericInput ("APPointX", 
            label = "Координаты AP", value = 50)),
        div(style = "display:inline-block; width:110px;", 
          numericInput ("APPointY", 
            "{x,y}", value = 50)),
        fileInput("file",
          "Загрузите json файл из osm", 
          accept = c(
                    "application/json",
                    "text/comma-separated-values,text/plain",
                    ".json")),
        numericInput ("R", 
          label = "Радиус действия AP", value = 100),
        actionButton("Simulation", "Симуляция"),
        actionButton("FigureOne", "График")
      ),
      # Функция, определяющая структуру основного окна приложения:
      mainPanel (
        plotOutput("EndPlot", width = '900px', height = "900px")
      )
    )
  )
)