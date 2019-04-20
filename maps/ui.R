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
        numericInput ("HTx", 
          label = "Высота AP", value = 50),
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
        actionButton("Start", "Запись входных данных"),
        actionButton("DataPropagation", "Разыграть координаты пользовательских устройств"),
        actionButton("Simulation", "Симуляция")
      ),
      # Функция, определяющая структуру основного окна приложения:
      mainPanel (
        h2(textOutput("TextOutput")),
        plotOutput("EndPlot", width = '900px', height = "900px")
      )
    )
  )
)