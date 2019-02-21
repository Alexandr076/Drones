library("shiny")

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
        numericInput ("HBuild", 
          label = "Высота зданий (не должна быть выше, чем высота AP)", value = 30),
        div(style = "display:inline-block; width:200px",
          numericInput ("gridSizeX", 
            label = "Размер сетки для моделирования {x,y}", value = 100)),
        div(style = "display:inline-block; width:200px;", 
          numericInput ("gridSizeY", 
            "", value = 100)),
        numericInput ("lmbda", 
          label = "Плотность распределения зданий (лучше указывать значения не больше 10^-4)", value = 10^-4),
        fileInput("file",
          "Загрузите файл с координатами AP (в шапке файла должны быть x и y)", 
          accept = c('.csv')),
        numericInput ("R", 
          label = "Радиус действия AP", value = 100),
        actionButton("Simulation", "Симуляция"),
        actionButton("FigureOne", "График (Вероятность подключения от количества AP)"),
        actionButton("FigureTwo", "График симуляционный")
      ),
      # Функция, определяющая структуру основного окна приложения:
      mainPanel (
        plotOutput("Figure", width = '100%', height = "800px")
      )
    )
  )
)