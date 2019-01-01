library("shiny")

shinyUI(pageWithSidebar(
  # Название приложения
  headerPanel("Drones"),
  
  # Функция, определяющая структуру боковой панели приложения:
  sidebarPanel(
    numericInput ("n", 
      label = "Количество генерируемых точек UP", value = 1),
    conditionalPanel (
      condition = "output.square",
      "That's a perfect square!"
  )
  ),
  
  # Функция, определяющая структуру основного окна приложения:
  mainPanel (
    plotOutput("EndPlot"),
    textOutput("TestText")
  )
))