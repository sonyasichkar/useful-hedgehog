#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Введение для меня: ui - то что будет на html (интерфейс), server - то, что будет делать R
# По идее в ui у на должен быть выпадающий список (?) в котором пользователь будет выбирать то, что он хочеть поменять. 
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Хотите мигрировать?! Не патриот?! Ладно, подберем Вам что-нибудь подходящее... наверное"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("country", "Текущая страна проживания:", 
                    choices=country),
        hr(),
        helpText("0 - полная удовлетворенность, 100 - полная неудовлетворенность"),
        sliderInput("crime",
                    "Неудовлетвореность уровнем безопасности (Crime Index)",
                    min = 0,  max = 100, value = 5),
        sliderInput("freedom",
                    "Неудовлетвореность уровнем свободы (Freedom Index)",
                    min = 0,  max = 100, value = 5),
        sliderInput("environment",
                    "Неудовлетвореность состоянием окружающей среды (EPI score)",
                    min = 0,  max = 100, value = 5),
        sliderInput("education",
                    "Неудовлетвореность уровнем образования (Education Index)",
                    min = 0,  max = 100, value = 5),
        sliderInput("health",
                    "Неудовлетвореность уровнем здравоохранения (Health Index)",
                    min = 0,  max = 100, value = 5),
        sliderInput("income",
                    "Неудовлетвореность уровнем дохода (Income Index)",
                    min = 0,  max = 100, value = 5)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("Map")
      )
   )
)

# Наши inputs 
# input$freedom, input$environment, input$crime, input$education, input$health, input$income 
# Пока последовательность действий видится такая 
# 1 - через output$country определить номер строки (страну) для которой мы делаем рекомендацию, чтобы потом это использовать в "регрессии"
# 2 - нам нужно будет сделать столбец со значеним "Индекса привлекательности для миграции", который будет рассчитан по нашей формуле для каждой страны 
# 3 - немного запарится с маппингом, чтобы вывести эти значения цветами на карту и все, профит, мы молодцы, опять можем ничего не делать

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}
   
#output$Map <- renderPlot()

     
# Run the application 
shinyApp(ui = ui, server = server)

