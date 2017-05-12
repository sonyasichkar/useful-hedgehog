#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Pomenyala v sliderinputs, tam nyjno vibirat' promejytok, a ne konkretnii number. Inache nichego ne vidast.
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
                    min = 0,  max = 100, value = c(20, 60)),
        sliderInput("freedom",
                    "Неудовлетвореность уровнем свободы (Freedom Index)",
                    min = 0,  max = 10, value = c(2, 6)),
        sliderInput("environment",
                    "Неудовлетвореность состоянием окружающей среды (EPI score)",
                    min = 0,  max = 100, value = c(20, 60)),
        sliderInput("education",
                    "Неудовлетвореность уровнем образования (Education Index)",
                    min = 0,  max = 100, value = c(20, 60)),
        sliderInput("health",
                    "Неудовлетвореность уровнем здравоохранения (Health Index)",
                    min = 0,  max = 1, value = c(0.1, 0.5)),
        sliderInput("income",
                    "Неудовлетвореность уровнем дохода (Income Index)",
                    min = 0,  max = 100, value = c(20, 60))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("Map")
      )
   )
)


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




#Po povodu mapping posmotrite vot eto: https://rstudio.github.io/leaflet/shiny.html
#https://www.showmeshiny.com/neo-fireball-tracking/ - vosmojno eto toje prigoditsya


