
# Введение для меня: ui - то что будет на html (интерфейс), server - то, что будет делать R
# По идее в ui у на должен быть выпадающий список (?) в котором пользователь будет выбирать то, что он хочеть поменять. 
library(shiny)
data = read.csv("data_country.csv", sep = ";")
library(dplyr)
library(tibble)
data1 = data %>% remove_rownames %>% column_to_rownames(var="Country")
# data1 = data1%>%select(crime_index, education)
library(maptools)
map_data = wrld_simpl

library('proxy') # Library of similarity/dissimilarity measures for 'dist()'
a = dist(data1, method="cosine")
b = as.matrix(a)
b = as.data.frame(b) #косинусное расстояние
### Номера в выборе страны
s = as.list(1:198)
names(s)=data$Country
### Тестируем что получается
data$result<-60*(data[2,3]-data[,2])
data[2,1]


###
rownames(data) <- data$Country 
##


##

# Разобраться здесь со знаками - Добавил минус, т.к большое значение рейтинга crime - плохо, а в остальных случаях, 
# включая жизнь, чем больше, тем лучше (подмигивающий смайлик)

# data$result=9*(data[150,2]-data[,2])*(-1)+
#   50*(data[150,3]-data[,3])+
#   30*(data[150,4]-data[,4])+
#   40*(data[150,5]-data[,5])+
#   70*(data[150,6]-data[,6])+
#   93*(data[150,7]-data[,7])
# ###

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Хотите мигрировать?! Не патриот?! Ладно, подберем Вам что-нибудь подходящее... наверное"),
   
  # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("country", "Текущая страна проживания:", 
                    choices=rownames(data)),
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
                    min = 0,  max = 100, value = 5),
        actionButton(inputId = "submit", "Update View", icon("refresh"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
       # verbatimTextOutput("Map")
        # MAp
        leafletOutput("mymap")
      #  tableOutput("view")
      )
   )
)

# Наши inputs 
# input$freedom, input$environment, input$crime, input$education, input$health, input$income 
# Пока последовательность действий видится такая 
# 1 - через output$country определить номер строки (страну) для которой мы делаем рекомендацию, чтобы потом это использовать в "регрессии"
# 2 - нам нужно будет сделать столбец со значеним "Индекса привлекательности для миграции", который будет рассчитан по нашей формуле для каждой страны 
# 3 - немного запарится с маппингом, чтобы вывести эти значения цветами на карту и все, профит, мы молодцы, опять можем ничего не делать

# Расчитать уровень похожести стран lab_recomend_system

# input$crime - насколько важно нашему клиенту изменить уровень преступности в стране (вводит его сам) 
# data$Crime_index[,input$country] - значение индекса в стране пользователя
# data$Crime_index - значения во всех других странах 

# 2 - crime index (больше-хуже)
# 3 - eco         
# 4 - education
# 5 - freedom
# 6 - health
# 7 - income

 
server <- function(input, output) {
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addPolygons(data = map_data, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5)
  })
  
  observeEvent(input$submit, {
    result = (-1)*(input$crime*(data[input$country,2]-data[,2])*(-1)+
           input$environment*(data[input$country,3]-data[,3])+
           input$education*(data[input$country,4]-data[,4])+
           input$freedom*(data[input$country,5]-data[,5])+
           input$health*(data[input$country,6]-data[,6])+
           input$income*(data[input$country,7]-data[,7]))

    result = data.frame(
      Country = data$Country,
      result = result,
      stringsAsFactors = F
    )
    
    map_data@data = data.frame(map_data@data, result[match(map_data@data[,"NAME"], result[,"Country"]),])
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = map_data$result)
    
    leafletProxy("mymap") %>% 
      clearShapes() %>% 
      addPolygons(data = map_data, color = ~pal(result))
    
  })  # По словам Алены Владимировны, нужно дублировать код для
  #дальнейшей визулизации - следующем аутпуте -, например, в график
  # делаем карту (или не карту)
  # output$view <- renderTable({
  #   result=(-1)*(input$crime*(data[input$country,2]-data[,2])*(-1)+
  #                  input$environment*(data[input$country,3]-data[,3])+
  #                  input$education*(data[input$country,4]-data[,4])+
  #                  input$freedom*(data[input$country,5]-data[,5])+
  #                  input$health*(data[input$country,6]-data[,6])+
  #                  input$income*(data[input$country,7]-data[,7]))
  #   countries<-as.data.frame(data$countries)
  #   countries$result<-result
  #   data = top5
  #   data
    # print(as.character(rownames(data)))
  # })
}

#output$Map <- renderPlot()


# Run the application 
shinyApp(ui = ui, server = server)


#Po povodu mapping posmotrite vot eto: https://rstudio.github.io/leaflet/shiny.html
#https://www.showmeshiny.com/neo-fireball-tracking/ - vosmojno eto toje prigoditsya
