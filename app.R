#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(reshape2)

base <- read.csv2("base.csv", check.names = FALSE)

fill <- c("#40b8d0", "#b2d183", "#56B4E9", "#26A4E9",  "#f2d183")


basegg <- select(base, EMPRESA, ANO, `Remuneração de Capitais de Terceiros`,`Remuneração de Capitais Próprios`, `Impostos, Taxas e Contribuições`, Pessoal)
basegg[basegg < 0] <- 0
basegg$total <- (basegg$`Remuneração de Capitais de Terceiros` + basegg$`Remuneração de Capitais Próprios` + basegg$`Impostos, Taxas e Contribuições` + basegg$Pessoal)
basegg[,3:6] <- basegg[,3:6] / basegg[,7] 
basegg <- basegg[,-7]


basegg <- melt(basegg, id = c("EMPRESA", "ANO"))

print(str(base))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Estrutura de Capital"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(selectInput("empresas", "Empresas",
                                 choices = unique(basegg$EMPRESA), selected = "Ambev S/A")),
        mainPanel(plotOutput("coolplot")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$coolplot <- renderPlot({
        filtered <-
            basegg %>%
            filter(EMPRESA == input$empresas
            )
        ggplot() + theme_economist() + scale_fill_economist() +
            theme(plot.title=element_text(family="OfficinaSanITC-Book"),
                  text=element_text(family="OfficinaSanITC-Book")) +
            geom_bar(aes(y = value*100, x = ANO, fill = variable), data = filtered, stat="identity") +
            geom_text(data=filtered, aes(x = ANO, y = round(value*100), group = variable, label = paste0(round(value*100,0),"%")),
                      colour="white", family="OfficinaSanITC-Book", size= 5 , position = position_stack(vjust = 0.5)) +
            theme(legend.position="bottom", legend.direction="horizontal",
                  legend.title = element_blank()) +
            scale_x_continuous(breaks=seq(min(filtered$ANO),max(filtered$ANO),1)) +
            scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
            labs(x="Ano", y="Porcentagem") +
            ggtitle("Estrutura de Capital de Empresas") + theme(legend.text=element_text(size=8))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
