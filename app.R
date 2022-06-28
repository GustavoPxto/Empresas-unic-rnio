library(readr)
options(encoding = "UTF-8")
Unicorn_Companies <- read_csv("Unicorn_Companies.csv", 
                              col_types = cols(`Valuation ($B)` = col_number(), 
                                               `Founded Year` = col_number(), `Investors Count` = col_number(), 
                                               `Deal Terms` = col_number()))
df = Unicorn_Companies
df$`Portfolio Exits`[df$`Portfolio Exits`== "None"] = 0
df = na.omit(df)
df$`Portfolio Exits` = as.numeric(df$`Portfolio Exits`)

df$Industry[df$Industry == "Internet software & services"] = "S&S de internet"
df$Industry[df$Industry == "E-commerce & direct-to-consumer"] = "E-commerce"
df$Industry[df$Industry == "Artificial intelligence"] = "IA"
df$Industry[df$Industry == "Health"] = "Saúde"
df$Industry[df$Industry == "Supply chain, logistics, & delivery"] = "Delivery"
df$Industry[df$Industry == "Data management & analytics"] = "DM&A"
df$Industry[df$Industry == "Cybersecurity"] = "CS"
df$Industry[df$Industry == "Mobile & telecommunications"] = "Telefonia"

library(shiny)
library(shinydashboard)
library(dplyr)
library(plyr)
library(ggplot2)

ui <- dashboardPage(skin = "purple",
                    
                    dashboardHeader(title = "Empresas Unicornio",
                                    titleWidth = 350),
                    
                    dashboardSidebar(
                      width = 350,
                      sidebarMenu(
                        menuItem("Introdução", tabName = "item0", icon = icon("info")),
                        menuItem("Visão geral", tabName = "item1", icon = icon("bullseye")),
                        menuItem("Indústria", tabName = "item2", icon=icon("industry")),
                        menuItem("Localidade", tabName = "item3", icon = icon("map"))
                      )),
                    
                    dashboardBody(
                      #shinyDashboardThemes(theme = )
                      tags$head(tags$style(HTML('
                                                  .main-header .logo {
                                                      font-family: "Lucida Console",
                                                    Courier, monospace;
                                                     font-weight: bold;
                                                      font-size: 24px;
                                                                        }
                    '))),
                      tabItems(
                        tabItem(tabName = "item0",
                                tabBox(id="t1", width = 12,
                                tabPanel("Sobre", icon=icon("address-card"),
                                  box(status = "primary", 
                                      width = 15,
                                      height = 600,
                                      solidHeader = T,
                                      title = "Introdução",
                                      h1("No mundo dos negócios, uma empresa Unicórnio é 
                                         uma startup privada avaliada em 1 bilhão de dólares ou mais. 
                                         O termo foi criado em 2013 pela investidora estadunidense 
                                         Aileen Lee justamente para representar a raridade estatística 
                                         do sucesso de tais empresas. Devido ao rápido crescimento de 
                                         startups ao redor do mundo, as unicórnios ganharam relevância. 
                                         Quando o termo foi criado em 2013, existiam apenas 39 unicórnios 
                                         no mundo, ao passo que no momento desse trabalho já são 1037."),
                                  )
                                ),
                                tabPanel("Dados", dataTableOutput("dataT"), icon = icon("table")), 
                                tabPanel("Estrutura", verbatimTextOutput("structure"), icon=icon("uncharted"))
                                )),
                        tabItem(tabName = "item1",
                                fluidRow(
                                splitLayout(cellWidths = c("50%", "50%"), plotOutput("ex_z"), plotOutput("ex_y"))
                                )),
                        tabItem(tabName = "item2",
                                fluidRow(
                                splitLayout(cellWidths = c("50%", "50%"), plotOutput("ex_e"), plotOutput("ex_f"))
                                )),
                        tabItem(tabName = "item3",
                                fluidRow(
                                  splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("ex_c"), plotOutput("ex_d"))
                                ))
                      )))

server <- function(input, output) { 
  
  output$dataT <- renderDataTable(df)
  
  output$structure <- renderPrint({
    df %>% 
      str()
  })
  
  output$ex_z <- renderPlot({
    input$action
    bp0 = ggplot(df, aes(x=df$`Valuation ($B)`)) + geom_histogram(color="darkblue", fill="lightblue", binwidth=3) +  
      scale_x_continuous(breaks=seq(0,150,10)) +
      stat_summary(aes(x = 0.1, y = df$`Valuation ($B)`, xintercept = stat(y)), fun.y = mean, geom = "vline", color = "orange") +
      theme_bw() + xlab("Valor de mercado ($B)") + ylab("Frequência") +
      ggtitle("Histograma do valor de mercado das empresas 'unicórnio'")
    bp0
  })
    output$ex_y <- renderPlot({
      input$action
      cccc = ggplot(df, aes(x=`Founded Year`)) + geom_line(stat = "bin") + 
        scale_x_continuous(breaks=seq(1970,2025,5), limits = c(1970, 2025)) + theme_bw() +
        labs(x = "Ano de fundação", y = "Frequência") + 
        ggtitle("Série temporal das criações de empresas 'unicórnio' por ano desde 1970")
      cccc
    })
  output$ex_a <- renderPlot({
    input$action
    df1 = df %>%
      group_by(Country) %>%
      dplyr::summarise(count = n()) %>%
      top_n(n = 10, wt = count)
    bp1 = ggplot(df1, aes(x = reorder(Country, count, decreasing = T), y = count)) +
      geom_bar(stat = "identity", fill = "slategray4") + scale_y_continuous(breaks=seq(0,500,50)) +
      theme_bw(base_size = 9) + labs(x="País", y="Frequência") + 
      ggtitle("Top-10 países com maiores mercados de empresas 'unicórnio'")
    bp1
  }) 
  output$ex_b <- renderPlot({
    input$action
    df1 <-ddply(df, "Country", transform, `Valuation ($B)`=sum(`Valuation ($B)`))
    df1 <- subset(df1, !duplicated(Country))
    df1 = top_n(ungroup(df1), n = 10, df1$Valuation...B.)
    dd = ggplot(df1, aes(x = reorder(Country, df1$Valuation...B., decreasing = T), y = df1$Valuation...B.)) + 
      geom_bar(stat = "identity", fill= "lightcyan4") + theme_bw(base_size = 9) +
      labs(x= "País", y= "Soma dos valores de mercado") +
      ggtitle("Top-10 países com mais empresas 'unicórnio'")
    dd
    
  })
  output$ex_c <- renderPlotly({
    df1 <-ddply(df, "Country", transform, `Valuation ($B)`=sum(`Valuation ($B)`))
    df1 <- subset(df1, !duplicated(Country))
    l <- list(color = toRGB("grey"), width = 0.5)
    fig <- plot_geo(df1)
    fig <- fig %>% add_trace(
      z = ~df1$Valuation...B., color = ~df1$Valuation...B., colors = 'Purples',
      text = ~Country, locations = ~Code, marker = list(line = l)
    )
    fig <- fig %>% colorbar(title = 'Bilhões de US$', tickprefix = '$')
    fig <- fig %>% layout(
      title = 'Mercados de empresas unicórnio'
    )
    fig
  })
  output$ex_d <- renderPlot({
    input$action
    df1 = df %>%
      group_by(City) %>%
      dplyr::summarise(count = n()) %>%
      top_n(n = 10, wt = count)
    ddd = ggplot(df1, aes(x = reorder(City, count, decreasing = F), y = count)) +
      geom_bar(stat = "identity", fill= "slategray3") + scale_y_continuous(breaks=seq(0,135,20)) +
      theme_bw(base_size = 10) + labs(x="Cidade", y="Frequência") + 
      ggtitle("Top-10 cidades com maiores mercados de empresas 'unicórnio'")
    ddd
  })
  output$ex_e <- renderPlot({
    input$action
    df1 = df %>%
      group_by(Industry) %>%
      dplyr::summarise(count = n()) %>%
      top_n(n = 11, wt = count)
    bp3 = ggplot(df1[-8,], aes(x = reorder(Industry, count, decreasing = T), y = count)) +
      geom_bar(stat = "identity", fill= "slategray2") + scale_y_continuous(breaks=seq(0,188,30)) +
      theme_bw(base_size = 8) + labs(x="Indústria", y="Frequência") + 
      ggtitle("Top-10 ramos da indústria com mais empresas 'unicórnio'")
    bp3
    
  })
  output$ex_f <- renderPlot({
    input$action
    df1 <-ddply(df, "Industry", transform, `Valuation ($B)`=sum(`Valuation ($B)`))
    df1 <- subset(df1, !duplicated(Industry))
    df1 = top_n(ungroup(df1), n = 11, df1$Valuation...B.)
    dddd = ggplot(df1[-9,], aes(x = reorder(Industry, Valuation...B., decreasing = T), y = Valuation...B.)) + 
      geom_bar(stat = "identity", fill= "aquamarine4") + theme_bw(base_size = 8) + 
      labs(x= "Indústria", y= "Soma dos valores de mercado") + 
      ggtitle("Top-10 ramos da indústria com maiores mercados de empresas 'unicórnio'")
    
    dddd
  })
  
  
}

shinyApp(ui, server)
