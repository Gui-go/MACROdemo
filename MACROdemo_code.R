rm(list = ls())

# Libraries ---------------------------------------------------------------

library("shiny")
library("shinydashboard")
library("plotly")
library("ggplot2")
library("packrat")
library("ggfortify")
library("shinyWidgets")  
library("DT")
library("tidyverse")
library("forecast")
library("dplyr")
library("moments")
library("tseries")
library("devtools")
library("rsconnect")
library("httr")
library("RCurl")
library("Quandl")



# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "MACROdemo",
    titleWidth = 300
  ),
  
  
  # Sidebar -----------------------------------------------------------------
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Home", tabName = "HOME", icon = icon("asterisk")),
      menuItem("Apresentação", tabName = "AP", icon = icon("star")),
      menuItem("Metodologia", tabName = "MET", icon = icon("cog")),
      menuItem("Contextualização", tabName = "CONTEXT", icon = icon("plus")),
      menuItem("Curto Prazo", tabName = "CP", icon = icon("plus"),
               menuSubItem("Introdução", tabName = "INTROCP", icon = icon("plus")),
               menuSubItem("Demanda Agregada", tabName = "DACP", icon = icon("plus")),
               menuSubItem("Mercado de Bens", tabName = "MBCP", icon = icon("plus")),
               menuSubItem("Curva IS", tabName = "ISCP", icon = icon("plus")),
               menuSubItem("Mercado Monetário", tabName = "MMCP", icon = icon("plus")),
               menuSubItem("Curva LM", tabName = "LMCP", icon = icon("plus")),
               menuSubItem("Integração dos Mercados", tabName = "COMBCP", icon = icon("plus")),
               menuSubItem("IS-LM", tabName = "ISLMCP", icon = icon("plus"))),
      menuItem("Médio Prazo", tabName = "MP", icon = icon("plus"), badgeLabel = "Breve", badgeColor = "red"),
      menuItem("Longo Prazo", tabName = "LP", icon = icon("plus"), badgeLabel = "Breve", badgeColor = "red"),
      menuItem("Observatório", tabName = "OB", icon = icon("plus")),
      menuItem("Referências", tabName = "REF", icon = icon("check")),
      menuItem("Sobre", tabName = "SOBRE", icon = icon("paperclip")),
      menuItem("Contato", tabName = "CTT", icon = icon("send"))
    )
  ),
  dashboardBody(
    

    includeCSS("www/MACROdemo.css"),

    tabItems(
      
      # Home --------------------------------------------------------------------
      
      
      
      tabItem(
        tabName = "HOME",
        fluidRow(h1(strong("HOME"), align = "center")),
        fluidPage(
          box(
            width = 12,
            br(),
            br(),
            h2('"Cada escolha uma renúncia, isso é a vida."(Jr, Charlie Brown. "Lutar pelo que é meu")')
          )
        )
      ),
      
      # Apresentação ------------------------------------------------------------
      
      tabItem(
        tabName = "AP",
        fluidRow(h1(strong("Apresentação"), align = "center")),
        fluidPage(
          box(
            width = 12,
            h3("MACROdemo"),
            br(),
            h3("No intuito de se desenvolver em mais detalhes a possibilidade de criação de uma aplicação shiny para o auxilio no aprendizado de macroeconomia, foi desenvolvida a presente plataforma, ainda em fase demo, para convidar os potenciais envolvidos em sua criação.")
          )
        )
      ),
      
      
      
      # Metodologia -------------------------------------------------------------
      
      tabItem(
        tabName = "MET",
        fluidRow(h1(strong("Metodologia"), align = "center")),
        fluidPage(
          box(
            width = 12
          )
        )
      ),
      
      
      # Contextualização --------------------------------------------------------
      
      tabItem(
        tabName = "CONTEXT",
        fluidRow(h1(strong("Contextualização"), align = "center")),
        fluidPage(
          box(
            width = 12,
            h3("Aqui acho interessante fazer uma introdução ao conteúdo e diferenciar as abordagens de curto, médio e longo prazo")
          )
        )
      ),
      
      
      
      # Curto Prazo -------------------------------------------------------------
      
      
      tabItem(
        tabName = "INTROCP",
        fluidRow(h1(strong("Introdução"), align = "center")),
        fluidPage(
          fluidRow(
            box(
              width = 12,
              title = tagList(shiny::icon("gear"), strong("Conteúdo")),
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary",
              h3("Introdução ao curto prazo")
            )
          ),
          fluidRow(
            box(
              width = 3,
              title = tagList(shiny::icon("gear"), strong("Controles")),
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary",
              column(12, inputPanel(
                column(6, numericInput("C0", "Consumo autônomo", min = 0, max = 1000, step = 1, value = 30, width = 300)),
                numericInput("C1", "Propensão Marginal a Consumir", min = 0, max = 1000, step = 0.05, value = 0.8)
              ))
            ),
            box(
              width = 9,
              title = tagList(shiny::icon("gear"), strong("Gráfico")),
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary",
              plotOutput("FC")
            )
          )
        )
      ),
      
      ####################### Demanda Agregada   Curto prazo   #####################
      tabItem(
        tabName = "DACP",
        fluidRow(h1(strong("Demanda Agregada"), align = "center")),
        fluidPage(
          box(
            width = 12,
            h3(""),
            h2(strong("Demanda Agregada: $$DA\\equiv C + I + G$$")),
            h2(strong("Então: $$DA \\equiv c_0 + c_1 (y- t_0 -t_y) + I_0 - a_i + G_0$$")),
            h2(strong("Logo: $$y = c_0 + c_1y - c_1t_0 - c_1t_y +I_0 -ai + G_0$$")),
            h2(strong("Portanto: $$y^* = \\left[\\frac{1}{1-c_1(1-t)}\\right] . \\left[c_0-c_1t_0 + I_0 + G_0 -ai\\right]$$"))
          )
        )
      ),
      
      ####################### Mercado de Bens   Curto Prazo  ###########################
      tabItem(
        tabName = "MBCP",
        fluidRow(h1(strong("Mercado de Bens"), align = "center")),
        fluidPage(
          box(
            width = 12,
            h3("Introdução ao Mercado de bens")
          )
        )
      ),
      
      
      ####################### IS  Curto Prazo   #####################
      tabItem(
        tabName = "ISCP",
        fluidRow(h1(strong("Investment = Savings"), align = "center")),
        fluidPage(
          box(
            width = 12,
            withMathJax(),
            h3(""),
            h2(strong("$$i=\\frac{A}{a}-1-c_1\\frac{(1-t)}{a}\\times{y}$$")),
            h2(strong("Sendo: $$A=c_0-c_1T_o+I_0+G_0$$")),
            h2(strong("Portanto: $$i=\\frac{c_0-c_1T_o+I_0+G_0}{a}-\\left[\\frac{1-c_1(1-t)}{a}\\right].y$$"))
          ),
          box(width = 3,
              numericInput("ISc0", "Consumo Autonomo", min = 0, max = 1000, step = 50, value = 450),
              numericInput("ISc1", "Propensão Marginal a Consumir", min = 0, max = 1000, step = 0.1, value = 0.8),
              numericInput("IST0", "T0", min = 0, max = 1000, step = 50, value = 300),
              numericInput("ISI0", "I0", min = 0, max = 1000, step = 50, value = 300),
              numericInput("ISG0", "G0", min = 0, max = 1000, step = 50, value = 300),
              numericInput("ISa", "a", min = 0, max = 1, step = 0.1, value = 0.1),
              numericInput("ISt", "t", min = 0, max = 1000, step = 1, value = 3)
          ),
          box(width = 9,
              plotOutput("ISgraph"))
        )
      ),
      
      #########################   Mercado Monetário  no Curto Prazo  #####################################
      
      tabItem(
        tabName = "MMCP",
        fluidRow(h1(strong("Mercado Monetário"), align = "center")),
        fluidPage(
          box(
            width = 12,
            h3("")
          )
        )
      ),
      
      ####################### IS  Curto Prazo   #####################
      tabItem(
        tabName = "LMCP",
        fluidRow(h1(strong("LM curve"), align = "center")),
        fluidPage(
          box(width = 3,
              numericInput("LMl0", "l0", min = 0, max = 1, step = 0.05, value = 0.9),
              numericInput("LMl1", "l1", min = 0, max = 1, step = 0.1, value = 0.3),
              numericInput("LMms0p", "oferta de moeda", min = 0, max = 1000, step = 50, value = 30),
              numericInput("LMvt", "velocity", min = 0, max = 1000, step = 0.1, value = 0.1)
          ),
          box(width = 9,
              plotOutput("LMgraph"))
        )
      ),            
      
      #########################   Combinação da Curvas  #####################################
      tabItem(
        tabName = "COMBCP",
        fluidRow(h1(strong("Integração dos Mercados"), align = "center")),
        fluidPage(
          box(
            width = 12,
            h3("")
          )
        )
      ),
      
      
      #########################   IS-LM curto prazo  #####################################
      
      tabItem(
        tabName = "ISLMCP",
        fluidRow(h1(strong("IS-LM"), align = "center")),
        fluidPage(
          box(width = 12,
              title = tagList(shiny::icon("gear"), strong("Controles")),
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary",
              column(3, inputPanel(
                numericInput("ISLMc0", "Consumo Autonomo", min = 0, max = 1000, step = 50, value = 450),
                numericInput("ISLMc1", "Propensão Marginal a Consumir", min = 0, max = 1000, step = 0.1, value = 0.8),
                numericInput("ISLMT0", "T0", min = 0, max = 1000, step = 50, value = 300),
                numericInput("ISLMI0", "I0", min = 0, max = 1000, step = 50, value = 300),
                numericInput("ISLMG0", "G0", min = 0, max = 1000, step = 50, value = 300),
                numericInput("ISLMa", "a", min = 0, max = 1, step = 0.1, value = 0.1),
                numericInput("ISLMt", "t", min = 0, max = 1000, step = 1, value = 3)
              )),
              column(3, inputPanel(
                numericInput("ISLMl0", "l0", min = 0, max = 1, step = 0.05, value = 0.9),
                numericInput("ISLMl1", "l1", min = 0, max = 1, step = 0.1, value = 0.3),
                numericInput("ISLMms0p", "oferta de moeda", min = 0, max = 1000, step = 50, value = 30),
                numericInput("ISLMvt", "velocity", min = 0, max = 1000, step = 0.1, value = 0.1)
              ))
          ),
          box(width = 12,
              title = tagList(shiny::icon("gear"), strong("Gráfico")),
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary",
              plotOutput("ISLMgraph"))
        )
      ),
      
      #########################   Médio Prazo  #####################################
      tabItem(
        tabName = "MP",
        fluidRow(h1(strong("Médio Prazo"), align = "center")),
        fluidPage(
          box(
            width = 12,
            h3("Em breve")
          )
        )
      ),
      
      #########################   Longo prazo  #####################################
      tabItem(
        tabName = "LP",
        fluidRow(h1(strong("Longo Prazo"), align = "center")),
        fluidPage(
          box(
            width = 12,
            h3("Em breve")
          )
        )
      ),
      
      #########################   Observatório  #####################################
      tabItem(
        tabName = "OB",
        fluidRow(h1(strong("Observatório"), align = "center")),
        fluidPage(
          box(title = tagList(shiny::icon("gear"), strong("Controles")),
              solidHeader = T,
              width = 4,
              height = "100%",
              status = "success",
              selectInput(
                inputId = "SOB",
                label = "Série:",
                choices = c("SELIC" = 4390,
                            "CDI" = 4391,
                            "IPCA" = 433,
                            "INPC" = 188),
                selected = "SELIC"
              ),
              br(),
              selectInput("MARKOB", "Marcadores:",
                          multiple = T,
                          selectize = T,
                          c("Média" = "m1",
                            "Mediana" = "m2"),
                          selected = c("m1", "m2")),
              br(),
              sliderInput(
                inputId = "SLDOB",
                label = "Período da série:",
                value = c(164, 392), min = 1 , max = 500, step = 1
              ),
              br(),
              box(
                column(2),
                width = 12,
                tableOutput("PIFOB")
              )
          ),
          tabBox(
            id = "GRAFOB",
            title = strong("Análise gráfica"),
            width = 8,
            side = "right",
            height = "673px",
            tabsetPanel(
              tabPanel(
                "Gráfico de linha",
                plotlyOutput("GLOB", height = "530px")),
              tabPanel(
                "Gráfico de linhas",
                plotlyOutput("GLOBS", height = "530px")),
              tabPanel(
                "OUTROS",
                plotlyOutput("OTOB", height = "530px")),
              tabPanel(
                "Tabela",
                dataTableOutput("TABLEOB"))
            )
          )
        )
      ),
      
      
      
      # Referêencias ------------------------------------------------------------
      
      tabItem(
        tabName = "REF",
        fluidRow(h1(strong("Referências"), align = "center")),
        fluidPage(
          box(
            width = 12,
            h3("Blanchard é nosso rei"),
            h3("Keynes"),
            h3("..."),
            h3("")
          )
        )
      ),
      
      
      # Sobre -------------------------------------------------------------------
      
      tabItem(
        tabName = "SOBRE",
        fluidRow(h1(strong("Sobre"), align = "center")),
        fluidPage(
          box(
            width = 12,
            h3("Essa é apenas uma versão demonstração pra que possamos desenvolver melhor essa ideia...")
          )
        )
      ),
      
      
      
      # Contato -----------------------------------------------------------------
      
      tabItem(
        tabName = "CTT",
        fluidRow(h1(strong("Contatos"), align = "center")),
        fluidPage(
          h2(strong("Início")),
          box(
            width = 12,
            h3(strong("Guilherme Viegas")),
            h3("guilhermeviegas1993@gmail.com"),
            tags$a(href = "https://guilhermeviegas.portfoliobox.net/", h3("Guigo's Portifólio")),
            br()
          )
        )
      )
      
      
    )
  )
)


# Servidor ----------------------------------------------------------------

server <- function(input, output){
  
  output$FC <- renderPlot({
    C = NULL
    C0 = input$C0
    C1 = input$C1
    YD = c(1:200)
    for (i in seq_along(YD)) {
      C[i]  <- input$C0+input$C1*YD[i]
    }
    df<- data.frame(YD, C)
    ggplot(df)+
      geom_line(aes(df$YD, df$C))+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0)+
      labs(title = "Função Consumo",
           x = "Renda, Y",
           y = "Consumo, C",
           caption = "MACROdemo")+
      xlim(-1, 100)+
      ylim(-1,100)+
      theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 10, face = "bold"),
            plot.caption = element_text(size = 10, face = "bold"),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  
  
  
  
  
  
  
  output$ISgraph <- renderPlot({
    ii = NULL
    c0 = input$ISc0
    c1 = input$ISc1
    T0 = input$IST0
    I0 = input$ISI0
    G0 = input$ISG0
    a = input$ISa
    t = input$ISt
    y = c(1:200)
    for (i in seq_along(y)) {
      ii[i] <- ((input$ISc0 - (input$ISc1*input$IST0) + input$ISI0 + input$ISG0)/ input$ISa)-((1-input$ISc1*(1-input$ISt))/input$ISa)*y[i]
    }
    df <- data.frame(ii, y)
    ggplot(df, aes(x = df$y, y = df$ii))+
      geom_line()+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0)+
      labs(title = "IS - Mercado de Bens",
           x = "Renda",
           y = "Juros")+
      xlim(-1, 200)+
      ylim(-1,18000)
  })
  
  
  output$LMgraph <- renderPlot({
    ii = NULL
    l0 = input$LMl0
    l1 = input$LMl1
    ms0p = input$LMms0p
    vt = input$LMvt
    y = c(1:200)
    for (i in seq_along(y)) {
      ii[i] <- c((1/input$LMl1)*(input$LMl0-input$LMms0p)+(1/input$LMl1)*(1/input$LMvt)*y[i])
    }
    df <- data.frame(ii, y)
    ggplot(df, aes(x = df$y, y = df$ii))+
      geom_line()+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0)+
      labs(title = "LM - Mercado Monetário",
           x = "Renda",
           y = "Juros")+
      xlim(-1, 200)+
      ylim(-1,8000)
  })
  
  output$ISLMgraph <- renderPlot({
    ISii = NULL
    ISc0 = input$ISLMc0
    ISc1 = input$ISLMc1
    IST0 = input$ISLMT0
    ISI0 = input$ISLMI0
    ISG0 = input$ISLMG0
    ISa = input$ISLMa
    ISt = input$ISLMt
    LMii = NULL
    LMl0 = input$ISLMl0
    LMl1 = input$ISLMl1
    LMms0p = input$ISLMms0p
    LMvt = input$ISLMvt
    y = c(1:2000)
    for (i in seq_along(y)) {
      ISii[i] <- ((input$ISLMc0 - (input$ISLMc1*input$ISLMT0) + input$ISLMI0 + input$ISLMG0) / input$ISLMa)-((1-input$ISLMc1*(1-input$ISLMt))/input$ISLMa)*y[i]
    }
    for (i in seq_along(y)) {
      LMii[i] <- c((1/input$ISLMl1)*(input$ISLMl0-input$ISLMms0p)+(1/input$ISLMl1)*(1/input$ISLMvt)*y[i])
    }
    
    dfISLM <- data.frame(ISii, LMii, y)
    
    ggplot(dfISLM)+
      geom_line(aes(x = dfISLM$y, y = dfISLM$ISii), size =1.6)+
      geom_line(aes(x = dfISLM$y, y = dfISLM$LMii), size =1.6)+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0)+
      labs(title = "IS - LM",
           x = "Renda",
           y = "Juros")+
      xlim(-1, 1000)+
      ylim(-1,18000)+
      theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 10, face = "bold"),
            plot.caption = element_text(size = 10, face = "bold"),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  
  
  
  SERIEOBraw <- reactive({
    SOBraw1 <- Quandl::Quandl(paste0("BCB/", input$SOB), type = "raw", collapse = "monthly", api_key = "gGdvN9gXsx9hxHMTWPNL")
    SOBraw1 <- SOBraw1[seq(dim(SOBraw1)[1],1),]
  })
  
  SERIEOBts <- reactive({
    TS <- ts(SERIEOBraw()[,2], start = c(as.numeric(substring(SERIEOBraw()[1,1],1,4)), as.numeric(substring(SERIEOBraw()[1,1],6,7))), frequency = 12)
  })
  
  
  output$PIFOB <- renderTable({
    PIF <- tibble("Período Inicial" = paste(substring(SERIEOBraw()[input$SLDOB[1],1],6,7), "/", substring(SERIEOBraw()[input$SLDOB[1],1],1,4)),
                  "-" = paste("-"),
                  "Período Final" = paste(substring(SERIEOBraw()[input$SLDOB[2],1],6,7), "/", substring(SERIEOBraw()[input$SLDOB[2],1],1,4)))
  })
  
  output$GLOB <- renderPlotly({
    GLST <- autoplot(ts(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]], start = c(as.numeric(substring(SERIEOBraw()[input$SLDOB[1],1],1,4)), as.numeric(substring(SERIEOBraw()[input$SLDOB[1],1],6,7))), frequency = 12))+
      labs(title = "Gráfico de linha da série temporal",
           x = "Tempo",
           y = "Y")
    if(is.null(input$MARKOB)){
      GLST
    } else if(all(c("m1", "m2") %in% input$MARKOB)){
      GLST <- GLST +
        geom_hline(yintercept = mean(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]]), color = "red")+
        geom_hline(yintercept = median(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]]), color = "blue")
    } else if(input$MARKOB == "m1" & input$MARKOB != "m2"){
      GLST <- GLST + geom_hline(yintercept = mean(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]]), color = "red")
    } else if(input$MARKOB != "m1" & input$MARKOB == "m2"){
      GLST <- GLST + geom_hline(yintercept = median(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]]), color = "blue")
    }
    GLST
  })
  
  
}


shinyApp(ui = ui, server = server)
#runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)






