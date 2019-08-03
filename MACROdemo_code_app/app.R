rm(list = ls())

# Libraries ---------------------------------------------------------------

library("shiny")
library("shinydashboard")
library("shinythemes")
library("shinyWidgets")  
library("tidyverse")
library("ggplot2")
library("plotly")
library("Quandl")
library("forecast")
library("tseries")
library("ggfortify")
library("moments")
library("rsconnect")
library("devtools")
library("DT")
library("packrat")
library("httr")
library("RCurl")


ui <- shinyUI(
    navbarPage("MACROdemo",
               tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "MACROdemo.css")),
               tabPanel("Apresentação",
                        br(),
                        br(),
                        br(),
                        br(),
                        h2("¬Oi, sou Guilherme Viegas, e este [e o MACROdemo kk")),
               navbarMenu("Gráficos innterativos",
                          tabPanel("Função Consumo",
                                   fluidPage(
                                       column(2,
                                              br(),
                                              br(),
                                              br(),
                                              br(),
                                              br(),
                                              br(),
                                              numericInput(inputId = "C0",label = "Consumo autônomo", value = 30, min = 1, max = 100, step = 10),
                                              numericInput(inputId = "C1", label = "Propensão Marginal a Consumir", value = 0.6, min = 0, max = 1, step = 0.1)),
                                       column(10,
                                              plotOutput(outputId = "FC", height = "800px")
                                              )
                                   )),
                          tabPanel("IS-LM",
                                   column(2,
                                          tabsetPanel(
                                              tabPanel("IS_1",
                                                       column(12,
                                                              br(),
                                                              numericInput("ISLMc0_1", "Consumo Autonomo", min = 0, max = 1000, step = 50, value = 450),
                                                              numericInput("ISLMc1_1", "Propensão Marginal a Consumir", min = 0, max = 1000, step = 0.1, value = 0.8),
                                                              numericInput("ISLMT0_1", "T0", min = 0, max = 1000, step = 50, value = 300),
                                                              numericInput("ISLMI0_1", "I0", min = 0, max = 1000, step = 50, value = 300),
                                                              numericInput("ISLMG0_1", "G0", min = 0, max = 1000, step = 50, value = 300),
                                                              numericInput("ISLMa_1", "a", min = 0, max = 1, step = 0.1, value = 0.1),
                                                              numericInput("ISLMt_1", "t", min = 0, max = 1000, step = 1, value = 3))),
                                              tabPanel("LM_1",
                                                       column(12,
                                                              br(),
                                                              br(),
                                                              br(),
                                                              numericInput("ISLMl0_1", "l0", min = 0, max = 1, step = 0.05, value = 0.9),
                                                              numericInput("ISLMl1_1", "l1", min = 0, max = 1, step = 0.1, value = 0.3),
                                                              numericInput("ISLMms0p_1", "oferta de moeda", min = 0, max = 1000, step = 50, value = 30),
                                                              numericInput("ISLMvt_1", "velocity", min = 0, max = 1000, step = 0.1, value = 0.1))),
                                              tabPanel("IS_2",
                                                       column(12,
                                                              br(),
                                                              numericInput("ISLMc0_2", "Consumo Autonomo", min = 0, max = 1000, step = 50, value = 450),
                                                              numericInput("ISLMc1_2", "Propensão Marginal a Consumir", min = 0, max = 1000, step = 0.1, value = 0.8),
                                                              numericInput("ISLMT0_2", "T0", min = 0, max = 1000, step = 50, value = 300),
                                                              numericInput("ISLMI0_2", "I0", min = 0, max = 1000, step = 50, value = 300),
                                                              numericInput("ISLMG0_2", "G0", min = 0, max = 1000, step = 50, value = 300),
                                                              numericInput("ISLMa_2", "a", min = 0, max = 1, step = 0.1, value = 0.1),
                                                              numericInput("ISLMt_2", "t", min = 0, max = 1000, step = 1, value = 3))),
                                              tabPanel("LM_2",
                                                       column(12,
                                                              br(),
                                                              br(),
                                                              br(),
                                                              numericInput("ISLMl0_2", "l0", min = 0, max = 1, step = 0.05, value = 0.9),
                                                              numericInput("ISLMl1_2", "l1", min = 0, max = 1, step = 0.1, value = 0.3),
                                                              numericInput("ISLMms0p_2", "oferta de moeda", min = 0, max = 1000, step = 50, value = 30),
                                                              numericInput("ISLMvt_2", "velocity", min = 0, max = 1000, step = 0.1, value = 0.1)))
                                          )),
                                   column(10,
                                          plotOutput("ISLMgraph", height = "800px")
                                          )),
                          tabPanel("Panel_M4")),
               navbarMenu("Animações",
                          tabPanel("Panel_M1"),
                          tabPanel("Inflation is a monetary phenomena"),
                          tabPanel("Panel_M3"),
                          tabPanel("Panel_M4")),
               tabPanel("Observatório",
                        fluidPage(
                            theme = "www/MACROdemo.css",
#                             tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #FFF; top: 17px; width: 2px;
#     height: 2px;
#     border: 1px solid #FFF;
#     background: #DDD;
#     border-radius: 27px;
#     -moz-border-radius: 27px;
#     box-shadow: 1px 1px 3px #FFF;
#     cursor: pointer;
# }")),
                            column(2,
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
                                   tableOutput("PIFOB")
                            ),
                            column(10,
                                   tabsetPanel(
                                       tabPanel(
                                           "Gráfico de linha",
                                           plotlyOutput("GLOB", height = "800px")),
                                       tabPanel(
                                           "Gráfico de linhas",
                                           plotlyOutput("GLOBS", height = "800px")),
                                       tabPanel(
                                           "OUTROS",
                                           plotlyOutput("OTOB", height = "800px")),
                                       tabPanel(
                                           "Tabela",
                                           dataTableOutput("TABLEOB"))
                                   )
                            ))
                        ),
               tabPanel("Quiz"),
               tabPanel("Sobre",
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        h2("Autor ", strong("Guilherme Viegas")),
                        br(),
                        h2("guilhermeviegas1993@gmail.com"))
    )
)






# Server ------------------------------------------------------------------


server <- function(input, output) {
    

# Server - Função Consumo -------------------------------------------------

    
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
            geom_line(aes(df$YD, df$C), size =1.3)+
            geom_hline(yintercept = 0)+
            geom_vline(xintercept = 0)+
            labs(title = "Função Consumo",
                 x = "Renda, Y",
                 y = "Consumo, C",
                 caption = "MACROdemo")+
            xlim(-1, 100)+
            ylim(-1,100)+
            theme_minimal()+
            theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
                  axis.title = element_text(size = 25, face = "bold"),
                  plot.caption = element_text(size = 18, face = "bold"),
                  axis.text = element_blank(),
                  axis.ticks = element_blank())
    })
    

# Server - ISLM -----------------------------------------------------------

    
    output$ISLMgraph <- renderPlot({
        ISii_1 = NULL
        ISc0_1 = input$ISLMc0_1
        ISc1_1 = input$ISLMc1_1
        IST0_1 = input$ISLMT0_1
        ISI0_1 = input$ISLMI0_1
        ISG0_1 = input$ISLMG0_1
        ISa_1 = input$ISLMa_1
        ISt_1 = input$ISLMt_1
        LMii_1 = NULL
        LMl0_1 = input$ISLMl0_1
        LMl1_1 = input$ISLMl1_1
        LMms0p_1 = input$ISLMms0p_1
        LMvt_1 = input$ISLMvt_1
        
        ISii_2 = NULL
        ISc0_2 = input$ISLMc0_2
        ISc1_2 = input$ISLMc1_2
        IST0_2 = input$ISLMT0_2
        ISI0_2 = input$ISLMI0_2
        ISG0_2 = input$ISLMG0_2
        ISa_2 = input$ISLMa_2
        ISt_2 = input$ISLMt_2
        LMii_2 = NULL
        LMl0_2 = input$ISLMl0_2
        LMl1_2 = input$ISLMl1_2
        LMms0p_2 = input$ISLMms0p_2
        LMvt_2 = input$ISLMvt_2
        
        y = c(1:2000)
        
        for (i in seq_along(y)) {
            ISii_1[i] <- ((input$ISLMc0_1 - (input$ISLMc1_1*input$ISLMT0_1) + input$ISLMI0_1 + input$ISLMG0_1) / input$ISLMa_1)-((1-input$ISLMc1_1*(1-input$ISLMt_1))/input$ISLMa_1)*y[i]
        }
        for (i in seq_along(y)) {
            LMii_1[i] <- c((1/input$ISLMl1_1)*(input$ISLMl0_1-input$ISLMms0p_1)+(1/input$ISLMl1_1)*(1/input$ISLMvt_1)*y[i])
        }
        
        for (i in seq_along(y)) {
            ISii_2[i] <- ((input$ISLMc0_2 - (input$ISLMc1_2*input$ISLMT0_2) + input$ISLMI0_2 + input$ISLMG0_2) / input$ISLMa_2)-((1-input$ISLMc1_2*(1-input$ISLMt_2))/input$ISLMa_2)*y[i]
        }
        for (i in seq_along(y)) {
            LMii_2[i] <- c((1/input$ISLMl1_2)*(input$ISLMl0_2-input$ISLMms0p_2)+(1/input$ISLMl1_2)*(1/input$ISLMvt_2)*y[i])
        }
        
        dfISLM <- data.frame(ISii_1, LMii_1, y, ISii_2, LMii_2)
        
        # coef(lm(ISii_1 ~ y))[[2]] X + 1Y = coef(lm(ISii_1 ~ y))[[1]]
        # coef(lm(LMii_1 ~ y))[[2]] X + 1Y = coef(lm(LMii_1 ~ y))[[1]]

        M1coef_1 <- matrix(c(coef(lm(ISii_1 ~ y))[[2]],
                           1,
                           coef(lm(LMii_1 ~ y))[[2]],
                           1),
                         nrow = 2,
                         byrow = T)
        
        M1cons_1 <- matrix(c(coef(lm(ISii_1 ~ y))[[1]],
                           coef(lm(LMii_1 ~ y))[[1]]),
                         nrow = 2)
        
        M1coef_2 <- matrix(c(coef(lm(ISii_2 ~ y))[[2]],
                             1,
                             coef(lm(LMii_2 ~ y))[[2]],
                             1),
                           nrow = 2,
                           byrow = T)
        
        M1cons_2 <- matrix(c(coef(lm(ISii_2 ~ y))[[1]],
                             coef(lm(LMii_2 ~ y))[[1]]),
                           nrow = 2)
        
        ggplot(dfISLM)+
            geom_line(aes(x = dfISLM$y, y = dfISLM$ISii_2), size =1.3, color = "#949494")+
            geom_line(aes(x = dfISLM$y, y = dfISLM$LMii_2), size =1.3, color = "#949494")+
            geom_line(aes(x = dfISLM$y, y = dfISLM$ISii_1), size =1.3)+
            geom_line(aes(x = dfISLM$y, y = dfISLM$LMii_1), size =1.3)+
            geom_segment(aes(x = -solve(M1coef_2, M1cons_2)[1], xend = -solve(M1coef_2, M1cons_2)[1], y = 0, yend = solve(M1coef_2, M1cons_2)[2]), linetype = 2, color = "#949494")+
            geom_segment(aes(x = 0, xend = -solve(M1coef_2, M1cons_2)[1], y = solve(M1coef_2, M1cons_2)[2], yend = solve(M1coef_2, M1cons_2)[2]), linetype = 2, color = "#949494")+
            geom_segment(aes(x = -solve(M1coef_1, M1cons_1)[1], xend = -solve(M1coef_1, M1cons_1)[1], y = 0, yend = solve(M1coef_1, M1cons_1)[2]), linetype = 2)+
            geom_segment(aes(x = 0, xend = -solve(M1coef_1, M1cons_1)[1], y = solve(M1coef_1, M1cons_1)[2], yend = solve(M1coef_1, M1cons_1)[2]), linetype = 2)+
            geom_point(aes(x = -solve(M1coef_2, M1cons_2)[1], y = solve(M1coef_2, M1cons_2)[2]), size = 6, color = "#949494")+
            geom_point(aes(x = -solve(M1coef_1, M1cons_1)[1], y = solve(M1coef_1, M1cons_1)[2]), size = 6)+
            geom_segment(aes(x = 0, xend = 0, y = solve(M1coef_1, M1cons_1)[2], yend = solve(M1coef_2, M1cons_2)[2]), size = 1.3)+
            geom_segment(aes(x = -solve(M1coef_1, M1cons_1)[1], xend = -solve(M1coef_2, M1cons_2)[1], y = 0, yend = 0), size = 1.3)+
            geom_hline(yintercept = 0)+
            geom_vline(xintercept = 0)+
            labs(title = "IS - LM",
                 x = "Renda",
                 y = "Juros",
                 caption = "MACROdemo")+
            xlim(-1, 1000)+
            ylim(-1,18000)+
            theme_minimal()+
            theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
                  #axis.title = element_text(size = 25, face = "bold"),
                  plot.caption = element_text(size = 18, face = "bold"),
                  #axis.text = element_blank(),
                  axis.ticks = element_blank())
    })
    

# Server - Observatório ---------------------------------------------------

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
        GLST + theme_minimal()
    })    
    
    

}

shinyApp(ui = ui, server = server)
