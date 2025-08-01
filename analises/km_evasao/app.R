# https://marianacfreitas.shinyapps.io/km_evasao/

library(shiny)
library(shinydashboard)
library(survival)
library(ggsurvfit)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = ""),
  dashboardSidebar(
    selectInput("forma_evasao",
                label = "Filtrar por forma de evasão:",
                choices = c("Todas as formas de evasão" = "todas",
                            "Evasão por formatra" = "formaturas",
                            "Evasão sem conclusão do curso" = "evasoes"),
                selected = "todas"),
    
    selectInput("variavel",
                label = "Variável para estratificação:",
                choices = c(
                            "Cotista" = "cotista",
                            "Número de tentaivas Aspectos Teóricos" = "numero_tentativas_aspectos_teoricos_da_computacao",
                            "Número de tentaivas Introdução à Ciência da Computação" = "numero_tentativas_introducao_a_ciencia_da_computacao",
                            "Forma de ingresso" = "forma_ingresso",
                            "Número de tentaivas Cálculo I" = "numero_tentativas_calculo_i",
                            "Número de tentaivas Programação I" = "numero_tentativas_programacao_i",
                            "Número de tentaivas Aspectos Teóricos" = "numero_tentativas_aspectos_teoricos_da_computacao",
                            "Número de tentaivas Lógica I" = "numero_tentativas_logica_para_computacao_i",
                            "Número de tentaivas Lógica Digital" = "numero_tentativas_elementos_de_logica_digital",
                            "Número de tentaivas Programação II" = "numero_tentativas_programacao_ii",
                            "Número de tentaivas Cálculo II" = "numero_tentativas_calculo_ii",
                            "Número de tentaivas Introdução ao Computador" = "numero_tentativas_introducao_ao_computador",
                            "Número de tentaivas Aritmética I" = "numero_tentativas_aritmetica_i",
                            "Número de tentaivas Geometria Analítica" = "numero_tentativas_geometria_analitica"),
                selected = "cotista")
  ),
  dashboardBody(
    fluidRow(
      box(width = 12, title = "Como Interpretar",
          status = "info",
          solidHeader = TRUE,
          p("Os dados utilizados a seguir contém informações sobre o tempo até evasão de alunos do curso de Ciências da Computação da Universidade Federal do Espírito Santo (UFES). A base de dados é composta por informações de 933 alunos do curso entre 2005 a 2025, contendo informações sobre as formas de ingresso dos alunos, bem como informações sobre as disciplinas cursadas. Evasão é considerada tanto a conclusão do curso, quanto aqueles que saíram do curso sem terminá-lo, já os alunos que ainda estão no curso são considerados dados censurados, já que eventualmente saírão do curso por um desses motivos. Para melhor compreemsão das análises, é possível filtrar os casos para apenas os que saíram por formatura ou os que saíram sem concluir o curso, porém aqueles que ainda estão cursando são considerados nos dois casos, já que não sabemos qual dos eventos irá apresentar.
"),
          p("A curva mostra a probabilidade de um aluno permanecer no curso ao longo do tempo:"),
          tags$ul(
            tags$li(strong("Quedas na curva:"), "indicam períodos com mais evasões/formaturas"),
            tags$li(strong("Curvas separadas:"), "mostram diferenças entre grupos"),
            tags$li(strong("Teste Log-Rank (p-valor):"), "valores abaixo de 0,05 indicam diferença real entre os grupos")
          )      ),
      box(width = 12, plotlyOutput("km_plot")),
      box(width = 12, 
          title = "Estatísticas Descritivas",
          verbatimTextOutput("estatisticas"))
    )
  )
)

server <- function(input, output) {
  
  # Carregar dados
  dados <- reactive({
    read_csv("dados_tratados.csv") 
  })
  
  # Filtrar dados conforme seleção
  dados_filtrados <- reactive({
    req(dados())
    
    df <- dados()
    
    if(input$forma_evasao == "formaturas") {
      df <- df %>% filter(forma_evasao %in% c("'Formado'", "'Sem evasão'"))
    } else if(input$forma_evasao == "evasoes") {
      df <- df %>% filter(forma_evasao != "'Formado'")
    }
    
    # Converter variáveis de tentativas para fator
    if(grepl("tentativas", input$variavel)) {
      df[[input$variavel]] <- as.factor(df[[input$variavel]])
    }
    
    return(df)
  })
  
  # Criar curva de Kaplan-Meier
  output$km_plot <- renderPlotly({
    req(dados_filtrados())
    
    formula <- as.formula(paste("Surv(anos_ate_evasao, status) ~", input$variavel))
    fit <- survfit2(formula, data = dados_filtrados())
    
    # Obter p-valor do teste log-rank
    p_valor <- survdiff(formula, data = dados_filtrados())$pvalue
    p_text <- ifelse(p_valor < 0.001, "p < 0.001", paste("p =", round(p_valor, 3)))
    
    # Obter nome amigável da variável
    variavel_nome <- names(which(
      c("Forma de ingresso" = "forma_ingresso",
        "Cotista" = "cotista",
        "Número de tentaivas Cálculo I" = "numero_tentativas_calculo_i",
        "Número de tentaivas Programação I" = "numero_tentativas_programacao_i",
        "Número de tentaivas Aspectos Teóricos" = "numero_tentativas_aspectos_teoricos_da_computacao",
        "Número de tentaivas ICC" = "numero_tentativas_introducao_a_ciencia_da_computacao",
        "Número de tentaivas Lógica I" = "numero_tentativas_logica_para_computacao_i",
        "Número de tentaivas Lógica Digital" = "numero_tentativas_elementos_de_logica_digital",
        "Número de tentaivas Programação II" = "numero_tentativas_programacao_ii",
        "Número de tentaivas Cálculo II" = "numero_tentativas_calculo_ii",
        "Número de tentaivas Introdução ao Computador" = "numero_tentativas_introducao_ao_computador",
        "Número de tentaivas Aritmética I" = "numero_tentativas_aritmetica_i",
        "Número de tentaivas Geometria Analítica" = "numero_tentativas_geometria_analitica") == input$variavel
    ))
    
    # Criar gráfico
    p <- ggsurvfit(fit) +
      labs(
        x = "Tempo até evasão (anos)",
        y = "Probabilidade de permanência",
        title = paste("Curva de Kaplan-Meier por", variavel_nome),
        subtitle = paste("Teste Log-Rank:", p_text),
        color = "Grupos"
      ) +
      add_risktable() +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")
    
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # Estatísticas descritivas
  output$estatisticas <- renderPrint({
    req(dados_filtrados())
    
    cat("=== RESUMO DOS DADOS ===\n\n")
    cat("Total de observações:", nrow(dados_filtrados()), "\n")
    cat("Eventos (evasões):", sum(dados_filtrados()$status), "\n")
    cat("Censuras:", sum(dados_filtrados()$status == 0), "\n\n")
    
    cat("Distribuição da variável estratificadora:\n")
    print(table(dados_filtrados()[[input$variavel]]))
  })
}

shinyApp(ui, server)