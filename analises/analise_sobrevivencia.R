library(survival)
library(ggsurvfit)
library(readr)
library(dplyr)
library(survminer)
library(ggplot2)
library(ggsci)
library(survivalsvm)


# Carregando dados
dados <- read_csv("data/dados_tratados.csv") |>
  select(-c(`...1`, id_aluno))

# Apenas aqueles que formaram ou não concluíram o curso
dados_formados <- dados |>
  filter(forma_evasao == "'Formado'" | forma_evasao == "'Sem evasão'")

dados_quit <- dados |>
  filter(forma_evasao != "'Formado'")

################# CURVAS DE KAPLAN-MEIER GERAIS

# Criar objeto de sobrevivência
fit <- survfit(Surv(anos_ate_evasao, status) ~ 1, data = dados)

# Plotar curva de Kaplan-Meier
ggsurvplot_geral <- ggsurvfit(fit) +
  labs(
    x = "Tempo até evasão (anos)",
    y = "Probabilidade de permanência"  ) +
  add_confidence_interval() +
  theme_minimal()

print(ggsurvplot_geral)

ggsave("analises/figuras/kaplan_meier_geral.png", plot = ggsurvplot_geral, 
       width = 8, height = 6, dpi = 300)

# Criar objeto de sobrevivência
fit_quit <- survfit(Surv(anos_ate_evasao, status) ~ 1, data = dados_quit)

# Plotar curva de Kaplan-Meier
ggsurvplot_quit <- ggsurvfit(fit_quit) +
  labs(
    x = "Tempo até evasão  sem conclusão do curso (anos)",
    y = "Probabilidade de permanência"  ) +
  add_confidence_interval() +
  theme_minimal()

print(ggsurvplot_quit)

ggsave("analises/figuras/kaplan_meier_desistentes.png", plot = ggsurvplot_quit, 
       width = 8, height = 6, dpi = 300)

# Criar objeto de sobrevivência
fit_formados <- survfit(Surv(anos_ate_evasao, status) ~ 1, data = dados_formados)

# Plotar curva de Kaplan-Meier
ggsurvplot_formados <- ggsurvfit(fit_formados) +
  labs(
    x = "Tempo até evasão  por formatura (anos)",
    y = "Probabilidade de permanência"  ) +
  add_confidence_interval() +
  theme_minimal()

print(ggsurvplot_formados)

ggsave("analises/figuras/kaplan_meier_formados.png", plot = ggsurvplot_formados, 
       width = 8, height = 6, dpi = 300)

################### MODELAGEM PARA O TEMPO DE SOBREVIVÊNCIA

set.seed(2402)  

# Criar índice para divisão
indices <- sample(1:nrow(dados), size = floor(0.8 * nrow(dados)), replace = FALSE)

# Tratamento de valores ausentes

imputar_dados <- function(dados) {
  # Passo 1: Identificar e remover variáveis com >50% NAs
  limiar_remocao <- 0.5
  vars_para_remover <- sapply(dados, function(x) {
    sum(is.na(x)) / length(x) > limiar_remocao
  }) %>% which() %>% names()
  
  if (length(vars_para_remover) > 0) {
    warning(paste("Variáveis removidas (mais de 50% NAs):", 
                  paste(vars_para_remover, collapse = ", ")))
    dados <- dados %>% select(-all_of(vars_para_remover))
  }
  
  # Passo 2: Imputação conforme regras específicas
  dados_imputados <- dados |>
    mutate(across(
      .cols = everything(),
      .fns = ~ {
        nome_var <- cur_column()
        
        if (is.numeric(.x)) {
          # Regra 1: Variáveis que começam com "numero_tentativas"
          if (startsWith(nome_var, "numero_tentativas")) {
            ifelse(is.na(.x), 0, .x)
          } 
          # Regra 2: Variáveis que começam com "media"
          else if (startsWith(nome_var, "media")) {
            ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)
          }
          # Regra 3: Demais variáveis numéricas (mediana)
          else {
            ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)
          }
        } 
        else {
          # Regra 4: Variáveis que começam com "fez" (moda)
          if (startsWith(nome_var, "fez")) {
            moda <- names(sort(table(.x), decreasing = TRUE))[1]
            ifelse(is.na(.x), moda, .x)
          }
          # Regra 5: Demais variáveis categóricas (moda)
          else {
            moda <- names(sort(table(.x), decreasing = TRUE))[1]
            ifelse(is.na(.x), moda, .x)
          }
        }
      }
    ))
  
  return(dados_imputados)
}

dados$cra <- as.numeric(gsub(",", ".", dados$cra))


# Aplicando a função
dados <- imputar_dados(dados) |>
  select(
    forma_evasao,
    cra,
    forma_ingresso,
    cotista,
    status,
    anos_ate_evasao,
    fez_calculo_i,
    fez_calculo_ii,
    fez_programacao_i,
    fez_introducao_a_ciencia_da_computacao,
    fez_aspectos_teoricos_da_computacao,
    fez_elementos_de_logica_digital,
    fez_programacao_ii,
    numero_tentativas_calculo_i,
    numero_tentativas_calculo_ii,
    numero_tentativas_programacao_i,
    numero_tentativas_introducao_a_ciencia_da_computacao,
    numero_tentativas_aspectos_teoricos_da_computacao,
    numero_tentativas_elementos_de_logica_digital,
    numero_tentativas_programacao_ii,
    media_final_calculo_i_1,
    media_final_programacao_i_1,
    media_final_elementos_de_logica_digital_1,
    media_final_programacao_ii_1
  )

# Dividir os dados
treino <- dados[indices, ]
teste <- dados[-indices, ]

formula_dados <- as.formula(
  paste0("Surv(anos_ate_evasao, status) ~ ",
         paste(setdiff(names(treino), c("anos_ate_evasao", "status")), collapse = " + "))
)

svm_add <- survivalsvm(formula_dados,
                       data = treino,
                       type = "regression",
                       gamma.mu = 1,
                       kernel = "add_kernel")

pred_svm_add <- predict(svm_add, newdata = teste)$predicted

cindex_svm_add <- concordance(Surv(teste$anos_ate_evasao, teste$status) ~ as.vector(pred_svm_add))$concordance
cindex_svm_add


