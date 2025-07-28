library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)


dados_aux <- read_delim("CienciaDaComputacaoCT_com_cripto.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE,
                     locale = locale(encoding = "Latin1")) |>
  clean_names() |>
  filter(!is.na(id_curso_aluno)) |>
  group_by(nome_cripto, matr_cripto) |>
  mutate(id_aluno = cur_group_id()) |>
  ungroup() |>
  select(-c(id_curso_aluno, cod_curso, nome_unidade, nome_docente,
            matr_cripto, nome_cripto, cod_disciplina, titulacao_docente, 
            sigla_situacao, situacao_item, sigla_semestre, cod_forma_evasao,
            ))

########### Organizando informações do aluno

dados_alunos <- dados_aux |>
  select(id_aluno, forma_evasao, ano_evasao, cra, crn, semestre_evasao, ano_ingresso,
         forma_ingresso, cotista) |>
  distinct() |>
  mutate(status = ifelse(forma_evasao == "'Sem evasão'", 0, 1),
  semestres_ate_evasao = as.numeric(substr(semestre_evasao, 2, 2)))


########### Organizando informações das disciplinas feitas

todos <- expand_grid(
  id_aluno = unique(dados_aux$id_aluno),
  nome_disciplina = unique(dados_aux$nome_disciplina)
)

dados_completos <- todos |>
  full_join(dados_aux |> select(id_aluno, nome_disciplina, media_final, descricao_situacao,
                                descricao_semestre, ano,
                                num_versao,
                                descr_estrutura, ch_total, tipo_disciplina), 
            by = c("id_aluno", "nome_disciplina")) |>
  mutate(feita = ifelse(!is.na(media_final), "sim", "não"))

contagem_disciplinas <-  dados_aux |>
  count(id_aluno, nome_disciplina, name = "n_vezes")

dados_completos <- dados_completos |> left_join(contagem_disciplinas) |>
  mutate(
    n_vezes = ifelse(descr_estrutura %in% c("'03-Atividades Complementares'", "'04-Atividades Complementares'"),
                     NA, n_vezes)
  )

# Padroniza nomes de disciplina
dados_completos <- dados_completos |>
  mutate(
    nome_disciplina = iconv(nome_disciplina, from = "UTF-8", to = "ASCII//TRANSLIT"), # remove acentos
    nome_disciplina = str_replace_all(nome_disciplina, "[\\r\\n\\t]", ""),             # remove \r, \n, \t
    nome_disciplina = str_squish(nome_disciplina),                                     # tira espaços duplicados
    nome_disciplina = str_to_lower(nome_disciplina),                                   # tudo minúsculo
    nome_disciplina = str_replace_all(nome_disciplina, "[^a-z0-9]+", "_"),             # substitui símbolos por _
    nome_disciplina = str_replace_all(nome_disciplina, "_$", "")                       # remove "_" no final
  )


# Calcula a média obtida em cada disciplina (jncluindo feitas mais de uma vez)
dados_medias <- dados_completos |>
  filter(descricao_semestre %in% c("'1º Semestre'", "'2º Semestre'"), feita == "sim") |>
  group_by(id_aluno, nome_disciplina) |>
  mutate(tentativa = row_number()) |>
  ungroup() |>
  mutate(nome_coluna = paste0("media_final", nome_disciplina, "_", tentativa)) |>
  select(id_aluno, nome_coluna, media_final) |>
  pivot_wider(names_from = nome_coluna, values_from = media_final)

# Cria indicador binário para todas as disciplinas
dados_indicadores <- dados_completos |>
  filter(descricao_semestre %in% c("'1º Semestre'", "'2º Semestre'"), feita == "sim") |>
  distinct(id_aluno, nome_disciplina, feita) |>
  mutate(nome_coluna = paste0("fez", nome_disciplina)) |>
  select(id_aluno, nome_coluna, feita) |>
  pivot_wider(names_from = nome_coluna, values_from = feita, values_fill = NA)

# Indica quantas vezes cada aluno fez cada disciplina
dados_tentativas <- dados_completos |>
  filter(descricao_semestre %in% c("'1º Semestre'", "'2º Semestre'"), feita == "sim") |>
  group_by(id_aluno, nome_disciplina) |>
  summarise(numero_tentativas = n(), .groups = "drop") |>
  mutate(nome_coluna = paste0("numero_tentativas", nome_disciplina)) |>
  select(id_aluno, nome_coluna, numero_tentativas) |>
  pivot_wider(names_from = nome_coluna, values_from = numero_tentativas, values_fill = 0)

# UNINDO TUDO
dados_final <- dados_alunos |>
  left_join(dados_indicadores, by = "id_aluno") |>
  left_join(dados_medias, by = "id_aluno") |>
  left_join(dados_tentativas, by = "id_aluno")


write.csv(dados_final, "dados_tratados.csv")

