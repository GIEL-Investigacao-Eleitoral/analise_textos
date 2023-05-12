#-------------------------------------------------
# Texto como dado (text as data)
# prof. Steven Ross
#-------------------------------------------------

library(speechbr)
library(tibble)
library(stopwords)

discursos = speech_data(
  keyword = "desigualdade",
  start_date = "2023-01-01", 
  end_date = "2023-05-01")

palavras_banidas = stopwords("pt")
palavras_banidas = tibble(palavras_banidas)
palavras_banidas = palavras_banidas %>% rename(word=palavras_banidas)

discursos %>%
  rowid_to_column("id") %>%
  select(id, discurso) %>%
  unnest_tokens(word, discurso) %>%
  anti_join(palavras_banidas) %>%
  count(word, sort = TRUE) %>%
  wordcloud2()

#-------------------------------------------------------
#-------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("hadley/emo")
#-------------------------------------------------------
#-------------------------------------------------------

library(emo)
library(tidyr)
library(dplyr)
library(reactable)

load(url("https://github.com/GIEL-Investigacao-Eleitoral/analise_textos/raw/main/dados/rdata/banco_tweets.RData"))
table(banco_tweets$veiculo)

emoji<-banco_tweets %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(50) 

emoji %>%  reactable()

#Top emoji por veiculo
emoji_por_veiculo<-banco_tweets %>%
  group_by(veiculo) %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(5)

emoji_por_veiculo %>%  reactable()


