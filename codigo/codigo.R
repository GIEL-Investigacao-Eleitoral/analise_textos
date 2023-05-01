#-------------------------------------------------
# Texto como dado (text as data)
# prof. Steven Ross
#-------------------------------------------------

Poeminho_do_Contra = c("Todos esses que aí estão",
                       "Atravancando meu caminho",
                       "Eles passarão...",
                       "Eu passarinho!")

length(Poeminho_do_Contra)

Poeminho_do_Contra[3]
Poeminho_do_Contra[4]

toupper(Poeminho_do_Contra)
tolower(Poeminho_do_Contra)

#------------------------------------------------------------
library(corpus)
text <- "love loving lovingly loved lover lovely love"
text

# Stemming é o ato de remover inflexões de uma palavra 
text_tokens(text) 
text_tokens(text, stemmer = "en") # english stemmer

texto <- "caminhar caminhando caminhou andou andar andando"
texto
text_tokens(texto) 
text_tokens(texto, stemmer = "pt") 

#remotes::install_github("DATAUNIRIO/lemmar")

library(lemmar)

lemmatize_pt(texto)
lemmatize_pt(Poeminho_do_Contra)


#-----------------------------------------------------------------------------
library(readtext)
dom = readtext("C:/Users/Hp/Documents/GitHub/minicurso_analise_texto/dados/txt/Dom_Casmurro.txt")

library(tidytext)
library(dplyr)
tidy_dom = dom %>%
  unnest_tokens(word, text)


tidy_dom %>% tibble() %>%
  count(word, sort = TRUE) 


library(stopwords)
palavras_banidas = stopwords("pt")
palavras_banidas = tibble(palavras_banidas)
palavras_banidas = palavras_banidas %>% rename(word=palavras_banidas)

tidy_dom %>%
  anti_join(palavras_banidas) %>% 
  tibble() %>%
  count(word, sort = TRUE) 

library(stringi)
tidy_dom$word = stri_trans_general(str = tidy_dom$word, id = "Latin-ASCII")

tidy_dom %>%
  anti_join(palavras_banidas) %>% 
  tibble() %>%
  count(word, sort = TRUE) 


TF = tidy_dom %>%
  anti_join(palavras_banidas) %>% 
  tibble() %>%
  count(word, sort = TRUE)  %>% 
  print(n=50)


library(wordcloud2)
wordcloud2(demoFreq)
#wordcloud2(TF)

library(ggplot2)
tidy_dom %>%
  anti_join(palavras_banidas) %>% 
  tibble() %>%
  count(word, sort = TRUE) %>%
  filter(n > 150) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill='red') +
  labs(y = NULL)

# Estudo de caso 1 (nuvem de palavras do grupo de foco)
# Importar
# Transformar em tokens



library(readxl)
library(dplyr)
grupo_foco <- read_excel("~/GitHub/analise_textos/dados/excel/transcricao_grupo_de_foco.xlsx")
grupo_foco = grupo_foco %>% pull(texto) %>% tibble() 
colnames(grupo_foco) = 'texto'
tidy_grupo_foco =  grupo_foco %>%
  unnest_tokens(word, texto)


TFGF = tidy_grupo_foco %>%
  anti_join(palavras_banidas) %>% 
  tibble() %>%
  count(word, sort = TRUE)  %>% 
  print(n=50)  

wordcloud2(TFGF)



#Bigramas 
bigrama_dom <- dom %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrama_dom %>%   tibble()  %>%
  count(bigram, sort = TRUE)

library(tidyr)
bigramas <- bigrama_dom %>% tibble() %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigramas <- bigramas %>%
  filter(!word1 %in% palavras_banidas$word) %>%
  filter(!word2 %in% palavras_banidas$word)

contagem_bigramas <- bigramas %>% 
  count(word1, word2, sort = TRUE)

contagem_bigramas %>% filter(word1 == "capitú")

#Trigramas
trigrama_dom <- dom %>%  tibble()  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

trigrama_dom %>% count(trigram, sort = TRUE)

#n-gramas
library(igraph)
rede_bigrama <- contagem_bigramas %>%
  filter(n > 20) %>%
  graph_from_data_frame()

rede_bigrama

library(ggraph)
set.seed(12345)

ggraph(rede_bigrama) +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) 

#-----------------------------------------------------------------------



#Bigramas de tweets 
bigrama_tweets <- banco_tweets$text %>% tibble() 
colnames(bigrama_tweets) = 'text'
bigrama_tweets = bigrama_tweets %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrama_tweets = bigrama_tweets %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrama_tweets <- bigrama_tweets %>%
  filter(!word1 %in% palavras_banidas$word) %>%
  filter(!word2 %in% palavras_banidas$word)


contagem_bigramas = bigrama_tweets %>%
  count(word1, word2, sort = TRUE)


rede_bigrama <- contagem_bigramas %>%
  filter(n > 150) %>%
  graph_from_data_frame()

rede_bigrama

ggraph(rede_bigrama) +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) 


# tf-idf 

table(banco_tweets$veiculo)

tweets_onibus = banco_tweets %>% filter(veiculo=='onibus')
tweets_uber = banco_tweets %>% filter(veiculo=='uber')

tweets_onibus = tweets_onibus$text %>% tibble()
tweets_uber = tweets_uber$text %>% tibble()

colnames(tweets_onibus) = 'text'
colnames(tweets_uber) = 'text'

tweets_onibus = tweets_onibus %>% unnest_tokens(word, text)

tweets_uber   = tweets_uber %>% unnest_tokens(word, text)

tweets_onibus = tweets_onibus %>% anti_join(palavras_banidas) %>%   count(word, sort = TRUE)
tweets_uber = tweets_uber %>% anti_join(palavras_banidas) %>%   count(word, sort = TRUE)
tweets_onibus$veiculo = 'onibus'
tweets_uber$veiculo = 'uber'


tweets = tweets_onibus %>% add_row(tweets_uber)
tweets_tf_idf <- tweets %>% bind_tf_idf(word, veiculo, n)

tweets_tf_idf %>%
  arrange(desc(tf_idf))

library(forcats)

tweets_tf_idf %>%
  group_by(veiculo) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = veiculo)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~veiculo, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
