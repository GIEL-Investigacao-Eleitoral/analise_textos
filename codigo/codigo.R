#-------------------------------------------------
# Texto como dado (text as data)
# prof. Steven Ross
#-------------------------------------------------

Poeminho_do_Contra = c("Todos esses que aí estão",
                       "Atravancando meu caminho",
                       "Eles passarão...",
                       "Eu passarinho!")

Poeminho_do_Contra

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

#------------------------------------------------------------
#------------------------------------------------------------
# Estudo de caso 1 (nuvem de palavras do grupo de foco)
# Importar
# Transformar em tokens
#------------------------------------------------------------
#------------------------------------------------------------

library(dplyr)
library(rio)
link_dados = 'https://github.com/GIEL-Investigacao-Eleitoral/analise_textos/raw/main/dados/excel/transcricao_grupo_de_foco.xlsx'
grupo_foco  = rio::import(file = link_dados) 

grupo_foco %>% 
  glimpse()

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

