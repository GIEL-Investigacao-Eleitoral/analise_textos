

library(stopwords)
palavras_banidas = stopwords("pt")
palavras_banidas = tibble(palavras_banidas)
palavras_banidas = palavras_banidas %>% rename(word=palavras_banidas)



#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

load(url("https://github.com/GIEL-Investigacao-Eleitoral/analise_textos/raw/main/dados/rdata/banco_tweets.RData"))

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
contagem_bigramas


library(igraph)
library(ggraph)
set.seed(12345)

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
