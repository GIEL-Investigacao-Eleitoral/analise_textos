
# install.packages("devtools")
# devtools::install_github("hadley/emo")
       
library(emo)
library(tidyr)
library(dplyr)
library(reactable)

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


