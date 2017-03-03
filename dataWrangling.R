# data wrangling tutorial:

library(plyr)
library(dplyr)
library(tidyr)

# packages plyr, dplyr, tidyr

flags <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/flags/flag.data", sep=",")

atributos <- c("name", "landmass", "zone", "area", "population", "language", "religion", "bars", "stripes", "colours",
               "red", "green", "blue", "gold", "white", "black", "orange", "mainhue", "circles", "crosses", "saltires",
               "quarters", "sunstars", "crescent", "triangle", "icon", "animate", "text", "topleft", "botright")

colnames(flags) <- atributos
head(flags)
View(flags)
factorCols <- c(2,3,6,7) # colunas correspondentes a continente, zona, idioma e religião serão indicados como fatores
flags[factorCols] = flags[factorCols] %>% lapply(., factor)
sapply(flags, class)

# temos o data frame, agora vamos seguir os passos do cheatsheet

# transformar os dados para a classe tbl:

flags <- tbl_df(flags)
glimpse(flags)
View(flags)
distinct(flags)

# removendo a linha repetida "Argentine"
for(i in 1:nrow(flags)){
  if(flags[i,1] == "Argentine")
    flags <- flags[-i, ]
}

View(flags)

# como o objetivo aqui é mostrar como trabalhar com o pacote, selecionaremos uma parte dos dados
# selecionar aleatoriamente uma fração das linhas dos dados
sample_frac(flags, 0.8, replace=FALSE)

# selecionar aleatoriamente um número de linhas dos dados
flags = sample_n(flags, 15, replace=FALSE)


# reshaping data

# função gather: coloca os dados numa apresentação chave/valor, ou seja,
# coloca os atributos desejados na coluna "key" e seu respectivo valor na coluna "value":
gathered_data = flags %>% gather(., key="key", value = "value", -name)
View(gathered_data)

# então, as funções spread e gather são complementares. Uma desfaz o que a outra faz.
# A função spread pega as chaves e as transforma em atributos - como tínhamos anteriormente.
spread_data = gathered_data %>% spread(., key, value)

# função arrange(): ordena as instâncias de acordo com os valores de um atributo. Exemplo:
# ordenar de acordo com a área do país 

#(do maior para o menor)
arrange_desc = flags %>% arrange(., desc(area))
head(arrange_desc)

#(do menor para o maior)
arrange = flags %>% arrange(., desc(area))
head(arrange)

# função filter
head(flags %>% filter(., population > 40 | area > 300), 5)
head(flags %>% filter(., population > 40,area > 300), 5)
head(flags %>% filter(., red == 1, white == 1)) # all countries that have yellow and green in its flags

# note que a função subset é bem mais rápida:
library(microbenchmark)
microbenchmark(filter = filter(flags, population > 40 | area > 300), 
               subset = subset(flags, population>40 | area>300))
microbenchmark(filterAND = filter(flags, population > 40,area > 300), 
               subsetAND = subset(flags, population>40,area>300))

## Summarise data

flags %>% summarise(., avg = mean(area))  # qual a área média?
flags %>% summarise(., sd = sd(area))     # qual o desvio padrão das áreas?
flags %>% summarise(., max = max(area))   # qual a área máxima?

View(flags %>% summarise_each(., funs(mean)))

flags %>% summarise(., meanArea = mean(area), meanPop = max(population),meanBlack = mean(black), 
                    meanBlue = mean(blue), meanGold = mean(gold))

# ddply function (agupar segundo alguma variável)

flags %>% ddply(., .(landmass)) 

# making new variables

# vamos supor que queremos saber quais os países que possuem verde e amarelo 
# sua bandeira. Podemos utilizar a função mutate():

# antes eu crio uma função que compara os vetores das cores:
pairsVec = function(x,y){
  res = NULL;
  for(i in 1:length(x)){
    if(x[i] == 1 && y[i] == 1)
      res[i] <- 1
    else
      res[i] <- 0
  }
  res
}

a=c(0,0,0,1,1,1); b=c(1,0,0,1,1,1)
pairsVec(a,b)
# ... e colocamos a nova variável como a última coluna dos dados:
View(mutate(flags, goldGrenn = pairsVec(gold, green), redWhite = pairsVec(red, white)))

# aplicar uma função "janela" para cada uma das variáveis (atributos)
mutate_each(flags, funs(cummean))

# criar novas variáveis por grupos:

GoldGrennFlags = flags %>% ddply(., .(landmass)) %>% mutate(., goldGrenn = pairsVec(gold, green))
# podemos, por exemplo, saber a porcentagem de países que possuem verde e 
# amarelo na bandeira fazendo a média por grupos:
GoldGrennFlags %>% ddply(., ~landmass, summarise, rate = mean(goldGrenn))
View(GoldGrennFlags)
# less verbose mode:
flags %>% mutate(., goldGrenn = pairsVec(gold, green)) %>% ddply(., ~landmass, summarise, rate = mean(goldGrenn))

# agora, vamos aprender a particionar/juntar os dados

# particionar:

# selecionar as variáveis entre "name" e "language" (inclusive) e as 10 primeiras observações
f1 = flags %>% select(., name:language) %>% slice(., 1:10)
f2 = flags %>% select(., -(name:language)) %>% slice(., 1:10)

# agora vamos juntar os dois:
setequal(bind_cols(f1,f2), flags %>% slice(.,1:10))



