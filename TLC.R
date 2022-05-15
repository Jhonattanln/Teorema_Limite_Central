### Importando bibliotecas
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)



### Criando dodos para anális
n <- 5000 # número da amostra
m <- 5000 # número da amostra que serão tiradas as médias

sim <- tibble(indice = 1:n,
              exponencial = double(length = n),
              uniforme = double(length = n),
              tStudent = double(length = n),
              fFisher = double(length = n))

set.seed(1234)

for(i in 1:n) {
  sim$exponencial[i] <- rexp(n = m) %>% mean()
  sim$uniforme[i] <- runif(n = m) %>% mean()
  sim$tStudent[i] <- rt(n = m, df = 2) %>% mean()
  sim$fFisher[i] <- rf(n = m, df1 = 2, df2 = 4) %>% mean()
}

### Plotando a distribuição dos dados
sim %>%
  ggplot(aes(x = exponencial))+ # distribuição exponencial
  geom_histogram(aes(y = ..density..), bins = 50, fill = 'black', alpha = 0.8)+
  geom_density(size = 1.5, alpha = 0.9, color = 'red')+
  theme_hc()+
  scale_colour_hc()

sim %>%
  ggplot(aes(x = uniforme))+ # distribuição uniforme
  geom_histogram(aes(y = ..density..),bins = 50, fill = 'black', alpha = 0.8)+
  geom_density(size = 1.5, alpha = 0.9, color = 'red')+
  theme_hc()+
  scale_colour_hc()

### Aplicando testes para amostra

#### Primeiramente é necessário normalizar os dados

teste <- tibble(indice = 3:n,
                Pexpo = double(length= n-2),
                Puni = double(length = n-2),
                PtStu = double(length = n-2),
                PfFis = double(length = n-2))
