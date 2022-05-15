### Importando bibliotecas
library(tibble)
library(dplyr)
library(tidyr)

### Criando dodos para anális
n <- 5000 # número da amostra
m <- 500 # número da amostra que serão tiradas as médias

sim <- tibble(indice = 1:n,
              exponencial = double(length = n),
              uniforme = double(length = n),
              tStudent = double(length = n),
              fFisher = double(length = n))

set.seed(42)

for(i in 1:n) {
  sim$exponencial[i] <- rexp(n = m) %>% mean()
  sim$uniforme[i] <- runif(n = m) %>% mean()
  sim$tStudent[i] <- rt(n = m, df = 2) %>% mean()
  sim$fFisher[i] <- rf(n = m, df1 = 2, df2 = 4) %>% mean()
}
