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


### Aplicando testes para amostra

#### Primeiramente é necessário normalizar os dados

testes <- tibble(indice = 3:n,
                Puni = double(length = n-2),
                PtStu = double(length = n-2),
                PfFis = double(length = n-2))

for(i in 3:nrow(testes)) {
  
  janela <- sim %>% 
    filter(indice <= i) %>% # selecionamos apenas os �ndices at� o atual
    transmute(uniforme = (uniforme - mean(uniforme)/sd(uniforme)),
              tStudent = (tStudent - mean(tStudent))/sd(tStudent),
              fFisher = (fFisher - mean(fFisher))/sd(fFisher))
  ### Rodando teste de Kolmogorov-Smirnov
  
  testes$Puni[i] <- ks.test(x = janela$uniforme, 'pnorm')$p.value
  testes$PtStu[i] <- ks.test(x = janela$tStudent, 'pnorm')$p.value
  testes$PfFis[i] <- ks.test(x = janela$fFisher, 'pnorm')$p.value
  
}

### Plotando distribui��o 

janela %>%
  ggplot(aes(x = fFisher))+ # distribuição Fisher
  geom_histogram(aes(y = ..density..),bins = 70, fill = 'black', alpha = 0.8)+
  geom_density(size = 1.5, alpha = 0.9, color = 'red')+
  theme_hc()+
  scale_colour_hc()

ecdf_fisher <- ecdf(janela$fFisher)

plot(ecdf_fisher)

### Gr�fico do p_value

testes %>%
  pivot_longer(Puni:PfFis,
               names_to = "distro",
               values_to = "p") %>%
  ggplot(aes(x = indice, color = distro, y = p))+
  geom_line(size=1.2, alpha = 0.8)+
  theme_hc()+
  scale_colour_hc()+
  scale_y_continuous(label = scales::percent)

