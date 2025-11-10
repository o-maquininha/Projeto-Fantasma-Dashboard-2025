library(readxl)
library(tidyverse)
library(knitr)
library(kableExtra)
source("rdocs/source/packages.R")

faixa_peso <- function(imc){
  if(imc < 18.5){
    return("Abaixo do peso")
  } else if(imc >= 18.5 & imc < 24.9){
    return("Peso normal")
  } else if(imc >= 25 & imc < 29.9){
    return("Sobrepeso")
  } else {
    return("Obesidade")
  }
}

relatorio_vendas <- read_excel(path = "relatorio_old_town_road.xlsx", sheet = "relatorio_vendas")
info_vendas <- read_excel(path = "relatorio_old_town_road.xlsx", sheet = "infos_vendas")
info_produtos <- read_excel(path = "relatorio_old_town_road.xlsx", sheet = "infos_produtos")
info_funcionarios <- read_excel(path = "relatorio_old_town_road.xlsx", sheet = "infos_funcionarios")
info_cidades <- read_excel(path = "relatorio_old_town_road.xlsx", sheet = "infos_cidades")
info_clientes <- read_excel(path = "relatorio_old_town_road.xlsx", sheet = "infos_clientes")
info_lojas <- read_excel(path = "relatorio_old_town_road.xlsx", sheet = "infos_lojas")

relatorio_vendas$Date <- ymd(relatorio_vendas$Date)

relatorio_vendas <- relatorio_vendas %>% 
  inner_join(info_vendas, by = c("SaleID" = "Sal3ID")) %>% 
  inner_join(info_lojas, by = c("StoreID" = "Stor3ID")) %>% 
  inner_join(info_cidades, by = c("CityID" = "C1tyID")) %>% 
  inner_join(info_produtos, by = c("ItemID" = "Ite3ID")) %>% 
  inner_join(info_clientes, by = c("ClientID" = "Cli3ntID"))

relatorio_vendas <- relatorio_vendas %>% 
  mutate(ano = year(Date)) %>%
  mutate(UnityPrice = UnityPrice * 5.31) %>% 
  mutate(receitaVenda = Quantity * UnityPrice) %>% 
  mutate(Height_dm = Height_dm * 10) %>% 
  mutate(Weight_lbs = Weight_lbs * 0.453592) %>% 
  rename(altura_cm = Height_dm,
         peso_kg = Weight_lbs) %>% 
  mutate(IMC = peso_kg/(altura_cm/100)^2) %>% 
  mutate(FaixaPeso = sapply(IMC, faixa_peso))

# glimpse(relatorio_vendas)
# any(is.na(relatorio_vendas))

# ---------------Analise 1-------------------------

tabela1 <-  relatorio_vendas %>% 
  group_by(ano, NameStore, NameCity, NameProduct) %>%
  summarise(
    receitaAnual = sum(receitaVenda)
  ) #%>% View

tabela2 <- tabela1 %>% 
  group_by(NameStore) %>% 
  summarise(
    receitaMedia = mean(receitaAnual),
    Mediana = median(receitaAnual),
    DP = sd(receitaAnual),
  ) %>% 
  arrange(desc(receitaMedia)) #%>% View

relatorio_vendas %>%
  group_by(ano, NameStore) %>% 
  summarise(receita = sum(receitaVenda)) %>% 
  ggplot(aes(ano, receita)) +
  scale_x_continuous(breaks = seq(1880, 1889, by = 2)) +
  geom_smooth(method = lm, se = FALSE, colour = "lightgrey") +
  geom_point(size = 1.5, colour = "#A11D21") +
  geom_line(colour = "#A11D21") +
  facet_wrap(~NameStore, nrow = 3) +
  labs(x = "Ano", y = "Receita") +
  theme_estat()

tabela3 <- relatorio_vendas %>%
  group_by(ano) %>% 
  summarise(
    receitaAnual = sum(receitaVenda)
  ) #%>% View

relatorio_vendas %>% 
  group_by(ano) %>% 
  summarise(receita = sum(receitaVenda)) %>%
  ggplot(aes(ano, receita)) +
  scale_x_continuous(breaks = c(1880:1889)) +
  geom_smooth(method = lm, se = FALSE, colour = "lightgrey") +
  geom_point(size = 1.5, colour = "#A11D21") +
  geom_line(colour = "#A11D21") +
  labs(x = "Ano", y = "Receita Total") +
  theme_estat()
#----------------Analise 1 extra---------------------------

tabela4 <- relatorio_vendas %>% 
  group_by(NameProduct) %>%
  summarise(
    quantidade = sum(Quantity)
    ) %>% 
  arrange(desc(quantidade)) #%>% View

tabela5 <- tabela1 %>% 
  group_by(NameProduct) %>%
  summarise(
    receitaMedia = mean(receitaAnual),
    Mediana = median(receitaAnual),
    DP = sd(receitaAnual),
  ) #%>% View

relatorio_vendas %>%
  group_by(ano, NameProduct) %>% 
  summarise(receita = sum(receitaVenda)) %>%
  ggplot(aes(ano,receita)) +
  scale_x_continuous(breaks = seq(1880, 1889, by = 2)) +
  geom_smooth(method = lm, se = FALSE, colour = "lightgrey") +
  geom_point(size = 1.5, colour = "#A11D21") +
  geom_line(colour = "#A11D21") +
  facet_wrap(~NameProduct, nrow = 2) +
  labs(title = "Receita anual por produto", x = "Ano", y = "Receita") +
  theme_estat()

tabela6 <- relatorio_vendas %>% 
  group_by(NameCity) %>%
  summarise(
    quantidade = sum(Quantity)
    ) %>% 
  arrange(desc(quantidade)) #%>% View

tabela7 <- tabela1 %>% 
  group_by(NameCity) %>%
  summarise(
    receitaMedia = mean(receitaAnual),
    Mediana = median(receitaAnual),
    DP = sd(receitaAnual),
  ) #%>% View

tabela8 <- tabela1 %>% 
  filter(ano %in% c(1880, 1889)) %>%
  group_by(NameCity) %>%
  summarise(
    "Taxa de cerscimento (%)" = ((sum(receitaAnual[ano == 1889]) -sum(receitaAnual[ano == 1880]))/sum(receitaAnual[ano == 1880])) * 100
  ) %>% arrange(desc(`Taxa de cerscimento (%)`)) #%>% View
  
relatorio_vendas %>%
  group_by(ano, NameCity) %>% 
  summarise(receita = sum(receitaVenda)) %>%
  ungroup() %>% 
  select(ano, NameCity, receita) %>%
  ggplot(aes(ano,receita, colour = NameCity)) +
  scale_x_continuous(breaks = seq(1880, 1889, by = 2)) +
  #geom_smooth(method = lm, se = FALSE, colour = "lightgrey") +
  geom_point(size = 3, alpha = .5) +
  geom_line(size = 1) +
  #facet_wrap(~NameCity, nrow = 2) +
  labs(title = "Receita anual por cidade", x = "Ano", y = "Receita", colour = "Cidade") +
  theme_estat() +
  theme(legend.position = "right")

# ---------------Analise 2----------------------
aux <- relatorio_vendas %>% 
  select(ClientID, peso_kg, altura_cm, FaixaPeso) %>%
  distinct()

p_media <- mean(aux$peso_kg)
p_sd <- sd(aux$peso_kg)
p_var <- var(aux$peso_kg)
h_media <- mean(aux$altura_cm)
h_sd <- sd(aux$altura_cm)
h_var <- var(aux$altura_cm)

np_test <- lillie.test(aux$peso_kg)
nh_test <- lillie.test(aux$altura_cm)

p_est <- np_test$statistic
p_pv <- np_test$p.value
h_est <- nh_test$statistic
h_pv <- nh_test$p.value

tabela9 <- data.frame(
  `p-valor` = c(p_pv, h_pv),
  `Estatística D` = c(p_est, h_est)) %>%
  rownames_to_column("Variável") %>%
  mutate(Variável = c("Peso", "Altura"))

aux %>% 
  ggplot(aes(x = altura_cm)) +
  geom_histogram(aes( y = ..density.., fill = "Distribuição empírica"), binwids = 30, colour = "white") +
  stat_function(
    fun = dnorm,
    args = list(mean = h_media,
                sd = h_sd),
    aes(colour = "Distribuição teórica"),
    size = 1.2, linetype = "dashed") +
  labs(x = "Altura (cm)", y = "Densidade", fill = NULL, colour = NULL) +
  theme_estat() +
  scale_colour_manual(values = c("Distribuição teórica" = "#003366"))



aux %>%
  ggplot(aes(x = peso_kg)) +
  geom_histogram(aes( y = ..density.., fill = "Distribuição empírica"), bins = 30, colour = "white") +
  stat_function(
    fun = dnorm,
    args = list(mean = p_media,
                sd = p_sd),
    aes(colour = "Distribuição teórica"),
    size = 1.2, linetype = "dashed") +
  labs(x = "Peso (kg)", y = "Densidade", fill = NULL, colour = NULL) +
  theme_estat() +
  scale_colour_manual(values = c("Distribuição teórica" = "#003366"))

relatorio_vendas %>% 
  select(ClientID, altura_cm) %>%
  distinct()  %>%
  ggplot(aes(x = altura_cm)) +
  geom_histogram(aes( y = ..density..), bins = 30, fill = "skyblue", colour = "white") +
  labs(x = " X", y = "Densidade") +
  stat_function(fun = dnorm,
                args = list(mean = mean(relatorio_vendas$altura_cm),
                            sd = sd(relatorio_vendas$altura_cm)),
                colour = "red", size = 1, linetype = "dashed") +
  theme_minimal()

cor_test <- cor.test(aux$peso_kg, aux$altura_cm, method = "pearson", conf.level = 0.95)
cor_hat <- cor_test$estimate
cor_IC <- cor_test$conf.int
cor_pv <- cor_test$p.value
cor_est <- cor_test$statistic

tabela10 <- data.frame(
  "$\\hat{\\rho}$" = cor_hat,
  "p-valor" = cor_pv,
  "Estatística t" = cor_est,
  "Intervalo de Confiança" = paste0("[", cor_IC[1], "; ", cor_IC[2], "]")) %>% 
  t()
  
relatorio_vendas %>%
  ggplot(aes(x = peso_kg)) +
  geom_density(aes(fill = "Distribuição amostral"), alpha = .5, colour = NA) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(relatorio_vendas$peso_kg),
                sd = sd(relatorio_vendas$peso_kg)),
    aes(colour = "Distribuição teórica"),
    size = 1.2, linetype = "dashed"
  ) +
  labs(
    x = "Peso (kg)",
    y = "Densidade",
    fill = NULL,
    colour = NULL
  ) +
  theme_estat() +
  scale_colour_manual(values = c("Distribuição teórica" = "#003366"))

#---------------Analise 3----------------------

ambar_seco <- relatorio_vendas %>% 
  filter(NameCity == "Âmbar Seco")


ambar_seco %>% 
  group_by(NameStore, Age) %>%
  ggplot(aes(NameStore, Age)) +
  geom_boxplot(fill = "#A11D21") +
  coord_flip() +
  labs(x = "Loja", y = "Idade") +
  theme_estat()

ambar_seco %>% 
  group_by(NameStore, Age) %>%
  ggplot(aes(NameStore, Age, colour = Sex)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Loja", y = "Idade") +
  theme_estat()

tabela11 <- ambar_seco %>% 
  group_by(NameStore) %>%
  summarise(
    Média = mean(Age),
    Mediana = median(Age),
    DP = sd(Age),
    DIQ = IQR(Age)) %>% 
  arrange(desc(Média))

#-------------------Analise 4----------------------

tabela12 <- relatorio_vendas %>%
  filter(ano == 1889) %>%
  group_by(NameStore) %>%
  summarize(receitaLoja = sum(receitaVenda)) %>%
  arrange(desc(receitaLoja))

top3_lojas <- tabela12 %>%
  slice(1:3) %>%
  pull(NameStore)

aux2 <- relatorio_vendas %>% 
  select(SaleID, NameStore, NameProduct, Quantity, ano, receitaVenda) %>%
  group_by(NameStore) %>% 
  mutate(receitaLoja = sum(receitaVenda)) %>% 
  filter(ano == 1889, NameStore %in% top3_lojas)

tabela13 <- aux2 %>% 
  group_by(NameProduct) %>%
  summarize(quantidadeVendida = sum(Quantity)) %>%
  arrange(desc(quantidadeVendida))

top3_produtos <- tabela13 %>%
  slice(1:3) %>%
  pull(NameProduct)

tabela14 <- aux2 %>%
  group_by(NameStore, NameProduct) %>%
  summarise(
    quantidadeVendida = sum(Quantity),
    receitaGerada = sum(receitaVenda)
  ) %>%
  arrange(NameStore, desc(quantidadeVendida)) %>% 
  
