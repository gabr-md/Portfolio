
# Financiamento de livros - Laredo

# Pacotes

library(dplyr)
library(ggplot2)
library(pROC)
library(readr)
library(gridExtra)
library(grid)


set.seed(12345)

# Função

freq.tab <- function(data,coluna){
  
  #coluna <- all_of(coluna)
  
  tabela <- data %>%
    rename("variavel" = coluna) %>%
    group_by(variavel,STATUS) %>%
    filter(STATUS == "BOM") %>%
    summarise(n_bom = n()) %>%
    ungroup() %>%
    select(variavel,n_bom) %>%
    left_join(data %>%
                rename("variavel" = coluna) %>%
                group_by(variavel,STATUS) %>%
                filter(STATUS == "MAU") %>%
                summarise(n_mau = n()) %>%
                ungroup() %>%
                select(variavel,n_mau), by = c("variavel"))
  
  tabela <- tabela %>%
    mutate(total = n_bom + n_mau,
           perc.bom = paste0(round(n_bom/total,4)*100,"%"),
           perc.mau = paste0(round(n_mau/total,4)*100,"%"),
           perc.total = "100%") %>%
    select(variavel,perc.bom,perc.mau,perc.total)
  
  colnames(tabela)[1] <- c(coluna)
  
  return(tabela)
  
}

#---------------

# Lendo dataset

setwd("C:\\Users\\gabri\\Documents\\Projetos\\Crédito\\Laredo\\")

data <- readxl::read_xls("351.xls")

data <- data %>%
select(IDADE,UNIFED,FONE,INSTRU,CARTAO,RESTR,RESID,FICÇÃO,NÃOFICÇAO,AUTOAJUDA,CATEG,STATUS)

#---------------

# Tipo das colunas da base de dados

glimpse(data)

#---------------

# Conferindo dados faltantes

# NA

table(is.na(data))

# Vazio

table(data == "")

#---------------

# Status

prop.table(table(data$STATUS))

# Analisando variáveis numéricas

# Descrição com dados simples

data %>%
  filter(STATUS == "BOM") %>%
  summarise(summary(IDADE)) %>%
  mutate(metrica = c("min","1 quartil","mediana","media","3 quartil","max")) %>%
  select(metrica,everything()) %>%
  rename("Bom" = "summary(IDADE)") %>%
  left_join(data %>%
              filter(STATUS == "MAU") %>%
              summarise(summary(IDADE)) %>%
              mutate(metrica = c("min","1 quartil","mediana","media","3 quartil","max")) %>%
              select(metrica,everything()), by = "metrica") %>%
  rename("Mau" = "summary(IDADE)")

data %>%
  ggplot(aes(x = IDADE, fill = STATUS)) +
  geom_histogram()

#---------------

# UNIFED

tabela_unifed <- freq.tab(data,"UNIFED")

tabela_unifed

# Fone

tabela_fone <- freq.tab(data,"FONE")

tabela_fone

# Instru

tabela_instru <- freq.tab(data,"INSTRU")

tabela_instru

# Cartão

tabela_cartao <- freq.tab(data,"CARTAO")

tabela_cartao

# Restr

tabela_restr <- freq.tab(data,"RESTR")

tabela_restr

# RESID

tabela_resid <- freq.tab(data,"RESID")

tabela_resid

# FICÇÃO

tabela_ficcao <- freq.tab(data,"FICÇÃO")

tabela_ficcao

# NÃOFICÇAO

tabela_nficcao <- freq.tab(data,"NÃOFICÇAO")

tabela_nficcao

# AUTOAJUDA

tabela_autoajuda <- freq.tab(data,"AUTOAJUDA")

tabela_autoajuda

# CATEG

tabela_categ <- freq.tab(data,"CATEG")

tabela_categ

#-------------

# Pensar em não fazer restritivo
# Variáveis de não ficção e telefone parecem não influenciar
# Variável autoajuda também influencia bem

# Modelo com restritivos

library(caret)
set.seed(12345)

indice = createDataPartition(y=data$STATUS, p=0.7, list=FALSE)
treino = data[indice, ]
teste = data[-indice, ]

prop.table(table(treino$STATUS))

prop.table(table(teste$STATUS))

treino <- treino %>% 
  mutate(STATUS = ifelse(STATUS == 'BOM',"0","1"),
         STATUS = as.numeric(STATUS))

# Fazendo os modelos

fit1 <- glm(STATUS ~.,data = treino, family = "binomial")

summary(fit1)

# Modelo 2 sem as variáveis que nãp influenciam

fit2_restr <- glm(STATUS ~.-FONE-RESID-NÃOFICÇAO,data = treino, family = "binomial")

summary(fit2_restr)

predito_fit2_restr <- predict(fit2_restr, newdata = teste,type = "response")

predito_fit2_restr <- ifelse(predito_fit2_restr >= 0.5,"MAU","BOM")

teste <- teste %>%
  mutate(valor = ifelse(STATUS == 1,"MAU","BOM"))

confusionMatrix(table(teste$STATUS,predito_fit2_restr)) # Acurácia 0,8433

predito_auc <- predict(fit2_restr, newdata = teste,type = "response")

auc(teste$STATUS,predito_auc)

roc(teste$STATUS,predito_auc, plot=T)

#------------------------------------------------------------

# Re Scorando o modelo

predito_fit2 <- predict(fit2_restr, newdata = data)

x <- -round(1000*((predito_fit2-6)/12))

x[x <= 0] <- 0
x[x >= 1000] <- 1000

data_aux <- data

data_aux$score <- x

data_aux <- data_aux %>% arrange(score)

limiares <- c()

for(i in c(seq(150,3000,150))){
  
  limiares <- c(limiares,data_aux$score[i])
  
}

data_aux$range <- cut(data_aux$score, breaks = c(0,limiares),
                  labels = limiares)

tabela_ks <- data_aux %>%
  filter(STATUS == "MAU") %>%
  group_by(range) %>%
  summarise(n = n()/600) %>%
  rename("Mau" = "n") %>%
  full_join(data_aux %>%
              filter(STATUS == "BOM") %>%
              group_by(range) %>%
              summarise(n = n()/2400), by = "range") %>%
  rename("Bom" = "n") %>%
  arrange(range) %>%
  mutate(Mau = ifelse(is.na(Mau) == T,0,Mau)) %>%
  left_join(data_aux %>%
              group_by(range) %>%
              summarise(n = n()/3000), by = "range") %>%
  mutate(KS = round((cumsum(Mau)-cumsum(Bom))*100,0),
         KS = paste0(KS,"%"))  

# KS de 55%, de acordo com a tabela do laredo esse é um resultado excelente

# Por sorte, logo na primeira tentativa o modelo apresentou bons resultados
# de acurácia, AUC e KS, podendo assim seguirmos para a criação de uma simples
# política

politica_score <- function(ponto_corte){
  
  aprovados <- c()
  
  inadimplencia_final <- c()
  
  producao_final <- c()
  
  for(k in 1:100){
    
    sorteio <- sample(c(1:3000),300,replace = T)
    
    amostra <- data_aux[sorteio,]
    
    valor <- round(rnorm(300,550,200),0)
    
    valor[valor <= 100] <- 100
    valor[valor >= 1000] <- 1000
    
    amostra$valor <- valor
    
    parcelas <- sample(c(2:6),300,replace = T)
    
    amostra$financiado <- NA
    amostra$resultado <- NA
    amostra$taxa <- NA
    amostra$parcelas <- parcelas
    
    # Resultado por score
    
    for(i in 1:300){
      
      if(amostra$score[i] <= ponto_corte){
        
        amostra$resultado[i] <- "Reprovado"
        
      }else{
        
        amostra$resultado[i] <- "Aprovado"
        
      }
      
    }
    
    # Resultado por restritivo e autoajuda
    
    for(i in 1:300){
      
      if(amostra$resultado[i] != "Reprovado" & amostra$AUTOAJUDA[i] == "SIM"){
        
        amostra$resultado[i] <- "Aprovado - Autoajuda - Limite 500"
        
      }
      if(amostra$resultado[i] != "Reprovado" & amostra$RESTR[i] == "SIM"){
        
        amostra$resultado[i] <- "Aprovado - Restritivo - Limite 400"
        
      }
      if(amostra$resultado[i] != "Reprovado" & amostra$AUTOAJUDA[i] == "SIM" & 
         amostra$RESTR[i] == "SIM"){
        
        amostra$resultado[i] <- "Aprovado - Restr. e AutoAjuda - Limite 300"
        
      }
      
    }
    
    # Ajustando o limite
    
    for(i in 1:300){
      
      novo_limite <- as.numeric(stringr::str_extract(amostra$resultado[i], "\\d+\\.*\\d*"))
      
      if(is.na(novo_limite) == F){
        
        if(amostra$valor[i] > novo_limite){
          
          amostra$valor[i] <- novo_limite
          
        }
        
      }
      
    }
    
    for(i in 1:300){
      
      if(amostra$resultado[i] == "Aprovado"){
        
        amostra$taxa[i] <- 1.025
        
      }
      if(amostra$resultado[i] == "Aprovado - Autoajuda - Limite 500"){
        
        amostra$taxa[i] <- 1.05
        
      }
      if(amostra$resultado[i] == "Aprovado - Restr. e AutoAjuda - Limite 300" |
         amostra$resultado[i] == "Aprovado - Restritivo - Limite 400"){
        
        amostra$taxa[i] <- 1.075
        
      }
      
    }
    
    amostra$financiado <- ((amostra$valor/amostra$parcelas) + (amostra$taxa-1)*amostra$valor)*amostra$parcelas
    
    # Produção
    
    producao <- amostra %>%
      filter(resultado != "Reprovado") %>%
      summarise(p = sum(valor))
    
    prod <- producao$p
    
    producao_final <- c(prod,producao_final)
    
    # Inadimplência
    
    mau_aprovado <- amostra %>%
      filter(STATUS == "MAU" & resultado != "Reprovado") %>%
      count()
    
    inadimplencia <- round(mau_aprovado$n/300,2)
    
    inadimplencia_final <- c(inadimplencia_final,inadimplencia)
    
    # Taxa aprovação
    
    aprovado <- amostra %>%
      filter(resultado != "Reprovado") %>%
      count()
    
    aprovado <- round(aprovado$n/300,2)
    
    aprovados <- c(aprovados,aprovado)
    
    
  }
  
  retorno <- list("Produção" = producao_final,"Inadimplência" = inadimplencia_final,
                  "Aprovação" = aprovados)
  
  
  return(retorno)
  
  
}

corte_602 <- politica_score(602)

corte_516 <- politica_score(516)

corte_644 <- politica_score(644)

corte_734 <- politica_score(734)


# 516

mean(corte_516$Produção)

mean(corte_516$Inadimplência)

mean(corte_516$Aprovação)

# 602

mean(corte_602$Produção)

mean(corte_602$Inadimplência)

mean(corte_602$Aprovação)

# 644

mean(corte_644$Produção)

mean(corte_644$Inadimplência)

mean(corte_644$Aprovação)

# 734

mean(corte_734$Produção)

mean(corte_734$Inadimplência)

mean(corte_734$Aprovação)










