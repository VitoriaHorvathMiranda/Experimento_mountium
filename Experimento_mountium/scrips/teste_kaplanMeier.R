library(survival)
library(survminer)
library(lubridate)
library(tidyverse)

#eh importante que não tenha mortes negativas (contagem errada)
data <- read_delim("../data/Moutium_sem_morte-.csv", delim = ",")

tidy_data <- data %>%
  pivot_longer(cols = c("R1", "R2", "R3", "R4", "R5"), #deixa o dado tidy
               names_to = "replica", values_to = "sobreviventes") %>% 
  mutate(data = as_date(dmy(data))) %>% #le a coluna de data como data
  mutate(DPI = if_else(especie == "bocquete" | especie == "melanogaster",
                       yday(data) - 291, #um dia antes do dia do ano que o experimento comecou
                       yday(data) - 292)) %>% 
  mutate(tratamento = case_when(tratamento == "V" ~ "FHV",
                                tratamento == "C" ~ "Ringer")) %>%
  mutate(wolbachia = case_when(wolbachia == "P" ~ "W+",
                               wolbachia == "N" ~"W-")) %>%
  mutate(especie = if_else(especie == "bocquete", "bocqueti", especie)) %>%
  mutate(catg = str_c(especie, wolbachia, tratamento, replica)) 

#junta as replicas (soma e trata como uma coisa so)
summary <- tidy_data %>% 
  group_by(wolbachia, tratamento, especie, DPI) %>%
  summarise(sobreviventes = sum(sobreviventes)) %>% 
  mutate(catg = str_c(especie, wolbachia, tratamento))


# função portos_por_dia ---------------------------------------------------
#função que calcula os mortos por dia com base na sobrevivencia
mortos_por_dia <- function(grupo){
  tbl_for <- summary %>% filter(catg == grupo)
  n_dias <- nrow(tbl_for)
  mortos <- vector(mode = "numeric", length = n_dias)
  mortos[1] <- 0
  
  for (i in 2:nrow(tbl_for)) {
    mortos[i] <- tbl_for$sobreviventes[i-1] - tbl_for$sobreviventes[i]
  }
  mortos
  tbl_for %>%
    mutate(mortos_por_dia = mortos)
  
}


# por especie -------------------------------------------------------------
#cria os df para cada especie com os mortos por dia
#depois cria uma tabela uma mosca por linha 
#n de dias contados de baimai e tri = 25; de bocq e melano = 26
#assume que nenhuma mosca morreu no ultimo dia
#é como se o ultimo dia não tivesse sido contado

#baimai
list_baimai <- list(mortos_por_dia("baimaiW-FHV"),
                    mortos_por_dia("baimaiW-Ringer"),
                    mortos_por_dia("baimaiW+FHV"),
                    mortos_por_dia("baimaiW+Ringer"))

df_baimai <- list_baimai %>% reduce(full_join, by = c("catg", "wolbachia", "tratamento", "especie", "sobreviventes", "DPI", "mortos_por_dia"))

single_fly_baimai <- df_baimai %>%
  ungroup() %>%
  mutate(teste = if_else(DPI == 25 & sobreviventes > 0, #n de moscas sobrevivente ate o ultimo dia
          sobreviventes,
          mortos_por_dia)) %>%
  uncount(teste) %>%
  select(wolbachia, tratamento, especie, DPI) %>%
  mutate(evento = if_else(DPI < 25, 1, 0)) %>%
  mutate(grupo = str_c(wolbachia, tratamento, sep = " "))


#Tri
list_tri <- list(mortos_por_dia("triaurariaW-FHV"),
                    mortos_por_dia("triaurariaW-Ringer"),
                    mortos_por_dia("triaurariaW+FHV"),
                    mortos_por_dia("triaurariaW+Ringer"))

df_tri <- list_tri %>% reduce(full_join, by = c("catg", "wolbachia", "tratamento", "especie", "sobreviventes", "DPI", "mortos_por_dia"))

single_fly_tri <- df_tri %>%
  ungroup() %>%
  mutate(teste = if_else(DPI == 25 & sobreviventes > 0,
                         sobreviventes,
                         mortos_por_dia)) %>%
  uncount(teste) %>%
  select(wolbachia, tratamento, especie, DPI) %>%
  mutate(evento = if_else(DPI < 25, 1, 0)) %>%
  mutate(grupo = str_c(wolbachia, tratamento, sep = " "))


#bocq
list_bocq <- list(mortos_por_dia("bocquetiW-FHV"),
                 mortos_por_dia("bocquetiW-Ringer"),
                 mortos_por_dia("bocquetiW+FHV"),
                 mortos_por_dia("bocquetiW+Ringer"))

df_bocq <- list_bocq %>% reduce(full_join, by = c("catg", "wolbachia", "tratamento", "especie", "sobreviventes", "DPI", "mortos_por_dia"))

single_fly_bocq <- df_bocq %>%
  ungroup() %>%
  mutate(teste = if_else(DPI == 26 & sobreviventes > 0,
                         sobreviventes,
                         mortos_por_dia)) %>%
  uncount(teste) %>%
  select(wolbachia, tratamento, especie, DPI) %>%
  mutate(evento = if_else(DPI < 26, 1, 0)) %>%
  mutate(grupo = str_c(wolbachia, tratamento, sep = " ")) 


#melano
list_melano <- list(mortos_por_dia("melanogasterW-FHV"),
                 mortos_por_dia("melanogasterW-Ringer"),
                 mortos_por_dia("melanogasterW+FHV"),
                 mortos_por_dia("melanogasterW+Ringer"))

df_melano <- list_melano %>% reduce(full_join, by = c("catg", "wolbachia", "tratamento", "especie", "sobreviventes", "DPI", "mortos_por_dia"))

single_fly_melano <- df_melano %>%
  ungroup() %>%
  mutate(teste = if_else(DPI == 26 & sobreviventes > 0, 
                         sobreviventes,
                         mortos_por_dia)) %>%
  uncount(teste) %>%
  select(wolbachia, tratamento, especie, DPI) %>%
  mutate(evento = if_else(DPI < 26, 1, 0)) %>%
  mutate(grupo = str_c(wolbachia, tratamento, sep = " ")) 


# kaplan meier ------------------------------------------------------------
#cria os graficos e calcula as estatisticas para as curvas Kaplan Meier

#baimai
surv_baimai <- Surv(time = single_fly_baimai$DPI, event = single_fly_baimai$evento)
fit_baimai <- survfit(surv_baimai ~ grupo, data = single_fly_baimai)

plot_baimai <- ggsurvplot(fit_baimai,
           data = single_fly_baimai,
           #pval = TRUE,
           conf.int = TRUE,
           legend.labs=c("W- FHV", "W- Ringer", "W+ FHV", "W+ Ringer"),
           legend.title = "") +
  labs(title = "D. baimai", x = "DPI")


#tri
surv_tri <- Surv(time = single_fly_tri$DPI, event = single_fly_tri$evento)
fit_tri <- survfit(surv_tri ~ grupo, data = single_fly_tri)

plot_tri <- ggsurvplot(fit_tri,
           data = single_fly_tri,
           #pval = TRUE,
           conf.int = TRUE,
           legend.labs=c("W- FHV", "W- Ringer", "W+ FHV", "W+ Ringer"),
           legend.title = "") +
  labs(title = "D. triauraria", x = "DPI")


#bocq
surv_bocq <- Surv(time = single_fly_bocq$DPI, event = single_fly_bocq$evento)
fit_bocq <- survfit(surv_bocq ~ grupo, data = single_fly_bocq)

plot_bocq <- ggsurvplot(fit_bocq,
           data = single_fly_bocq,
           #pval = TRUE,
           conf.int = TRUE,
           legend.labs=c("W- FHV", "W- Ringer", "W+ FHV", "W+ Ringer"),
           legend.title = "") +
  labs(title = "D. bocqueti", x = "DPI")


#melano
surv_melano <- Surv(time = single_fly_melano$DPI, event = single_fly_melano$evento)
fit_melano <- survfit(surv_melano ~ grupo, data = single_fly_melano)

plot_melano <- ggsurvplot(fit_melano,
           data = single_fly_melano,
           #pval = TRUE,
           conf.int = TRUE,
           legend.labs=c("W- FHV", "W- Ringer", "W+ FHV", "W+ Ringer"),
           legend.title = "") +
  labs(title = "D. melanogaster", x = "DPI")


#não consegui juntar os plots pq o formado do ggsurplot eh estranho
#mesma coisa na hora de salvar




