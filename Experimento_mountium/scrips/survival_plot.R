library(tidyverse)
library(lubridate)
#sei esse pacote só para calcular o erro padrao mas nao sei se eh bom
library(plotrix) 

# o delim é ";" porque eu transformei pelo excel
data <- read_delim("../data/experimento_mountium.csv", delim = ";")

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

summary <- tidy_data %>%
  group_by(especie, wolbachia, tratamento, DPI) %>%
  summarise(media = mean(sobreviventes),
            erro_padrao = std.error(sobreviventes)) %>% 
  mutate(catg = str_c(especie, wolbachia, tratamento)) %>%
  ungroup() 

#calcula a media e o ep por grupo
prop_data <- summary %>%
  mutate(first_media = case_when( #pega a media do primeiro dia e repete para os demais dia numa coluna nova
    catg == "bocquetiW+FHV" ~ subset(summary, catg == "bocquetiW+FHV" & DPI == 1)[["media"]],
    catg == "bocquetiW+Ringer" ~ subset(summary, catg == "bocquetiW+Ringer" & DPI == 1)[["media"]],
    catg == "bocquetiW-FHV" ~ subset(summary, catg == "bocquetiW-FHV" & DPI == 1)[["media"]],
    catg == "bocquetiW-Ringer" ~ subset(summary, catg == "bocquetiW-Ringer" & DPI == 1)[["media"]],
    catg == "melanogasterW+FHV" ~ subset(summary, catg == "melanogasterW+FHV" & DPI == 1)[["media"]],
    catg == "melanogasterW+Ringer" ~ subset(summary, catg == "melanogasterW+Ringer" & DPI == 1)[["media"]],
    catg == "melanogasterW-FHV" ~ subset(summary, catg == "melanogasterW-FHV" & DPI == 1)[["media"]],
    catg == "melanogasterW-Ringer" ~ subset(summary, catg == "melanogasterW-Ringer" & DPI == 1)[["media"]],
    catg == "baimaiW+FHV" ~ subset(summary, catg == "baimaiW+FHV" & DPI == 1)[["media"]],
    catg == "baimaiW+Ringer" ~ subset(summary, catg == "baimaiW+Ringer" & DPI == 1)[["media"]],
    catg == "baimaiW-FHV" ~ subset(summary, catg == "baimaiW-FHV" & DPI == 1)[["media"]],
    catg == "baimaiW-Ringer" ~ subset(summary, catg == "baimaiW-Ringer" & DPI == 1)[["media"]],
    catg == "triaurariaW+FHV" ~ subset(summary, catg == "triaurariaW+FHV" & DPI == 1)[["media"]],
    catg == "triaurariaW+Ringer" ~ subset(summary, catg == "triaurariaW+Ringer" & DPI == 1)[["media"]],
    catg == "triaurariaW-FHV" ~ subset(summary, catg == "triaurariaW-FHV" & DPI == 1)[["media"]],
    catg == "triaurariaW-Ringer" ~ subset(summary, catg == "triaurariaW-Ringer" & DPI == 1)[["media"]],
    TRUE ~ media )) %>% 
  mutate(prop_media = (media*100)/first_media,
         prop_erro = (erro_padrao*100)/first_media)
  
#faz o grafico com a media e erro padrao
prop_data %>%  
  select(especie, wolbachia, tratamento, prop_media, prop_erro, DPI) %>%
  mutate(catg2 = str_c(wolbachia, tratamento, sep = " ")) %>%
  ggplot(aes(x = DPI, y = prop_media, color = catg2)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = prop_media - prop_erro, ymax = prop_media + prop_erro)) +
  facet_wrap(~ especie, nrow = 2) +
    coord_cartesian(ylim = c(0, 100)) +
  labs(y = "Survival (%)", color = NULL)
  
ggsave(filename = "../plots/media_replicas.png")

#plot por replica -------------------------------------------------------

#calcula a proporção de sobreviventes por grupo e por réplica
rep_prop <- tidy_data %>%
    mutate(total = case_when(
    catg == "bocquetiW+FHVR1" ~ subset(tidy_data, catg=="bocquetiW+FHVR1"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW+FHVR2" ~ subset(tidy_data, catg=="bocquetiW+FHVR2"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW+FHVR3" ~ subset(tidy_data, catg=="bocquetiW+FHVR3"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW+FHVR4" ~ subset(tidy_data, catg=="bocquetiW+FHVR4"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW+FHVR5" ~ subset(tidy_data, catg=="bocquetiW+FHVR5"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW+RingerR1" ~ subset(tidy_data, catg=="bocquetiW+RingerR1"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW+RingerR2" ~ subset(tidy_data, catg=="bocquetiW+RingerR2"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW+RingerR3" ~ subset(tidy_data, catg=="bocquetiW+RingerR3"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW+RingerR4" ~ subset(tidy_data, catg=="bocquetiW+RingerR4"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW+RingerR5" ~ subset(tidy_data, catg=="bocquetiW+RingerR5"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW-FHVR1" ~ subset(tidy_data, catg=="bocquetiW-FHVR1"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW-FHVR2" ~ subset(tidy_data, catg=="bocquetiW-FHVR2"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW-FHVR3" ~ subset(tidy_data, catg=="bocquetiW-FHVR3"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW-FHVR4" ~ subset(tidy_data, catg=="bocquetiW-FHVR4"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW-FHVR5" ~ subset(tidy_data, catg=="bocquetiW-FHVR5"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW-RingerR1" ~ subset(tidy_data, catg=="bocquetiW-RingerR1"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW-RingerR2" ~ subset(tidy_data, catg=="bocquetiW-RingerR2"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW-RingerR3" ~ subset(tidy_data, catg=="bocquetiW-RingerR3"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW-RingerR4" ~ subset(tidy_data, catg=="bocquetiW-RingerR4"& DPI == 1)[["sobreviventes"]],
    catg == "bocquetiW-RingerR5" ~ subset(tidy_data, catg=="bocquetiW-RingerR5"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW+FHVR1" ~ subset(tidy_data, catg=="melanogasterW+FHVR1"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW+FHVR2" ~ subset(tidy_data, catg=="melanogasterW+FHVR2"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW+FHVR3" ~ subset(tidy_data, catg=="melanogasterW+FHVR3"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW+FHVR4" ~ subset(tidy_data, catg=="melanogasterW+FHVR4"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW+FHVR5" ~ subset(tidy_data, catg=="melanogasterW+FHVR5"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW+RingerR1" ~ subset(tidy_data, catg=="melanogasterW+RingerR1"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW+RingerR2" ~ subset(tidy_data, catg=="melanogasterW+RingerR2"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW+RingerR3" ~ subset(tidy_data, catg=="melanogasterW+RingerR3"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW+RingerR4" ~ subset(tidy_data, catg=="melanogasterW+RingerR4"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW+RingerR5" ~ subset(tidy_data, catg=="melanogasterW+RingerR5"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW-FHVR1" ~ subset(tidy_data, catg=="melanogasterW-FHVR1"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW-FHVR2" ~ subset(tidy_data, catg=="melanogasterW-FHVR2"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW-FHVR3" ~ subset(tidy_data, catg=="melanogasterW-FHVR3"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW-FHVR4" ~ subset(tidy_data, catg=="melanogasterW-FHVR4"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW-FHVR5" ~ subset(tidy_data, catg=="melanogasterW-FHVR5"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW-RingerR1" ~ subset(tidy_data, catg=="melanogasterW-RingerR1"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW-RingerR2" ~ subset(tidy_data, catg=="melanogasterW-RingerR2"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW-RingerR3" ~ subset(tidy_data, catg=="melanogasterW-RingerR3"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW-RingerR4" ~ subset(tidy_data, catg=="melanogasterW-RingerR4"& DPI == 1)[["sobreviventes"]],
    catg == "melanogasterW-RingerR5" ~ subset(tidy_data, catg=="melanogasterW-RingerR5"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW+FHVR1" ~ subset(tidy_data, catg=="baimaiW+FHVR1"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW+FHVR2" ~ subset(tidy_data, catg=="baimaiW+FHVR2"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW+FHVR3" ~ subset(tidy_data, catg=="baimaiW+FHVR3"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW+FHVR4" ~ subset(tidy_data, catg=="baimaiW+FHVR4"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW+FHVR5" ~ subset(tidy_data, catg=="baimaiW+FHVR5"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW+RingerR1" ~ subset(tidy_data, catg=="baimaiW+RingerR1"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW+RingerR2" ~ subset(tidy_data, catg=="baimaiW+RingerR2"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW+RingerR3" ~ subset(tidy_data, catg=="baimaiW+RingerR3"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW+RingerR4" ~ subset(tidy_data, catg=="baimaiW+RingerR4"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW+RingerR5" ~ subset(tidy_data, catg=="baimaiW+RingerR5"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW-FHVR1" ~ subset(tidy_data, catg=="baimaiW-FHVR1"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW-FHVR2" ~ subset(tidy_data, catg=="baimaiW-FHVR2"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW-FHVR3" ~ subset(tidy_data, catg=="baimaiW-FHVR3"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW-FHVR4" ~ subset(tidy_data, catg=="baimaiW-FHVR4"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW-FHVR5" ~ subset(tidy_data, catg=="baimaiW-FHVR5"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW-RingerR1" ~ subset(tidy_data, catg=="baimaiW-RingerR1"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW-RingerR2" ~ subset(tidy_data, catg=="baimaiW-RingerR2"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW-RingerR3" ~ subset(tidy_data, catg=="baimaiW-RingerR3"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW-RingerR4" ~ subset(tidy_data, catg=="baimaiW-RingerR4"& DPI == 1)[["sobreviventes"]],
    catg == "baimaiW-RingerR5" ~ subset(tidy_data, catg=="baimaiW-RingerR5"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW+FHVR1" ~ subset(tidy_data, catg=="triaurariaW+FHVR1"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW+FHVR2" ~ subset(tidy_data, catg=="triaurariaW+FHVR2"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW+FHVR3" ~ subset(tidy_data, catg=="triaurariaW+FHVR3"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW+FHVR4" ~ subset(tidy_data, catg=="triaurariaW+FHVR4"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW+FHVR5" ~ subset(tidy_data, catg=="triaurariaW+FHVR5"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW+RingerR1" ~ subset(tidy_data, catg=="triaurariaW+RingerR1"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW+RingerR2" ~ subset(tidy_data, catg=="triaurariaW+RingerR2"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW+RingerR3" ~ subset(tidy_data, catg=="triaurariaW+RingerR3"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW+RingerR4" ~ subset(tidy_data, catg=="triaurariaW+RingerR4"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW+RingerR5" ~ subset(tidy_data, catg=="triaurariaW+RingerR5"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW-FHVR1" ~ subset(tidy_data, catg=="triaurariaW-FHVR1"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW-FHVR2" ~ subset(tidy_data, catg=="triaurariaW-FHVR2"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW-FHVR3" ~ subset(tidy_data, catg=="triaurariaW-FHVR3"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW-FHVR4" ~ subset(tidy_data, catg=="triaurariaW-FHVR4"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW-FHVR5" ~ subset(tidy_data, catg=="triaurariaW-FHVR5"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW-RingerR1" ~ subset(tidy_data, catg=="triaurariaW-RingerR1"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW-RingerR2" ~ subset(tidy_data, catg=="triaurariaW-RingerR2"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW-RingerR3" ~ subset(tidy_data, catg=="triaurariaW-RingerR3"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW-RingerR4" ~ subset(tidy_data, catg=="triaurariaW-RingerR4"& DPI == 1)[["sobreviventes"]],
    catg == "triaurariaW-RingerR5" ~ subset(tidy_data, catg=="triaurariaW-RingerR5"& DPI == 1)[["sobreviventes"]],
    TRUE ~ sobreviventes
  )) %>%
  mutate(prop =  (sobreviventes*100)/total)


# plots -------------------------------------------------------------------

#todas as replicas e todos as especies (n bruto)
tidy_data %>%
  mutate(catg2 = str_c(wolbachia, tratamento, sep = " ")) %>%
  ggplot(aes(x = DPI, y = sobreviventes, color = catg2, shape = replica)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ especie, nrow = 2)

#mesmo do anterior (n proporcional)
rep_prop %>%
  mutate(catg2 = str_c(wolbachia, tratamento, sep = " ")) %>%
  ggplot(aes(x = DPI, y = prop, color = catg2, shape = replica)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ especie, nrow = 2) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(y = "Survival (%)", color = NULL)

ggsave("../plots/replicas.png")

# por especie -------------------------------------------------------------

#boc
rep_prop %>%
  filter(especie == "bocqueti") %>%
  mutate(catg2 = str_c(wolbachia, tratamento, sep = " ")) %>%
  ggplot(aes(x = DPI, y = prop, shape = replica, color = catg2))+
  geom_point() +
  geom_line() +
  facet_wrap(~ catg2, nrow = 2) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(y = "Survival (%)", color = NULL, title = "D. bocqueti") 

#tri
rep_prop %>%
  filter(especie == "triauraria") %>%
  mutate(catg2 = str_c(wolbachia, tratamento, sep = " ")) %>%
  ggplot(aes(x = DPI, y = prop, shape = replica, color = catg2))+
  geom_point() +
  geom_line() +
  facet_wrap(~ catg2, nrow = 2) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(y = "Survival (%)", color = NULL, title = "D. triauraria") 

#melano
rep_prop %>%
  filter(especie == "melanogaster") %>%
  mutate(catg2 = str_c(wolbachia, tratamento, sep = " ")) %>%
  ggplot(aes(x = DPI, y = prop, shape = replica, color = catg2))+
  geom_point() +
  geom_line() +
  facet_wrap(~ catg2, nrow = 2) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(y = "Survival (%)", color = NULL, title = "D. melanogaster")
