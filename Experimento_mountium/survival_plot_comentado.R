library(tidyverse)
library(lubridate)
library(plotrix) #sei esse pacote só para calcular o erro padrao mas nao sei se ta certo


data <- read_delim("experimento_mountium.csv", delim = ";")

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