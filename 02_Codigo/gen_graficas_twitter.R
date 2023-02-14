# Analisis Tweets
library(tidyverse)
library(emojifont)
library(scales)
library(extrafont)

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

excel <- list.files("01_Datos/tweets",
                    pattern = ".xlsx",
           full.names = T)

datos <- lapply(excel, readxl::read_xlsx) %>%
  do.call(rbind, .)

followers_twitter <- datos %>%
  select(screen_name, Estado, followers_count) %>%
  unique() %>%
  arrange(-followers_count) %>%
  filter(Estado == "Coahuila") %>%
  rbind.data.frame(tibble(screen_name = "leninperezr",
                            Estado = "Coahuila",
                            followers_count = 7982))

followers_twitter %>%
  ggplot(aes(x = str_c("@", screen_name),
             y = followers_count)) +
  geom_col(fill = "#4a99ea") +
  geom_text(aes(label = followers_count),
            vjust = -.5,
            family = "Montserrat",
            size = 5) +
  scale_y_continuous(expand = expansion(c(0, 0.3))) +
  labs(x = NULL, y = NULL,
       title = "Número de followers en Twitter",
       subtitle = "Al 25 de enero del 2023",
       caption = "Fuente: API de Twitter") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "Montserrat"),
        axis.text.x = element_text(face = "bold"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

ggsave("03_Visualizaciones/followers_tw.png",
       height = 5,
       width = 5.5,
       device = "png")

# Tweets de la última semana:

datos %>%
  filter(Estado == "Coahuila") %>%
  group_by(screen_name) %>%
  mutate(n = n()) %>%
  mutate(screen_name = str_c("@", screen_name, "\n",
                             n, " tweets"
                             )) %>%
  mutate(screen_name = factor(screen_name, levels = rev(sort(unique(screen_name))))) %>%
  ggplot(aes(y = created_at,
             x = screen_name)) +
  # geom_jitter(width = 0.1,
  #             pch = 21,
  #             fill = "#4a99ea") +
  # geom_text(aes(label = fontawesome(c('fa-twitter'))),
  #           family='fontawesome-webfont',
  #           size=7) +
  geom_text(aes(label = fontawesome(c('fa-twitter'))),
            family='fontawesome-webfont',
            size=6,
            color = "#4a99ea") +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "Tweets de los pre-candidatos durante la última semana",
       subtitle = "Periodo del 16 al 26 de enero del 2023",
       caption = "Fuente: API de Twitter") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "Montserrat"),
        axis.text.y = element_text(face = "bold"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

ggsave("03_Visualizaciones/tweets_actividad.png",
       height = 6,
       width = 5.5,
       device = "png")
