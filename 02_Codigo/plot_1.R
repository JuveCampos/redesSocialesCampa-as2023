library(tidyverse)

# Fuente: https://es.statista.com/estadisticas/1035031/mexico-porcentaje-de-usuarios-por-red-social/
statista_rrss <- tibble::tribble(
       ~Característica, ~Porcentaje.de.personas,
            "WhatsApp",                 "94,3%",
            "Facebook",                 "93,4%",
  "Facebook Messenger",                 "80,5%",
           "Instagram",                 "79,1%",
              "TikTok",                 "70,4%",
             "Twitter",                   "56%",
           "Pinterest",                   "46%",
            "Telegram",                 "39,9%",
            "Snapchat",                 "29,8%",
            "LinkedIn",                 "21,6%",
             "Discord",                 "15,7%",
            "iMessage",                 "15,2%",
              "Reddit",                 "12,2%"
  )

bd_plot <- statista_rrss %>%
  mutate(Porcentaje.de.personas = str_remove_all(Porcentaje.de.personas, pattern = "\\%")) %>%
  mutate(Porcentaje.de.personas = str_replace_all(Porcentaje.de.personas, pattern = "\\,", replacement = ".")) %>%
  mutate(Porcentaje.de.personas = as.numeric(Porcentaje.de.personas))

# str_c('"', statista_rrss$Característica, '" = ') %>% writeLines()

colores <- c("WhatsApp" = "#6dc366",
               "Facebook" = "#3771e4",
               "Facebook Messenger" = "#b248bc",
               "Instagram" = "#943f99",
               "TikTok" = "black",
               "Twitter" = "#4a99ea",
               "Pinterest" = "#a82525",
               "Telegram" = "#4c94c9",
               "Snapchat" = "#e9e050",
               "LinkedIn" = "#306fa9",
               "Discord" = "#5560dd",
               "iMessage" = "#82ed7d",
               "Reddit" = "#df5126")

bd_plot %>%
  ggplot(aes(x = reorder(Característica, Porcentaje.de.personas),
             y = Porcentaje.de.personas,
             fill = Característica)) +
  geom_col() +
  geom_text(aes(label = str_c(format(Porcentaje.de.personas, nsmall = 1), "%")),
            hjust = -.1,
            family = "Montserrat") +
  coord_flip() +
  scale_fill_manual(values = colores) +
  scale_y_continuous(expand = expansion(c(0, 0.3))) +
  labs(x = NULL, y = NULL,
       title = "Porcentaje de usuarios por red social en México",
       subtitle = "con respecto al total de usuarios de redes sociales en el país, 2022",
       caption = "Fuente: https://es.statista.com/estadisticas/1035031/mexico-porcentaje-de-usuarios-por-red-social/") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "Montserrat"),
        axis.text.y = element_text(face = "bold"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

ggsave("03_Visualizaciones/grafica_porcentaje_usuarios.png",
       height = 6,
       width = 6.5,
       device = "png")

