library(tidyverse)

bd <- readxl::read_xlsx("01_Datos/rrss_candidatos.xlsx") %>%
   filter(Estado == "Coahuila")

matriz <- bd %>%
  mutate(across(.cols = 4:ncol(.),
                .fns = function(x){
                  ifelse(is.na(x),
                         yes = 0,
                         no = 1)
                }))

matriz %>%
  pivot_longer(cols = 4:ncol(.)) %>%
  ggplot(aes(x = name,
             y = Nombre,
             fill = factor(value))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "gray",
                               "1" = "navyblue")) +
  labs(x = NULL, y = NULL,
       title = "¿Los precandidatos tienen las siguientes redes sociales?",
       subtitle = "Al 25 de Enero del 2023") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "Montserrat"),
        axis.text.y = element_text(face = "bold" , size = 15),
        axis.text.x = element_text(angle = 90, size = 15),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 15))

ggsave("03_Visualizaciones/matriz_redes.png",
       height = 6,
       width = 10)

# Followers_total
bd2 <- readxl::read_xlsx("01_Datos/rrss_candidatos.xlsx",
                         sheet = 2) %>%
  mutate(Nombre = factor(Nombre, levels = rev(sort(unique(Nombre)))))


bd2 %>%
  pivot_longer(cols = 4:ncol(.)) %>%
  ggplot(aes(x = Nombre,
             y = value,
             fill = Partido,
             label = prettyNum(value, big.mark = ","))) +
  geom_col() +
  geom_text(hjust = -0.15,
             family = "Montserrat") +
  scale_fill_manual(values = c("MORENA" = "brown",
                               "PRI" = "#164a34",
                               "PT" = "#850d09",
                               "UDC" = "orange")) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  facet_wrap(~name, scales = "free_x") +
  scale_y_continuous(expand = expansion(c(0, 0.5), 0)) +
  labs(title = "Números de Redes Sociales para los pre-candidatos de Coahuila",
       subtitle = "Cifras al 15 de Enero del 2023",
       caption = "Fuente: Elaboración propia con datos de sus perfiles sociales.
       Las cifras del canal de Youtube de Ricardo Mejía correspondiente al canal de 'Amigos de Ricardo Mejía'") +
  theme_bw() +
  theme(
    legend.position = "none",
        text = element_text(family = "Montserrat"),
        axis.text.x = element_blank(),
          # element_text(face = "bold" , size = 15),
        panel.grid = element_blank(),
        axis.text.y = element_text(angle = 0, size = 15),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(face = "bold", size = 18),
        plot.title = element_text(face = "bold", size = 18),
    plot.caption = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 15))

ggsave("03_Visualizaciones/redes_totales_seguidores.png",
       height = 7,
       width = 10,
       device = "png")
