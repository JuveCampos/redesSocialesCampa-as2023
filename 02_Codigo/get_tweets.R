library(tidyverse)
library(rtweet)

# Base de datos madre:
bd <- readxl::read_xlsx("01_Datos/rrss_candidatos.xlsx")

# Bases de datos locales:
bd_coah <- bd %>%
  # filter(Estado == "Coahuila") %>%
  mutate(consulta = str_c("(from:", Twitter, ")")) %>%
  filter(!is.na(consulta))

# Construimos la consulta:
consulta = str_c("(from:", bd_coah$Twitter, ")") %>% na.omit()

get_tweets <- function(i){
  # i = 1
  tryCatch({
    datos_i <- bd_coah[i,]
    tweets <- rtweet::search_tweets(datos_i$consulta,
                                    n = 1000,
                                    include_rts = T,
                                    retryonratelimit = T)

    t2 <- tweets %>%
      select(user_id, status_id, created_at, screen_name, text,
             display_text_width, is_retweet, favorite_count, retweet_count,
             hashtags, media_url,
             status_url, followers_count,
      ) %>%
      mutate(Estado = datos_i$Estado)

    t2$hashtags  <- unlist(lapply(t2$hashtags, function(x) str_c(x, collapse = ";")))
    t2$media_url <- unlist(lapply(t2$media_url, function(x) str_c(x, collapse = ";")))

    openxlsx::write.xlsx(t2, str_c("01_Datos/tweets/",
                                   str_to_lower(str_replace_all(str_c(datos_i$Nombre), pattern = "\\s", replacement = "_")),
                                   "__",
                                   Sys.time() %>% str_replace_all(pattern = "\\-|\\s|\\:", replacement = "_"),
                                   ".xlsx"))
    print(i)
  }, error = function(e){
    print(str_c("Error en el elemento ", i))
  })
}

# Descargamos los tweets
lapply(1:nrow(bd_coah), get_tweets)
