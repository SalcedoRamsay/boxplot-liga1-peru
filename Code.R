# OBTENER LOS VALORES DE MERCADOS Y OTROS DATOS DE TRANSFERMARKET DE LA LIGA 1

## 1. Cargar Librerías ----
library(worldfootballR)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(ggplot2)
library(forcats)
library(ggrepel)
library(cowplot)
library(magick)

##2. Scrapear los jugadores de la Liga 1 - Apertura 2025 ----
url_ligape ="https://www.transfermarkt.com/liga-1-apertura/startseite/wettbewerb/TDeA"

#Esta función extrae los nombres y URLs de los jugadores de un equipo en Transfermarkt.
get_players_from_team <- function(team_url) {
  page <- read_html(team_url)
  players <- page %>%
    html_nodes(".inline-table .hauptlink a") %>%
    html_attr("href") %>%
    unique() %>%
    paste0("https://www.transfermarkt.com", .)
  names <- page %>%
    html_nodes(".inline-table .hauptlink a") %>%
    html_text() %>%
    unique()
  tibble(player_name = names, player_url = players)
}

#Aplica la función a todos los equipos y crea un data frame con los nombres y URLS.
jugadores_ligape <- NULL
for(team_url in equipos_ligape) {
  temp <- get_players_from_team(team_url) %>%
    mutate(team_name = str_match(team_url, ".com/(.*?)/")[2])
  jugadores_ligape <- bind_rows(jugadores_ligape, temp)
}

#Función para extraer información de un jugador con los datos que se encuentran en Transfermarket.
get_player_info <- function(player_url) {
  player_page <- read_html(player_url)
  
  player_meta_keywords <- player_page %>%
    html_node("meta[name='keywords']") %>%
    html_attr("content")
  
  player_meta_description <- player_page %>%
    html_node("meta[name='description']") %>%
    html_attr("content")
  
  player_name <- strsplit(player_meta_keywords, ",")[[1]][1]
  
  player_age <- str_extract(player_meta_description, "\\d{2}, from") %>%
    str_replace(", from", "")
  
  player_birth_date <- str_extract(player_meta_description, "\\*\\s[A-Za-z]+\\s\\d{2},\\s\\d{4}") %>%
    str_replace_all("\\*", "") %>%
    str_trim()
  
  player_club <- str_extract(player_meta_description, "➤\\s[^,]+, since") %>%
    str_replace("➤", "") %>%
    str_replace(", since", "") %>%
    str_trim()
  
  player_positions <- player_page %>%
    html_nodes(".detail-position__position") %>%
    html_text(trim = TRUE)
  
  player_positions <- player_positions[!grepl("Other position:", player_positions)]
  player_position <- ifelse(length(player_positions) > 0, paste(unique(player_positions), collapse = ", "), NA)
  
  player_valuation <- str_extract(player_meta_description, "Market value: €[0-9\\.]+[mk]") %>%
    str_replace("Market value: ", "")
  
  player_name_native <- player_page %>%
    html_nodes(".info-table__content--bold") %>%
    html_text(trim = TRUE) %>%
    .[1]
  
  player_birth_place_list <- player_page %>%
    html_nodes(".info-table__content--regular, .info-table__content--bold") %>%
    html_text(trim = TRUE)
  
  place_index <- which(player_birth_place_list == "Place of birth:")
  player_birth_place <- ifelse(length(place_index) > 0 && place_index < length(player_birth_place_list),
                               player_birth_place_list[place_index + 1], NA)
  
  player_height <- player_page %>%
    html_nodes(".info-table__content--regular, .info-table__content--bold") %>%
    html_text(trim = TRUE) %>%
    grep("m$", ., value = TRUE) %>%
    .[1]
  
  player_citizenship <- player_page %>%
    html_nodes(".flaggenrahmen") %>%
    html_attr("title") %>%
    unique() %>%
    paste(collapse = ", ")
  
  player_foot <- player_page %>%
    html_nodes(".info-table__content--bold") %>%
    html_text(trim = TRUE) %>%
    grep("left|right", ., value = TRUE) %>%
    .[1]
  
  player_agent <- player_page %>%
    html_nodes(".info-table__content--regular, .info-table__content--bold") %>%
    html_text(trim = TRUE)
  agent_index <- which(player_agent == "Player agent:")
  if (length(agent_index) > 0 && agent_index < length(player_agent)) {
    player_agent <- player_agent[agent_index + 1]
  } else {
    player_agent <- NA
  }
  
  player_joined <- player_page %>%
    html_nodes(".info-table__content--regular, .info-table__content--bold") %>%
    html_text(trim = TRUE)
  joined_index <- which(player_joined == "Joined:")
  player_joined <- ifelse(length(joined_index) > 0 && joined_index < length(player_joined),
                          player_joined[joined_index + 1], NA)
  
  player_contract_expires <- player_page %>%
    html_nodes(".info-table__content--regular, .info-table__content--bold") %>%
    html_text(trim = TRUE)
  contract_index <- which(player_contract_expires == "Contract expires:")
  player_contract_expires <- ifelse(length(contract_index) > 0 && contract_index < length(player_contract_expires),
                                    player_contract_expires[contract_index + 1], NA)
  
  player_contract_extension <- player_page %>%
    html_nodes(".info-table__content--regular, .info-table__content--bold") %>%
    html_text(trim = TRUE)
  extension_index <- which(player_contract_extension == "Last contract extension:")
  player_contract_extension <- ifelse(length(extension_index) > 0 && extension_index < length(player_contract_extension),
                                      player_contract_extension[extension_index + 1], NA)
  
  player_outfitter <- player_page %>%
    html_nodes(".info-table__content--bold") %>%
    html_text(trim = TRUE) %>%
    grep("Nike|Adidas|Puma", ., value = TRUE) %>%
    .[1]
  
  player_twitter <- player_page %>%
    html_node(".social-media-toolbar__icons a[title='Twitter']") %>%
    html_attr("href")
  
  player_facebook <- player_page %>%
    html_node(".social-media-toolbar__icons a[title='Facebook']") %>%
    html_attr("href")
  
  player_instagram <- player_page %>%
    html_node(".social-media-toolbar__icons a[title='Instagram']") %>%
    html_attr("href")
  
  tibble(
    player_name, player_age, player_birth_date, player_club, player_position, player_valuation, URL = player_url,
    player_name_native, player_birth_place, player_height, player_citizenship,
    player_foot, player_agent, player_joined, player_contract_expires, player_contract_extension, player_outfitter,
    player_twitter, player_facebook, player_instagram
  )
}

#Aplica la función a todos los jugadores de la Liga 1 creando un data frame
players_ligape_data <- map_df(jugadores_ligape$player_url, possibly(get_player_info, otherwise = tibble()))

##3. Crear el boxplot de Liga 1 - Apertura 2025 ----

# Función para convertir valores de mercado a números retirando los simbolos (m o k)
convert_valuation <- function(value) {
  if (is.na(value)) {
    return(NA)
  }
  numeric_value <- as.numeric(str_extract(value, "[0-9\\.]+"))
  unit <- str_extract(value, "[mk]")
  if (unit == "m") {
    return(numeric_value * 1e6)
  } else if (unit == "k") {
    return(numeric_value * 1e3)
  } else {
    return(NA)
  }
}

#Aplicamos la transformación
players_ligape_data_cleaned <- players_ligape_data %>%
  mutate(player_valuation = sapply(player_valuation, convert_valuation))

# Filtramos los jugadores sin valor de mercado y filtramos los 25 mejores valorados de cada club.
data_ligape <- players_ligape_data_cleaned %>%
  filter(!is.na(player_valuation)) %>%
  group_by(player_club) %>%
  mutate(rank = rank(-player_valuation, ties.method = "first")) %>%
  filter(rank <= 25) %>% 
  mutate(
    upper_bound = quantile(player_valuation, 0.75, na.rm = TRUE),
    lower_bound = quantile(player_valuation, 0.25, na.rm = TRUE),
    is_outlier = ifelse(player_valuation > (upper_bound + (upper_bound - lower_bound) * 1.5), 1, 0),
    player_value_M_EUR = player_valuation / 1e6
  ) %>%
  ungroup()

#Se escoge los colores del gráfico y de los clubes.
col_text_and_lines = "#252525"
col_background = "grey80"
club_colors <- c(
  "Club Sporting Cristal" = "#00ADEF",
  "Universitario de Deportes" = "#7A0019",
  "Club Alianza Lima" = "#002060",
  "FBC Melgar" = "#A52A2A",
  "Cusco FC" = "#FFCC00",
  "Club Cienciano" = "#D71920",
  "Alianza Atlético Sullana" = "#0033CC",
  "Asociación Deportiva Tarma" = "#0080FF",
  "Universidad Técnica de Cajamarca" = "#A5A5A5",
  "Club Atlético Grau" = "#FFCC00",
  "Alianza Universidad" = "#003366",
  "Sport Huancayo" = "#FF0000",
  "CD Los Chankas" = "#8A1538",
  "Sport Boys Association" = "#F9A8D4",
  "Comerciantes Unidos" = "#006633",
  "Deportivo Garcilaso" = "#004AAD",
  "Club Juan Pablo II" = "#FFD700",
  "Ayacucho FC" = "#D2691E",
  "Deportivo Binacional" = "#00BFFF"
)

#Creación del boxplot
p1 = ggplot(data_ligape, aes(x = fct_reorder(player_club, player_value_M_EUR), 
                             y = player_value_M_EUR, fill = player_club, color = player_club)) +
  geom_boxplot(alpha = 0.65, outlier.shape = NA) +  # outlier.shape = NA para que no doble dibuje los puntos
  geom_point(data = data_ligape %>% filter(is_outlier == 1), 
             aes(color = player_club), size = 2) + 
  geom_text_repel(data_ligape %>% filter(is_outlier == 1), 
                  mapping = aes(label = player_name, color = player_club), size = 2) +
  scale_fill_manual(values = club_colors) +
  scale_color_manual(values = club_colors) +
  scale_y_continuous(breaks = seq(0, max(data_ligape$player_value_M_EUR, na.rm = TRUE), 0.5)) +
  coord_flip() +
  theme_bw() +
  guides(fill = "none", colour = "none") +
  labs(
    x = "", 
    y = "\nValor de mercado\n[Millones de EUR]", 
    fill = "Club",
    title = "Distribución de valor de mercado en la Liga 1 - Apertura 2025",
    subtitle = "Se consideran los 25 jugadores mejor valorados de cada club al 05-03-2025",
    caption = "Data: Transfermarkt - Viz: Sebastián Salcedo"
  ) +
  theme(
    panel.background = element_rect(fill = col_background, colour = col_text_and_lines),
    plot.background = element_rect(fill = col_background, colour = "transparent"),
    legend.background = element_rect(fill = col_background, colour = "transparent"),
    legend.box.background = element_rect(fill = col_background, colour = "transparent"),
    panel.grid = element_line(colour = "grey20", size = 0.05),
    text = element_text(colour = col_text_and_lines, size = 10),
    axis.text.x = element_text(colour = col_text_and_lines),
    axis.text.y = element_text(colour = col_text_and_lines),
    legend.position = "top",
    legend.key = element_rect(fill = col_background),
    plot.margin = margin(1, 1, 0.5, 0.5, unit = "cm")
  )
 
p2 <- ggdraw() +
  draw_plot(p1) +
  draw_image("liga1-logo.png", x = 0.42, y = 0.46, scale = 0.08)  # Cambia el nombre por el logo que tengas
p2

ggsave("valores_ligape_2025.png", height = 9, width = 12)
