library(tidyverse)
library(ggradar)
library(ggtext)
library(scales)
library(showtext)
library(glue)
library(emojifont)


survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')
loadouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/seasons.csv')

season4_last_position <- survivalists %>% 
  filter(season == 4 & result == 7) %>% 
  inner_join(loadouts, by = c("name", "season")) %>% 
  select(name, result, item) %>% 
  mutate(result = ifelse(result == 7, 10, result))
  


df <- survivalists %>% 
  inner_join(loadouts, by = c("name", "season")) %>% 
  filter(result == 1 | result == 10) %>% 
  select(name, result, item) %>%
  rbind(season4_last_position) %>% 
  count(result, item) %>%
  #filter(n > 1) %>% 
  complete(result, item) %>% 
  mutate(result =ifelse(result==1, "Winner", "First tapped out"),
         n = ifelse(is.na(n), 0, n),
         n = ifelse(n > 10, 10, n), # saws sum 11 because one participant took 2
         n = n/10) %>% 
  pivot_wider(names_from = item, values_from = n) %>% 
  rename(group = result)
  
font_add_google("Secular One", "secularone")
font_add_google("Roboto", "roboto")

# Showtext will be automatically invoked when needed
showtext_auto()


txt <- "white"

twitter <- glue("<span style='font-family:\"fontawesome-webfont\";color:{txt}'>{emojifont::fontawesome('fa-twitter')}</span>")

github <- glue("<span style='font-family:\"fontawesome-webfont\";color:{txt}'>{emojifont::fontawesome('fa-github')}</span>")

caption <- str_wrap(glue(
    "Data source: rtrek • {twitter} @SaenzJohanS • Code: {github} SebasSaenz/TidyTuesday"), 2000)



plot <- df %>% 
  ggradar(font.radar = "secularone",
          group.colours = c("#3e363f", "darkgreen"),
          grid.label.size = 8,
          axis.label.size = 4,
          group.point.size = 5,
          group.line.width = 1,
          fill = TRUE,
          fill.alpha = 0.4,
          background.circle.colour = "#547C73",
          axis.line.colour = "darkgrey",
          gridline.min.colour = "black",
          gridline.mid.colour = "black",
          gridline.max.colour = "black",
          legend.position = "bottom") +
  labs(title = "A kit to survive ALONE",
       subtitle = str_wrap(glue("To survive you need a trapping wire, gillnet and multitool. Those<br />
       tools were the most popular among the winners of the TV-show<br /> ALONE
       compared with the survivalists that first<br /> tapped out. The radarplot shows 
       the proportion of survivalists<br /> that selected each object")),
       caption = caption) +
  theme(plot.background = element_rect(colour = "#547C73", fill = "#547C73"),
        panel.background = element_rect(colour = "#547C73", fill = "#547C73"),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text = element_text(colour = "white",
                                   size = 20),
        legend.background = element_blank(),
        plot.title = element_text(family = "secularone",
                                  size = 40,
                                  color = "#003554",
                                  face = "bold"),
        plot.subtitle = element_markdown(family = "secularone",
                                         size= 13,
                                         color = "#003554"),
        plot.caption = element_markdown(family="roboto",
                                        colour = "white",
                                        hjust = 0.5, 
                                        size = 11))

plot$layers[[1]]$aes_params <- c(plot$layers[[1]]$aes_params, colour = "white")
plot$layers[[5]]$aes_params <- c(plot$layers[[5]]$aes_params, colour = "white")
plot$layers[[6]]$aes_params <- c(plot$layers[[6]]$aes_params, colour = "white")
plot$layers[[12]]$aes_params <- c(plot$layers[[12]]$aes_params, colour = "white")
plot$layers[[13]]$aes_params <- c(plot$layers[[13]]$aes_params, colour = "white")
plot$layers[[14]]$aes_params <- c(plot$layers[[14]]$aes_params, colour = "white")
plot

