# title: "TidyTuesday 2023/4 - ALONE data by {Alone}}"
# author: "Johan S. Sáenz"
# date: "2023-01-28"

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(ggradar)
library(ggtext)
library(showtext)
library(glue)
library(emojifont)
library(here)

# load data --------------------------------------------------------------------
survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')
loadouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')

# wrangling data ---------------------------------------------------------------

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

# load fonts and titles --------------------------------------------------------
font_add_google("Secular One", "secularone")
font_add_google("Roboto", "roboto")

# Showtext will be automatically invoked when needed
showtext_auto()

logo_txt <- "white"
  
twitter <- glue("<span style='font-family:\"fontawesome-webfont\";color:{logo_txt}'>{emojifont::fontawesome('fa-twitter')}</span>")

github <- glue("<span style='font-family:\"fontawesome-webfont\";color:{logo_txt}'>{emojifont::fontawesome('fa-github')}</span>")

title <- str_wrap(glue("A KIT TO SURVIVE ALONE"))
subtitle <- str_wrap(glue("To survive in the wilderness for as long as possible, you need a trapping wire, a<br /> gillnet and a multitool. Those tools were the most popular among the <b style='color:darkgreen'>winners</b><br /> of the TV-show ALONE compared with the <b style='color:#3e363f'>survivalists that first tapped out</b>. The<br /> radarplot shows the proportion of survivalists that selected each object."))
caption <- str_wrap(glue(
  "Data source: Alone package • {twitter} @SaenzJohanS • Code: {github} SebasSaenz/TidyTuesday"))

# plot -------------------------------------------------------------------------
plot <- df %>% 
  ggradar(font.radar = "secularone",
          group.colours = c("#3e363f", "darkgreen"),
          grid.label.size = 12,
          axis.label.size = 12,
          group.point.size = 2,
          group.line.width = 0.6,
          fill = TRUE,
          fill.alpha = 0.4,
          background.circle.colour = "#547C73",
          axis.line.colour = "grey",
          gridline.min.colour = "black",
          gridline.mid.colour = "black",
          gridline.max.colour = "black",
          legend.position = "bottom",
          plot.extent.x.sf = 1.3) +
  labs(title = title,
       subtitle = subtitle,
       caption = caption) +
  theme(plot.background = element_rect(colour = "#547C73", 
                                       fill = "#547C73"),
        panel.background = element_rect(colour = "#547C73",
                                        fill = "#547C73"),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text = element_text(colour = "white",
                                   size = 40),
        legend.background = element_blank(),
        plot.title = element_text(family = "secularone",
                                  size = 70,
                                  color = "#003554",
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_markdown(family = "secularone",
                                         size= 35,
                                         color = "#003554",
                                         lineheight = 0.4,
                                         hjust = 0),
        plot.caption = element_markdown(family="roboto",
                                        colour = "white",
                                        hjust = 0.5, 
                                        size = 30),
        plot.margin = margin(5,5,5,5))

#solution for changeing the radar font color
plot$layers[[1]]$aes_params <- c(plot$layers[[1]]$aes_params, colour = "white")
plot$layers[[5]]$aes_params <- c(plot$layers[[5]]$aes_params, colour = "white")
plot$layers[[6]]$aes_params <- c(plot$layers[[6]]$aes_params, colour = "white")
plot$layers[[12]]$aes_params <- c(plot$layers[[12]]$aes_params, colour = "white")
plot$layers[[13]]$aes_params <- c(plot$layers[[13]]$aes_params, colour = "white")
plot$layers[[14]]$aes_params <- c(plot$layers[[14]]$aes_params, colour = "white")


# save-plot --------------------------------------------------------------------
ggsave(plot, filename = here::here("plots", "2023/alone.png"), width = 5, height = 5, dpi = 400)

