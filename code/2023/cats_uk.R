# title: "TidyTuesday 2023/5 - ALONE data by {Alone}}"
# author: "Johan S. Sáenz"
# date: "2023-01-28"

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)#
library(jpeg)
library(patchwork)
library(ggtext)
library(showtext)
library(emojifont)
library(glue)

# load data --------------------------------------------------------------------
cats_uk <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')
cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')


# wrangling data ---------------------------------------------------------------

# calcultae days tracked per cat
days_per_cat <- cats_uk %>% 
  mutate(yeartime = ymd_hms(timestamp),
         date = as.Date(yeartime)) %>% 
  group_by(tag_id) %>% 
  summarise(max= max(date),
            min=min(date)) %>% 
  mutate(days = as.numeric(difftime(max, min, units = "days")))

clean_df <- cats_uk %>% 
  mutate(yeartime = ymd_hms(timestamp), 
         date = as.Date(yeartime), 
         hour = as.numeric(format(yeartime, format = "%H")),
         movement = if_else(ground_speed > 0, 1, ground_speed)) %>% 
  select(tag_id, hour, movement) %>% 
  mutate(time_of_day = case_when(
                                 hour <= 6 ~ "Night",
                                 hour <= 10 ~ "Morning",
                                 hour <= 14 ~ "Midday",
                                 hour <= 18 ~ "Afternoon",
                                 hour >= 19 ~ "Night")) %>% 
  group_by(tag_id, time_of_day) %>% 
  summarise(sum_movement = sum(movement)) %>% 
  select(tag_id, sum_movement, time_of_day) %>% 
  inner_join(days_per_cat, by = "tag_id") %>% 
  group_by(tag_id, time_of_day) %>% 
  mutate(move_per_day = sum_movement/days,
         move_per_day = if_else(is.infinite(move_per_day), 0, move_per_day)) %>% 
  inner_join(cats_uk_reference, by = "tag_id") 

#calculate mean of movement per sex and day time
mean_movement <- clean_df %>% 
  group_by(animal_sex, time_of_day) %>% 
  summarise(mean =mean(move_per_day), .groups = "drop")

clean_df$time_of_day <- factor(clean_df$time_of_day,
                               levels = c("Night", "Morning", "Midday", "Afternoon"))
#load cat eyes images
img <- readJPEG(source = "images/cat2.jpeg",
                native = TRUE)

# load fonts, titles and image -------------------------------------------------
font_add_google("Fjalla One", "fjallaone")
font_add_google("Indie Flower", "indieflower")
font_add_google("Roboto", "roboto")
showtext_auto()

logo_txt <- "white"
  
twitter <- glue("<span style='font-family:\"fontawesome-webfont\";color:{logo_txt}'>{emojifont::fontawesome('fa-twitter')}</span>")

github <- glue("<span style='font-family:\"fontawesome-webfont\";color:{logo_txt}'>{emojifont::fontawesome('fa-github')}</span>")

title <- str_wrap(glue("AT NIGHT I MOVE"))
subtitle <- str_wrap(glue("<b style='color:#11eeee'>Female</b> and <b style='color:#11ee11'>male</b> cats move more during the night compared<br />
       to the rest of the day.<br /> <b style='font-size:50px'>Rhombus = mean.</b>"))
caption <- str_wrap(glue(
  "Data source: Movebank for Animal Tracking • {twitter} @SaenzJohanS • Code: {github} SebasSaenz/TidyTuesday"))

# plot -------------------------------------------------------------------------
plot <- clean_df %>% 
  ggplot() +
  geom_point(aes(x = time_of_day,
                  y = move_per_day,
                  color = animal_sex,
                  group = animal_sex),
             alpha = 0.6,
             position=position_jitterdodge(seed = "1998",
                                           jitter.width = 0.3)) +
  geom_point(data=mean_movement,
             aes(x=time_of_day,
                 y=mean,
                 group = animal_sex),
             color="white",
             position= position_dodge(width = 0.9),
             shape=18,
             size=4) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,120),
                     breaks = seq(0, 120, 15)) +
  scale_color_manual(values = c("#11eeee", "#11ee11")) +
  labs(y = "Movement per day",
       x = NULL,
       title = title,
       subtitle = subtitle,
       caption = caption) +
  theme_classic() +
  theme(plot.background = element_rect(colour = "black", fill = "black"),
        panel.background = element_rect(colour = "black", fill = "black"),
        legend.position = "none",
        axis.text =  element_text(family = "fjallaone",
                                  colour = "white",
                                  size = 30),
        axis.title =  element_text(family = "fjallaone",
                                   colour = "white",
                                   size = 40),
        plot.title = element_text(family = "fjallaone",
                                  colour = "white",
                                  face = "bold",
                                  size = 90,
                                  hjust = 0),
        plot.subtitle = element_markdown(family = "indieflower",
                                         colour = "white",
                                         face = "bold",
                                         size = 50,
                                         hjust = 0,
                                         margin = margin(5,0,15,0),
                                         lineheight = 0.1),
        plot.caption = element_markdown(family="roboto",
                                        colour = "white",
                                        hjust = 0.5, 
                                        size = 27,
                                        margin = margin(10, 0, 0, 0))) +
  annotation_raster(img,
                    ymin = 80,
                    ymax = 120,
                    xmin = 2,
                    xmax = 4)

# save-plot --------------------------------------------------------------------
ggsave(plot,
       file="plots/2023/cats_uk.png",
       width = 5,
       height = 5,
       dpi = 400)






  