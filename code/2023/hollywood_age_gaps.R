# libraries --------------------------------------------------------------------
library(tidyverse)
library(ggdist)
library(ggtext)
library(showtext)
library(emojifont)
library(glue)

# load-data --------------------------------------------------------------------
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

# fonts ------------------------------------------------------------------------
font_add_google("Courgette", "courgette")
showtext_auto()

# wrangling-data  --------------------------------------------------------------

actor1 <- age_gaps %>% 
  select(actor_1_name, age_difference) %>% 
  rename(actor =actor_1_name, difference=age_difference)

actor2 <- age_gaps %>% 
  select(actor_2_name, age_difference) %>% 
  rename(actor =actor_2_name, difference=age_difference)

df <- rbind(actor1, actor2) %>% 
  group_by(actor) %>% 
  mutate(med = median(difference),
         min = min(difference),
         max = max(difference),
         n = n()) %>% 
  ungroup() %>% 
  mutate(start = fct_reorder(actor, med),
    y = as.numeric(start) - 1) %>% 
  filter(n >12)

# title ------------------------------------------------------------------------
logo_txt <- "white"
  
twitter <- glue("<span style='font-family:\"fontawesome-webfont\";color:{logo_txt}'>{emojifont::fontawesome('fa-twitter')}</span>")

github <- glue("<span style='font-family:\"fontawesome-webfont\";color:{logo_txt}'>{emojifont::fontawesome('fa-github')}</span>")

title <- str_wrap(glue("Hollywood Age Gaps"))
subtitle <- glue("From the actors that have played more frequently in a couples role, Roger Moore is the one with the<br> highest <b style='color:pink'>average age gap</b> with his <b style='color:red'>partners</b>.")
caption <- str_wrap(glue(
  "Data source: Hollywood Age Gaps • {twitter} @SaenzJohanS • Code: {github} SebasSaenz/TidyTuesday"))


# plot -------------------------------------------------------------------------
plot <- ggplot(df, 
       aes(difference, 
           y = y)) +
  geom_segment(aes(x=min, xend =max, y=y, yend=y),
               color="#b2a8df") +
  labs(y = NULL,
       x = "Age gap (years)") +
  geom_point(position = position_jitter(height = 5,
                                        width = 0.2,
                                        seed = 1988),
             size = 3,
             color = "red",
             fill = "red",
             shape = 23,
             alpha = 0.6) +
  geom_text(data = df,
            aes(x = min, 
                y = y,
                label = actor),
            family = "courgette",
            color = "white",
            fontface = "bold",
            hjust=1.2,
            size = 14) +
  geom_point(data = df,
             aes(x = med, 
                 y = y),
             shape = 23,
             color = "pink",
             fill = "pink",
             size = 5) +
  geom_text(data = df,
            aes(x = med + .16, 
                y = y - 10,
                label = glue::glue("{round(med, 1)} years")),
            family = "courgette",
            color = "white",
            size = 10,
            vjust = 1,
            hjust = 0) +
  scale_x_continuous(breaks = seq(0,40,5),
                     limits = c(-10, 40)) +
  labs(title = title,
       subtitle = subtitle,
       caption = caption) +
  theme_classic() +
  theme(plot.background = element_rect(colour = "#d4b9da",
                                       fill = "#d4b9da"),
        panel.background = element_rect(colour = "#d4b9da", fill = "#d4b9da"),
        legend.position = "none",
        text = element_text(colour = "white",
                            family = "courgette"),
        axis.text.x = element_text(color = "white",
                                   size = 50),
        axis.title.x = element_text(size = 50),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(family = "courgette",
                                  colour = "#6a51a3",
                                  face = "bold",
                                  size = 150,
                                  hjust = 0),
        plot.subtitle = element_markdown(family = "courgette",
                                         colour = "white",
                                         face = "bold",
                                         size = 50,
                                         hjust = 0,
                                         margin = margin(5,0,15,0),
                                         lineheight = 0.2),
        plot.caption=element_markdown(family="roboto",
                                      face = "bold",
                                      colour = "white",
                                      hjust = 0.5, 
                                      size = 30,
                                      margin = margin(25, 0, 0, 0)))

# save-plot---------------------------------------------------------------------
ggsave(plot, file = "plots/2023/age_gaps.png", width = 8, height = 6, dpi = 400)  

