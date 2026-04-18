# Load library -----------------------------------------------------------------

coffee_survey <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-14/coffee_survey.csv'
)

x <- coffee_survey %>%
  filter(
    political_affiliation == "Democrat" | political_affiliation == "Republican"
  ) %>%
  count(political_affiliation, sweetener) %>%
  mutate(sweetener = if_else(is.na(sweetener), "No", "Yes")) %>%
  group_by(political_affiliation, sweetener) %>%
  summarise(sum_n = sum(n), .groups = "drop") %>%
  group_by(political_affiliation) %>%
  mutate(percent = 100 * (sum_n / sum(sum_n))) %>%
  ungroup() %>%
  mutate(
    percent_signed = if_else(sweetener == "Yes", -percent, percent),
    sweetener = factor(sweetener, levels = c("Yes", "No"))
  )

sugar <- ggplot(
  x,
  aes(
    x = percent_signed,
    y = political_affiliation,
    fill = political_affiliation
  )
) +
  geom_col(width = 0.6) +

  geom_vline(xintercept = 0, linetype = "dashed") +

  scale_x_continuous(
    labels = function(x) paste0(abs(x), "%"),
    limits = c(-100, 100)
  ) +

  scale_fill_manual(
    values = c(
      "Democrat" = "#377EB8",
      "Republican" = "#E41A1C"
    )
  ) +

  geom_text(
    aes(label = paste0(round(percent), "%")),
    hjust = ifelse(x$percent_signed < 0, 1.1, -0.1),
    size = 4,
    family = "serif"
  ) +
  annotate("text", x = c(-75, 75), y = c(2.5, 2.5), label = c("NO", "YES")) +
  labs(
    x = "Percentage",
    y = NULL,
    fill = "Political affiliation",
    title = "Do you add swettener to the coffe?"
  ) +

  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold", size = 20)
  )


many_cups <- coffee_survey %>%
  filter(
    political_affiliation == "Democrat" | political_affiliation == "Republican"
  ) %>%
  count(political_affiliation, cups) %>%
  mutate(more_4 = if_else(cups == "More than 4", "Yes", "No")) %>%
  group_by(political_affiliation, more_4) %>%
  summarise(sum_n = sum(n), .groups = "drop") %>%
  group_by(political_affiliation) %>%
  mutate(percent = 100 * (sum_n / sum(sum_n))) %>%
  ungroup() %>%
  mutate(
    percent_signed = if_else(more_4 == "Yes", percent, -percent),
    sweetener = factor(more_4, levels = c("Yes", "No"))
  )


cups <- ggplot(
  many_cups,
  aes(
    x = percent_signed,
    y = political_affiliation,
    fill = political_affiliation
  )
) +
  geom_col(width = 0.6) +

  geom_vline(xintercept = 0, linetype = "dashed") +

  scale_x_continuous(
    labels = function(x) paste0(abs(x), "%"),
    limits = c(-100, 100)
  ) +

  scale_fill_manual(
    values = c(
      "Democrat" = "#377EB8",
      "Republican" = "#E41A1C"
    )
  ) +

  geom_text(
    aes(label = paste0(round(percent), "%")),
    hjust = ifelse(many_cups$percent_signed < 0, 1.1, -0.1),
    size = 4,
    family = "serif"
  ) +
  annotate("text", x = c(-75, 75), y = c(2.5, 2.5), label = c("NO", "YES")) +
  labs(
    x = "Percentage",
    y = NULL,
    fill = "Political affiliation",
    title = "Do you drink > than 4 cups?"
  ) +

  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold", size = 20)
  )


sugar +
  cups +
  plot_layout(
    ncol = 1,
    guides = "collect",
    axis_titles = "collect",
    axes = "collect"
  ) +
  plot_annotation(
    title = "Political Coffee",
    theme = theme(
      plot.title = element_text(size = 40, family = "serif", face = "bold"),
    )
  ) &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    legend.key.size = unit(0.1, "in")
  )

ggsave("plots/2024/political_coffe.png", width = 3, height = 3)
