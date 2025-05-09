# Filter for Norway and Portugal only
ESS11_NO_PT <- ESS11_clean %>%
  filter(cntry %in% c("NO", "PT"))

# Barplot comparing responsibility levels between NO and PT

ggplot(ESS11_NO_PT, aes(x = clim_change_resp, fill = cntry)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("NO" = "steelblue", "PT" = "tomato")) +
  labs(
    title = "Perceived responsibility for climate change: Norway vs Portugal",
    x = "Perceived responsibility",
    y = "Count",
    fill = "Country"
  ) +
  theme_minimal()

ggsave("plots/responsibility_NO_vs_PT.png", width = 8, height = 6, dpi = 300)
# This bar plot compares the perceived responsibility for climate change between Norway and Portugal.
# It shows that individuals in Norway are more likely to report high levels of personal responsibility,
# while those in Portugal tend to report lower or medium levels.

# Linear regression models by country
model_NO <- lm(as.numeric(clim_change_resp) ~ edulvlb_clean, data = ESS11_NO_PT %>% filter(cntry == "NO"))
model_PT <- lm(as.numeric(clim_change_resp) ~ edulvlb_clean, data = ESS11_NO_PT %>% filter(cntry == "PT"))

summary(model_NO)
summary(model_PT)


#Do More Educated Individuals Feel More Responsible for Climate Change?

ggplot(ESS11_NO_PT, aes(x = edulvlb_clean, y = as.numeric(clim_change_resp), color = cntry)) +
  geom_smooth(method = "lm", se = TRUE, size = 1.3) +
  scale_color_manual(values = c("NO" = "steelblue", "PT" = "tomato")) +
  labs(
    title = "Relationship between education and climate responsibility",
    subtitle = "Comparison between Norway and Portugal",
    x = "Education level (1 = low, 7 = high)",
    y = "Responsibility (1 = Low, 3 = High)",
    color = "Country"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("plots/lm_only_NO_PT.png", width = 8, height = 6, dpi = 300)
