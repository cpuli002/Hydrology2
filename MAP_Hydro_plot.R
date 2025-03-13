MAP_WaterDepth2 <- MAP_WaterDepth %>%
  mutate(YearMonth = floor_date(Date, "month"))


MAP_WaterDepth2_monthly <- MAP_WaterDepth2 %>%
  group_by(YearMonth) %>%
  summarise(WD_mean = mean(WD, na.rm = TRUE),
            WD_sd = sd(WD, na.rm = TRUE))

df_monthly <- MAP_WaterDepth2_monthly %>%
  mutate(TimeIndex = as.numeric(YearMonth - min(YearMonth)))

quad_model <- lm(WD_mean ~ poly(TimeIndex, 2, raw = TRUE), data = df_monthly)


r2_value <- summary(quad_model)$r.squared

coefs <- coef(quad_model)
eq <- sprintf("y = %.4f x² + %.4f x + %.4f", coefs[3], coefs[2], coefs[1])

y_min <- min(df_monthly$WD_mean, na.rm = TRUE) - 5  
start_date <- as.Date("2004-01-01")
end_date <- as.Date("2024-09-01")

ggplot(df_monthly, aes(x = YearMonth, y = WD_mean)) +
  geom_point(size = 2.5) +   
  geom_line() +            
  geom_errorbar(aes(ymin = WD_mean - WD_sd, ymax = WD_mean + WD_sd), width = 10) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, linetype = "dashed") +
  annotate("text", x = as.Date("2015-06-01"), y = y_min,
           label = sprintf("r² = %.2f\n%s", r2_value, eq),
           size = 4, hjust = 0) +
  labs(x = "Year", y = "Water Depth (cm)", title = "Mean Monthly Waterdepth in the Eastern Marl Prairies") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
