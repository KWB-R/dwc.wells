library(dplyr)
a <- Data %>% group_by(well_gallery) %>%
  summarise(n_wells = length(unique(well_id))) %>%
  arrange(-n_wells)

df <- well_feature_data %>% filter(well_function == "Betriebsbrunnen")

# plot construction year distribution for different well galleries
library(ggplot2)
ggplot(df, aes(x = construction_year)) +
  geom_histogram(aes(y = ..density..), boundary = 1930, binwidth = 10, fill = "grey") +
  geom_density(color = "deepskyblue2") +
  scale_x_continuous(limits = c(1920, 2020), breaks = seq.int(1920, 2020, 20)) +
  facet_wrap(~well_gallery,scales = "free") +
  sema.berlin.utils::my_theme()

ggsave("distribution_construction_year_Betrieb_v2.png", dpi = 600, width = 25, height = 20)

getwd()
a <- data.frame(table(df$construction_year, df$well_gallery))
a$Var1 <- as.integer(as.character(a$Var1))
a(seq.int(min(a$Var1)))

library(ggplot2)
ggplot(df, aes(x = screen_material, y = well_age_years)) +
  geom_boxplot(width = 0.4) +
  labs(x = "", y = "Well age [yrs]") +
  sema.berlin.utils::my_theme()
ggsave("material_vs_well_age.png", dpi = 600, width = 6, height = 3)
