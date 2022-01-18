library(dwc.wells)


df <- model_data %>% select(well_age_years, n_rehab, time_since_rehab_years, volume_m3_d.cv)
df <- model_data %>% select(well_age_years, Qs_rel)

ggplot(df, aes(x = well_age_years, y = Qs_rel)) +
  geom_point(alpha = 0.3)


kclust <- kmeans(df, centers = 3)
kclust

summary(kclust)

augment(kclust, df)

tidy(kclust)

glance(kclust)

library(dplyr)

kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(df, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, df)
  )


kclusts

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

p1 <- 
  ggplot(assignments, aes(x = well_age_years, y = Qs_rel)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  geom_point(data = clusters, size = 5, shape = "x") +
  facet_wrap(~ k)
p1



ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()
