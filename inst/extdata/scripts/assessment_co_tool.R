path <- file.path("//<servername>/projekte$/SUW_Department/Projects",
                    "DWC/Data-Work-packages/WP2_Performance/T232_WellPlanning")
#filename <- "Modellguete_CO_HSc.csv"
filename <- "20211119_Modellguete_CO-Daten_HSc.csv"
infile <- file.path(path, filename)

df <- read.csv(infile, dec = ",", sep = ";")
df <- df[,1:3]
names(df) <- c("well_id", "Qs_rel", ".pred")
str(df)
library(dwc.wells)
library(tidymodels)
dwc.wells::scatterplot(df)
ggsave("scatterplot_co_tool_v2.png", dpi = 600, width = 3.5, height = 3)
getwd()


# classification performance ---------------------------------------------------

# classify Qs data
df_pred <- df %>%
  mutate(Qs_rel_class = classify_Qs(Qs_rel),
         .pred_class = classify_Qs(.pred))

# confusion matrix
matrix <- conf_mat(df_pred, truth = Qs_rel_class, estimate = .pred_class)

# performance metrics
metrics <- matrix %>% summary()

save_data(matrix, getwd(), "co_tool_numeric_to_class_matrix_split80", formats = "RData")
save_data(metrics, getwd(), "co_tool_numeric_to_class_metrics_split80")
