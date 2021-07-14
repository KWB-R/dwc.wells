# get numerical variables
is_numeric <- unlist(lapply(df, is.numeric))
num_vars <- names(df)[is_numeric]
num_input_vars <- num_vars[num_vars %in% model_features]
df_num <- df %>% dplyr::select("Qs_rel", num_input_vars)

# create categorical variable from Qs_rel
df$Qs_rel_cat <- cut(df$Qs_rel,
                     breaks = c(-Inf, 1/3, 2/3, Inf),
                     labels=c("1-low","2-middle","3-high"))
frequency_table(df$Qs_rel_cat)

# get categorical variables
cat_vars <- names(df)[!(names(df) %in% num_vars)]
cat_input_vars <- cat_vars[cat_vars %in% model_features]
df_cat <- df %>% dplyr::select("Qs_rel_cat", cat_input_vars)


# Pearson correlation ----------------------------------------------------------

pearson <- cor(df_features_num, use = "pairwise.complete.obs", method = "pearson")


# my.corrplot.mixed ------------------------------------------------------------
my.corrplot.mixed <- function(data, cl.lim = c(-1,1), number.cex = 1.0) {
  corrplot::corrplot.mixed(data, lower = "number", upper = "color", outline = TRUE,
                 tl.cex = 1.0, tl.pos = "lt", tl.col = "black", diag = "u",
                 cl.cex = 1.0, cl.ratio = 0.2, number.cex = number.cex, cl.lim = cl.lim)
}


# my.corrplot ------------------------------------------------------------------
my.corrplot <- function(data, cl.lim = c(-1,1)) {
  corrplot::corrplot(data, method = "color", outline = TRUE, tl.cex = 1.1, cl.lim = cl.lim,
           tl.col = "black", cl.cex = 1.0, cl.ratio = 0.2)
}

my.corrplot(pearson)


# simple plot: upper and lower half squares
pdf(file.path("numerics_cor_pearson_v1b.pdf"), width = 10, height = 10)
my.corrplot.mixed(pearson, number.cex = 0.5)
dev.off()


# other plot: upper half squares, lower half values
pdf(file.path("numerics_cor_pearson_v2b.pdf"), width = 10, height = 10)
my.corrplot(pearson)
dev.off()

# Chi-square test for categorical variables ------------------------------------


library(kwb.utils)

# chi2.CramersV.test --------------------------------------------------------
chi2.CramersV.test <- function(data, counts.factor = 1) {

  # 1) set up matrix template
  p.matrix <- matrix(nrow = length(colnames(data)),
                     ncol = length(colnames(data)))
  rownames(p.matrix) <- colnames(data)
  colnames(p.matrix) <- rownames(p.matrix)

  chi2.matrix <- p.matrix

  cramersV.matrix <- p.matrix

  # 2) start plot
  pdff <- preparePdfIf(to.pdf = TRUE)


  # 3) conduct chi test and calculate p for each pair of variables i and j
  for (i in colnames(data)) {
    for (j in colnames(data)) {
      data[,i] <- as.character(data[,i])
      data[,j] <- as.character(data[,j])
      cat(sprintf("\"%s\" \"%s\"\n", i, j)) # compared variables
      tbl <- table(data[,i], data[,j])*counts.factor # create contingency table (e.g. factor 0.01)
      chi2.matrix[i,j] <- chisq.test(tbl)$statistic # calculate p-values
      p.matrix[i,j] <- chisq.test(tbl)$p.value # calculate p-values
      cramersV.matrix[i,j] <- lsr::cramersV(tbl) # calculates Cramér's V or phi as measure for effect size

      # 4) plot distribution

      # 4a) create contingency matrix with each columns summing up to 100
      tbl_rel_col <- 100*prop.table(tbl,2)


      n <- nrow(tbl_rel_col)

      # 4b) define colors
      if (n > 8) {
        cols <- c(RColorBrewer::brewer.pal(n, "Paired"),
                  RColorBrewer::brewer.pal(n, "Set2"))
      } else {
        cols <- RColorBrewer::brewer.pal(n, "Blues")
      }


      # 4c) plot legend and start new page if new variable
      if (j == colnames(data)[1]) {
        par(mfrow=c(3,4), mar = c(6.1, 4.1, 4.1, 4.1))
        plot.new()
        title(i)
        if (n > 8) {
          legend("top", legend = rownames(tbl_rel_col), fill = cols, bty = "n", ncol = 2, cex = 0.9)
        } else {
          legend("top", legend = rownames(tbl_rel_col), fill = cols, bty = "n")
        }
      }

      # 4d. plot distribution for each pair of variables
      if(!i == j) {
        barplot(tbl_rel_col, las = 1, ylab = "share [%]", col = cols, las = 2)
        title(paste(j))
        #abline(h = 88.907569, lty = 2)
      }
    }
  }
  # 4e) finish plot
  finishAndShowPdfIf(to.pdf=TRUE, pdff)

  # 5) return matrixes
  return(list(chi2.matrix, p.matrix, cramersV.matrix))
}


chi.stats <- chi2.CramersV.test(df_cat)
chi2.values <- chi.stats[[1]] # chi2-values
p.values <- chi.stats[[2]] # p-values (dependent of sample size)
cramers.V <- chi.stats[[3]] # measure of effect size: "Cramér's V" (independent of sample size)
save_data(cramers.V, path = paths$stats_data, "CramersV")
save_data(pearson, path = paths$stats_data, "pearson")


# other plot: upper half squares, lower half values
pdf(file.path("categorical_data_cor_CramersV.pdf"), width = 6, height = 6)
my.corrplot(cramers.V)
dev.off()


# Boruta algorithm
library(Boruta)
boruta <- Boruta(Qs_rel ~ ., data = df_num, doTrace = 2, maxRuns = 500)
print(boruta)
png(file.path("boruta_variable_importance.png"), width = 15, height = 12, res = 300, units = "cm")
plot(boruta, las = 2, cex.axis = 0.7, xlab = "", whichShadow = c(F, F, F), ylim = c(0, 80))
dev.off()

# Random Forest test
rf60 <- randomForest::randomForest(Qs_rel~., data = df_num)
randomForestExplainer::important_variables(rf60)
randomForestExplainer::plot_min_depth_distribution(rf60)
ggplot2::ggsave("rf_minimal_depth.png", width = 7, height = 4, dpi = 600)
