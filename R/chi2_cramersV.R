#' Title
#'
#' @param data data frame on which to perform Chi-2-test
#'
#' @importFrom graphics barplot legend par plot.new title
#' @importFrom stats chisq.test
#' @export
#'
chi2.CramersV.test <- function(data) {

  # 1) set up matrix template
  p.matrix <- matrix(nrow = length(colnames(data)),
                     ncol = length(colnames(data)))
  rownames(p.matrix) <- colnames(data)
  colnames(p.matrix) <- rownames(p.matrix)

  chi2.matrix <- p.matrix

  cramersV.matrix <- p.matrix

  # 2) start plot
  pdff <- kwb.utils::preparePdfIf(to.pdf = TRUE)


  # 3) conduct chi test and calculate p for each pair of variables i and j
  for (i in colnames(data)) {
    for (j in colnames(data)) {
      data[,i] <- as.character(data[,i])
      data[,j] <- as.character(data[,j])
      cat(sprintf("\"%s\" \"%s\"\n", i, j)) # compared variables
      tbl <- table(data[,i], data[,j]) # create contingency table (e.g. factor 0.01)
      chi2.matrix[i,j] <- chisq.test(tbl)$statistic # calculate p-values
      p.matrix[i,j] <- chisq.test(tbl)$p.value # calculate p-values
      cramersV.matrix[i,j] <- lsr::cramersV(tbl) # calculates Cramer's V or phi as measure for effect size
      #cramersV.matrix[i,j] <- rcompanion::cramerV(tbl, bias.correct = TRUE) # makes almost no difference

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
  kwb.utils::finishAndShowPdfIf(to.pdf=TRUE, pdff)

  # 5) return matrixes
  return(list(chi2.matrix, p.matrix, cramersV.matrix))
}
