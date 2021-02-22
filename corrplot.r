install.packages("readxl")
install.packages("openxlsx")
library("openxlsx")
library("readxl")
library("corrplot")

my_data <- read_excel("C:/Users/yd/Desktop/CCB Project/results/output.xlsx")
my_num_data <- my_data[, sapply(my_data, is.numeric)]
all_parameters_corr<-cor(x = my_num_data[-1], y = my_num_data[1], method='spearman')
consumption_corr<-cor(x = my_num_data[7:36], y = my_num_data[1], method='spearman')


cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(all_parameters_corr)

#corrplot(all_parameters_corr, method = "circle", tl.pos='n', type="lower", order="hclust", col=colorRampPalette(c("red", "white", "blue"))(20)) #without label names
#corrplot(all_parameters_corr, type="lower", order="hclust", tl.pos='n', p.mat = p.mat, sig.level = 0.01)
corrplot(all_parameters_corr, tl.pos='n', type="lower", order="hclust", p.mat = p.mat, sig.level = 0.01, insig = "blank")

#write.xlsx(consumption_corr, "C:/Users/yd/Desktop/CCB Project/results/consumption.xlsx",col.names = TRUE, row.names = TRUE)


