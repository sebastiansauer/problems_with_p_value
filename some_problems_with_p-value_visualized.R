##############################
# Some demonstrations that relying on aggregate statistics can be misleading
# illustrated diagrammatically using ggplot2
# Sebastian Sauer
##############################




library(ggplot2)
library(tidyr)


n <- 50  # sample size
perc_dec <- .80  # percentage of observations with *decreasing* value
perc_inc <- 1 - perc_dec

# make up some data, (standard) normal distributed
set.seed(42)  # for repro
X <- rnorm(n = n, mean = 0, sd = 1)

# most observations are *decreasing* in their value, but some are increasing
X_dec <- rnorm(n = n * perc_dec, mean = -0.5, sd = 1)
X_inc <- rnorm(n = n - (n * perc_dec), mean = +5.5, sd = 1)

# mean(X_dec)
# mean(X_inc)

not_signif <- function(group1, group2, alpha = .05, paired = T){
  t <- t.test(x = group1, y = group2, conf.level = 1 - alpha, paired = paired)
  result <- ""
  if (t$p.value < alpha) {result <- "significant"}
  else {
    result <- "not_significant"
  }
  return(result)
}


group1 <- X  # baseline group

# mixture of sample with higher and of sample with  lower mean
group2 <- c(X_dec, X_inc)

t <- t.test(group1, group2, paired = T)


# dataframe for plotting
df <- data.frame(id = 1:n,
                 type = c(rep("decreasing", n * perc_dec),
                          rep("increasing", (n - (n * perc_dec)))),
                 group1 = group1, group2 = group2)

# ggplot needs melted (long-format) dataframes
df_long <- gather(df, group, value, 3:4)


# Plots ########################################################################

# Plot means with SE
p0 <- ggplot(data = df_long, aes(x = group, y = value)) +
  ylim(c(min(df_long$value), max(df_long$value)))
p_mean_cl <- stat_summary(fun.data = "mean_cl_boot",  color = "#39998A")
p_mean <- stat_summary(fun.y = mean, geom = "point", colour = "red")
p1 <- p0 + p_mean_cl +
  stat_summary(fun.y=mean, colour="black", geom="line", aes(group = 1)) +
  ggtitle(paste("method: ", t$method,
                ", p: ", round(t$p.value, 3),
                ", t: ",round(t$statistic, 3),
                ", df: ", t$parameter,
          sep = ""))
p1
# ggsave(p1, file = "p1.pdf", width = 8, height = 6)


# Plot individual observations
p_point <- geom_point(aes(color = type), alpha = .7)
p2 <- p0 + p_point
p2
# ggsave(p2, file = "p2.pdf", width = 8, height = 6)



# as previous but plus means
p3 <- p0 + p_point + p_mean_cl + ggtitle(paste("method: ", t$method,
                                             ", p: ", round(t$p.value, 3),
                                             ", t: ",round(t$statistic, 3),
                                             ", df: ", t$parameter,
                                             sep = ""))
p3
# ggsave(p3, file = "p3.pdf", width = 8, height = 6)


# spagetthi plot of change from group 1 to group 2
p4 <- p0 +  p_point +
  geom_line(aes(group = id, color = type)) +
  p_mean_cl +
  ggtitle(paste("Percentage of observations with *decreasing* value: ",perc_dec))
p4
# ggsave(p4, file = "p4.pdf", width = 8, height = 6)



