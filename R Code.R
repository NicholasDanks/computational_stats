# Lesson 1 ----

?cor
vignette("extending_badgecreatr", package = "badgecreatr")

# Lesson 2 ----

numbers <- c(12, 14, 14, 25, 33, 35, 38, 38, 41, 43, 45, 50, 58, 59)
numbers[5]
sum(numbers) 
seq(3,7)  

customers <- read.table(file = "Files/customer_ages.txt", header = TRUE)
customers$age
nrow(customers)  

help(c)

ages <- customers$age
ages
sum(ages) / length(ages)  
mean(ages)
sample_ages <- ages[c(2, 15, 28, 385)]
random_ages <- sample(ages, 4)
mean(random_ages)

hist(ages)
plot(density(ages))

sort(ages)
boxplot(ages, horizontal = TRUE)  
stripchart(ages, method = "stack", add = TRUE)
summary(ages)



exam <- read.table("Files/exam_results.txt", header = TRUE)  
scores <- exam$scores
hist(scores)
hist(scores, breaks = 50, col = "lightgray")
?hist
hist(scores, breaks = seq(0, 100, 10))

h <- hist(scores)
h$counts
h$breaks

plot( density(scores), main = "Exam Score Distribution")

hist(scores, breaks=50, prob=TRUE)  
lines(density(scores, adjust=0.5), col="blue", lwd=2)  
lines(density(scores, adjust=1), col="blue")
lines(density(scores, adjust=2), col="blue", lty="dashed")

## Visualizing

#install.packages("manipulate")
library(manipulate)

# Plot a distribution
plotdist <- function(xseq, xdens, col, xlim, type, lty, lwd, segments=NULL, qlty, qcol, polyfill=NULL) {
  if (type == "plot") {
    plot(xseq, xdens, type="l", lwd=0, col=col, frame=FALSE, xlim=xlim, lty=lty, ylab='', xlab='')
  }
  
  if (!is.null(polyfill)) {
    polygon(polyfill[,1], polyfill[,2], col=qcol, border=qcol)
  }
  
  # draw quantile lines
  if (!is.null(segments)) {
    segments(x0=segments[,1], x1=segments[,1], y0=0, y1=segments[,2], lwd=lwd, col=qcol, lty=qlty)
  }
  
  lines(xseq, xdens, type="l", lwd=lwd, col=col, lty=lty)
}

# Plot the t distribution
plott <- function(lwd=2, ncp=0, df=300, col=rgb(0.30,0.50,0.75), xlim=c(-3,3), type="plot", lty="solid", quants=NULL, qlty="solid", qcol=rgb(0.30,0.50,0.75, 0.5), fill_quants=NULL) {
  xseq = seq(ncp-6, ncp+6, length=1000)
  xdens = dt(xseq, ncp=ncp, df=df)
  if (length(xlim) == 0) {
    xlim = c(ncp-3.5, ncp+3.5)
  }
  
  segments <- NULL
  polyfill <- NULL
  
  if (!is.null(quants)) {
    xquants = qt(quants, ncp=ncp, df=df)
    dquants = dt(xquants, ncp=ncp, df=df)
    segments = cbind(xquants, dquants)
  }
  
  if(!is.null(fill_quants)) {
    polyq = qt(fill_quants, ncp=ncp, df=df)
    polyfill.x = seq(polyq[1], polyq[2], by=0.001)
    polyfill.y = dt(polyfill.x, ncp=ncp, df=df)
    polyfill.x = c(polyfill.x[1], polyfill.x, tail(polyfill.x,1))
    polyfill.y = c(0, polyfill.y, 0)
    polyfill <- cbind(polyfill.x, polyfill.y)
  }
  
  plotdist(xseq, xdens, col, xlim, type, lty, lwd, segments, qlty, qcol, polyfill)
}

t_null_plot <- function(df, alpha) {
  plott(df=df, col=rgb(0.75, 0.1, 0.1), qcol=rgb(1, 0.5, 0.5), xlim=c(-6, 6), fill_quants=c(1-alpha, 0.999))
}

t_alt_lines <- function(df, ncp=0, alpha) {
  blue <- rgb(0.1, 0.1, 0.75)
  lightblue <- rgb(0.4, 0.4, 1, 0.3)
  quants <- c(0.5)
  power_quant <- pt(qt(1-alpha, df=df), df=df, ncp=ncp)
  plott(df=df, ncp=ncp, type='lines', lty="dashed", col=blue, quants=quants, qcol=lightblue, xlim=c(-6, 6), fill_quants=c(power_quant, 0.999))
}

t_test_plot <- function(diff, sd, n, alpha) {
  df=n-1
  t = diff/(sd/sqrt(n))
  t_null_plot(df, alpha)
  t_alt_lines(df,t, alpha)
}

manipulate(
  t_test_plot(diff, sd, n, alpha),
  diff  = slider(0, 4, step = 0.1, initial = 0.5),
  sd    = slider(1, 5, step = 0.1, initial = 4),
  n     = slider(2, 500, step = 1, initial = 100),
  alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
)

# Lesson 3 ----

runif(n=10)
uniform_dice <- round(runif(n=100000, min=0.5, max=6.5))

hist(uniform_dice)
sample_dice <- sample(1:6, size=100000, replace=TRUE)  
hist(sample_dice)

rbinom(1, 5, 0.5)
rbinom(10, 5, 0.5)

hist(rbinom(1000, 1, 0.5), col="lightgray")
hist(rbinom(1000, 2, 0.5), col="lightgray")
hist(rbinom(1000, 10, 0.5), col="lightgray")
hist(rbinom(1000, 500, 0.5), col="lightgray")

normal_population <-	rnorm(100000, mean=1.73, sd=0.15)

hist(normal_population, xlim=c(0,2.5), col="lightblue", probability=TRUE)  
lines(density(normal_population), col = "blue", lwd=3)

a <- rnorm(2000, mean=5, sd=7)
b <- rnorm(2000, mean=20, sd=7) 
c <- rnorm(2500, mean=35, sd=5)  
d <- rnorm(3000, mean=50, sd=8)  
e <- rnorm(1000, mean=70, sd=5)  
f <- rnorm(1000, mean=80, sd=7)

taiwan_ages <- c(a,b,c,d,e,f)

hist(taiwan_ages, xlim=c(0,100), breaks=50, col="lightblue", probability=TRUE)  
lines(density(taiwan_ages), col = "blue", lwd=3)

customers <- read.table("Files/customer_ages.txt", header = TRUE)  
ages <- customers$age

hist(ages, probability = TRUE, main = "Customer Ages")  
lines(density(ages), col="blue", lwd=2)

sum(ages) / length(ages)  
mean(ages)          
median(ages)  

population_movie_ratings <- round(rnorm(mean=50, 9, n=10000000))  
summary(population_movie_ratings)

sample_movie_ratings <- sample(population_movie_ratings, size=450)
summary(sample_movie_ratings)

plot(density(population_movie_ratings, 
             adjust=2),
             col="blue", 
             lwd=2,  
             main="population versus sample")

lines(density(sample_movie_ratings), 
      lty="dashed", 
      lwd=1)

boxplot(population_movie_ratings,  
        sample_movie_ratings,  
        horizontal=TRUE)

# Lesson 4 ----
positive <- function(num) {  
  if (num < 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

positive(-23)
positive(74)

norm_data <- rnorm(500000)

results <- c()

for (i in 1:length(norm_data)) {  if (norm_data[i] < 0) {
  results[i] <- FALSE
} else {
  results[i] <- TRUE
}
}

result <- sapply(norm_data, positive)

result <- norm_data > 0

ifelse(norm_data > 0, "positive", "negative")

system.time( norm_data > 0 )

plot_centrality <- function(distr, title) {
  # Plot the full distribution of abc
  plot(density(distr), col="blue", lwd=2, main = title)
  # Add vertical lines showing mean and median  
  abline(v=mean(distr))  
  abline(v=median(distr), lty="dashed")
}

d1 <- rnorm(n=500, mean=15, sd=5)  
d2 <- rnorm(n=200, mean=30, sd=5)  
d3 <- rnorm(n=100, mean=45, sd=5)  
d123 <- c(d1, d2, d3)

plot_centrality(distr = d123, title = "Right Skewed")

d123 <- rnorm(n=800)  
plot_centrality(d123, title="Normal")

d1 <- rnorm(n=100, mean=5, sd=5) 
d2 <- rnorm(n=200, mean=20, sd=5) 
d3 <- rnorm(n=500, mean=35, sd=5)  
d123 <- c(d1, d2, d3)
plot_centrality(d123,
                  title="Left (Negatively) Skewed")

quartiles_vs_sd <- function(distr) {
  # plot data distribution, mean + standard deviations lines  
  plot(density(distr))
  abline(v=mean(distr))
  sd_points = mean(distr) + c(-3, -2, -1, 0, 1, 2, 3)*sd(distr)
  abline(v=sd_points, lty='dashed')
  # return the distance of each quartile from the mean  
  q = quantile(distr, c(0.25, 0.50, 0.75))
  return((q - mean(distr))/sd(distr))
}

data = rnorm(n=10000, mean=0, sd=1)  
quartiles_vs_sd(data)

data = rnorm(n=10000, mean=35, sd=3.5)  
quartiles_vs_sd(data)

mean(data) + c(-1, 1)*sd(data)  
mean(data) + c(-1.96, 1.96)*sd(data)  
quartiles_vs_sd(d123)  


a <- rnorm(n=100000, mean=150, sd=15)  
b <- rnorm(n=200000, mean=190, sd=25)  
c <- rnorm(n=500000, mean=255, sd=25)  
d <- rnorm(n=200000, mean=310, sd=20)  
pop <- c(a,b,c,d)
pop_mean <- mean(pop)

plot(density(pop), col="blue", lty="dashed",  main="population data")
abline(v=pop_mean, lty="dashed")  
pop_mean

sample_size = 300
sample0 = sample(pop, sample_size)  
sample0_mean = mean(sample0)

lines(density(sample0), col="blue", lwd=2)  
abline(v=sample0_mean, lwd=2)
sample0_mean

resamples <- replicate(3000,
                       sample(sample0, length(sample0), replace=TRUE))

plot(density(pop), 
     lwd=0, 
     ylim=c(0, 0.009),  
     main="population vs. bootstrapped samples")

plot_resample_density <- function(sample_i) {  
  lines(density(sample_i), col=rgb(0.0, 0.4, 0.0, 0.01))  
  return(mean(sample_i))
}

sample_means <- apply(resamples, 2, FUN=plot_resample_density)

# Plot hidden population and original sample distributions
lines(density(sample0), lwd=3)
lines(density(pop), lwd=2, lty="dashed")

# Plot population and original sample densities  
plot(density(pop), col="blue", lty="dashed")  
lines(density(sample0), col="blue", lwd=2)

# Draws lines for each sampling mean  
plot_resample_mean <- function(sample_i) {
abline(v=mean(sample_i), col=rgb(0.0, 0.4, 0.0, 0.01))
}

apply(resamples, 2, FUN=plot_resample_mean)

# Draw lines of population and original sample mean  
abline(v=mean(sample_means), lwd=2)  
abline(v=pop_mean, lty="dashed")

## Distribution of sampling
plot(density(sample_means), lwd=2, xlim=c(0, 400))

## Confidence intervals of the sampling means
quantile(sample_means,  probs=c(0.025, 0.975))
#	2.5%	97.5%
# 233.7158 245.9080

quantile(sample_means, probs=c(0.005, 0.995))
#	0.5%	99.5%
# 231.8270 247.8224

# Lesson 5 ----

pnorm(-1.13)  
1 - pnorm(0.80)


customer_ages <- as.vector(read.csv(file = "Files/customer_ages.txt")$age)
plot(density(customer_ages),
     col = "blue",
     main = "Customer Age Data Density Plot")

head(customer_ages)

customer_ages_mc <- (customer_ages - mean(customer_ages))

head(customer_ages_mc)

customer_ages_std <- (customer_ages - mean(customer_ages))/sd(customer_ages)

head(customer_ages_std)

plot(density(customer_ages_std),
     col = "blue",
     main = "Standardized Customer Age Data Density Plot")

set.seed(10)
plot_resample <- function(sample0) {
  resample <- sample(sample0, length(sample0), replace=TRUE)  
  lines(density(resample), col=rgb(0.5,0.5,1, 0.1))  
  resample_stat <- mean(resample)
  abline(v=mean(resample), col=rgb(0.5, 1, 1, 0.1))  
  return(resample_stat)
}

show_resample_width <- function(sample0, title) {  
  num_bootstraps = 1000
  plot(density(sample0), lwd=0, title, ylab="", frame.plot=FALSE, yaxt="n", ylim = c(0,0.05))
  sample_means <- replicate(num_bootstraps, plot_resample(sample0))  
  lines(density(sample0), lwd=1, col="black")
}

mean = mean(customer_ages)
show_resample_width(sample(customer_ages, 50), 
                    "Small Sample Resampling")
         
show_resample_width(customer_ages, 
                    "Large Sample Resampling")                        

n = length(customer_ages)  
num_boot <- 2000
sample_statistic <- function(stat_function, sample0) {  
  resample <- sample(sample0, length(sample0), replace=TRUE)  
  stat_function(resample)
}
rm(mean)
sample_means <- replicate(num_boot,
                          sample_statistic(mean, customer_ages))  
plot(density(sample_means), lwd=2, main="sample means")  
quantile(sample_means, probs = c(0.025, 0.975))
#     2.5%    97.5% 
# 45.24054 48.43634 

sample_medians <- replicate(num_boot,
                            sample_statistic(median, customer_ages))  
plot(density(sample_medians), lwd=2, main="sample medians")  
quantile(sample_medians, probs = c(0.025, 0.975))
#  2.5% 97.5% 
#    45    48 

boot_combinations <- function(n) choose(2*n - 1, n)
boot_combinations(10)

# Hypothesis Testing with t
hyp_mean <- 410
sample_size <- 180
sample_mean <- 508.70
sample_sd <- 178.35
se <- (sample_sd /sqrt(sample_size))  
t <- (sample_mean - hyp_mean) / se  
df = 179
1 - pt(t, df)  

# Hypothesis Testing with Bootstrap
set.seed(50)
pop <- rnorm(100000, mean=511, sd=183)
# Manager's hypothesis: μ=140
manager_hyp <- 410
n = 180
auditor_sample <- sample(pop, n)
mean(auditor_sample)
# [1] 507.4672
sd(auditor_sample)
# [1] 177.8446
mean(auditor_sample) - manager_hyp

boot_mean_diffs <- function(sample0, mean_hyp) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  return( mean(resample) - mean_hyp )
}
num_boots <- 2000
mean_diffs <- replicate(num_boots,
                        boot_mean_diffs(auditor_sample, manager_hyp))
sd(mean_diffs)
#  [1] 13.34826

diff_ci_95 <- quantile(mean_diffs, probs=c(0.025, 0.975))


boot_t_stat <- function(sample0, mean_hyp) {
  resample <- sample(sample0, length(sample0), replace=TRUE)  
  diff <- mean(resample) - mean_hyp
  se <- sd(resample)/sqrt(length(resample))  
  return( diff / se )
}

num_boots <- 2000
t_boots <- replicate(num_boots, boot_t_stat(auditor_sample, manager_hyp))
mean(t_boots)
plot(density(t_boots), xlim=c(0,12), col="blue", lwd=2)
diff_ci_95 <- quantile(t_boots, probs=c(0.025, 0.975))

abline(v=mean(t_boots))  
abline(v=diff_ci_95, lty="dashed")

# Lesson 6 ----

interactive_regression <- function() {
  cat("Click on the plot to create data points; hit [esc] to stop")
  plot(NA, xlim=c(-5,50), ylim=c(-5,50))
  points = data.frame()
  repeat {
    click_loc <- locator(1)
    if (is.null(click_loc)) break
    if(nrow(points) == 0 ) {
      points <- data.frame(x=click_loc$x, y=click_loc$y)
    } else {
      points <- rbind(points, c(click_loc$x, click_loc$y))
    }
    plot(points, xlim=c(-5,50), ylim=c(-5,50), pch=19, cex=2, col="gray")
    if (nrow(points) < 2) next
    model <- lm(points$y ~ points$x)
    abline(model, lwd=2, col="cornflowerblue")
    text(1, 50, paste(c("Raw intercept: ", round(model$coefficients[1], 2)), collapse=" "))
    text(1, 45, paste(c("Raw slope : ", round(model$coefficients[2], 2)), collapse=" "))
    text(1, 40, paste(c("Correlation : ", round(cor(points$x, points$y), 2)), collapse=" "))
  }
  return(points)
}

system.time( read.csv("Files/piccollage_accounts_bundles.csv", header=TRUE) )

library(data.table)
system.time( fread("Files/piccollage_accounts_bundles.csv") )  


ac_bundles_dt <- fread("Files/piccollage_accounts_bundles.csv") 
ac_bundles_matrix <- as.matrix(ac_bundles_dt[, -1])

library(lsa)
system.time( cosine(ac_bundles_matrix) )  

library(qlcMatrix)
system.time( qlcMatrix::cosSparse(ac_bundles_matrix) )  

b1 <- c(ac1 = 1, ac2 = 4, ac3 = 2)
b2 <- c(ac1 = 2, ac2 = 2, ac3 = 1)
b3 <- c(ac1 = 4, ac2 = 4, ac3 = 2)
b4 <- c(ac1 = 3, ac2 = 0, ac3 = 0)
ac_bundles <- cbind(b1, b2, b3, b4)

ac_bundles
t(ac_bundles)

ac_bundles[1, ]
ac_bundles[, 1]
plot(t(ac_bundles), xlim=c(0, 5), ylim=c(0, 5))
arrows(0, 0, ac_bundles[1, ], ac_bundles[2, ])
text(ac_bundles[1, ], ac_bundles[2, ]+0.3, colnames(ac_bundles))

terms <- "mom|mother|dad|father|baba|mama"  
bundle_names <- colnames(ac_bundles_matrix)
grep(terms, bundle_names, ignore.case=TRUE, value=TRUE)

cosine_recos <- function(items_matrix) {
  cos_sim_matrix <- qlcMatrix::cosSparse(items_matrix)
  diag(cos_sim_matrix) <- 2
  row_recos<- function(cos_sim_row) {
    names(sort(cos_sim_row, decreasing = TRUE))
  }
  all_recos <- t(apply(cos_sim_matrix, 2, row_recos))
  final_recos <- all_recos[, -1]
  return(final_recos[, 1:5])
}

recos_cos <- cosine_recos(ac_bundles_matrix)
recos_cos["sweetmothersday", ]

bundle_means <- apply(ac_bundles_matrix, 2, mean)
bundle_means_matrix <- t(replicate(nrow(ac_bundles_matrix), bundle_means))
ac_bundles_mc_b <- ac_bundles_matrix - bundle_means_matrix
recos_cor <- cosine_recos(ac_bundles_mc_b)
recos_cor["sweetmothersday", ]

account_means <- apply(ac_bundles_matrix, 1, mean)
account_means_matrix <- replicate(ncol(ac_bundles_matrix), account_means)
ac_bundles_mc_ac <- ac_bundles_matrix - account_means_matrix
recos_adj_cos <- cosine_recos(ac_bundles_mc_ac)
recos_adj_cos["sweetmothersday", ]

# Lesson 7 ----

# Load package
library(seminr)

# Creating measurement mode
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)

# Creating structural model
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

# slice the data
train_index <- sample(1:nrow(mobi), 200)
test_index <- setdiff(1:nrow(mobi),train_index)

# Estimating the model
mobi_pls_train <- estimate_pls(data = mobi[train_index,],
                               measurement_model = mobi_mm,
                               structural_model = mobi_sm)

# Collect the vectors and matrices necessary for predictions!!
# (Are you as excited as me?)
standard_deviations_vector <- mobi_pls_train$sdData
means_vector <- mobi_pls_train$meanData
validation_data <- mobi[test_index, names(standard_deviations_vector)]

inner_path_coef <- mobi_pls_train$path_coef
outer_weights <- mobi_pls_train$outer_weights
outer_loadings <- mobi_pls_train$outer_loadings

# Standardize the data (using the training descriptives)
standardized_validation_data <- t(t(sweep(validation_data,2,means_vector)) / standard_deviations_vector)

# Predict antecedents
constructs_predict_from_items <- as.matrix(standardized_validation_data) %*% outer_weights

# Predict endogenous from exogenous
endogenous_predict_from_exogenous <- constructs_predict_from_items %*% inner_path_coef

# Predict endogenous items from endogenous construct predictions
items_from_constructs <- endogenous_predict_from_exogenous %*% t(outer_loadings)

# Unstandardize the predictions
unstandardized_predictions <- t((t(items_from_constructs) * standard_deviations_vector) + means_vector)


# Subset for the Endogenous Items we are interested in
satisfaction_predictions <- unstandardized_predictions[,c("CUSA1","CUSA2","CUSA3")]

error <- mobi[test_index,c("CUSA1","CUSA2","CUSA3")] - satisfaction_predictions[,c("CUSA1","CUSA2","CUSA3")]

# Some predictive metrics
CUSA1_RMSE <- sqrt(mean((error[,"CUSA1"])^2))
CUSA2_RMSE <- sqrt(mean((error[,"CUSA2"])^2))
CUSA3_RMSE <- sqrt(mean((error[,"CUSA3"])^2))

# Lesson 8 ----

set.seed(42)

my_x_data <- data.frame(x1 = rnorm(100), x2 = rnorm(100))

y1 = rnorm(100)
lm(y1 ~ x1 + x2, data=my_x_data)

y2 = rnorm(100)
lm(y2 ~ x1 + x2, data=my_x_data)

y_regr <- function(x_data) {  
  invisible(
    function(y) {
      lm(y ~ x1 + x2, data = x_data)
    }
  )
}

set.seed(42)
my_x_data <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
lm_with_x <- y_regr(my_x_data)

y1 = rnorm(100)  
lm_with_x(y1)

y2 = rnorm(100)
lm_with_x(y2)


