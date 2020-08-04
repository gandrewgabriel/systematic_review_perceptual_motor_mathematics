library(stats) #v4.0.2
library(ggplot2) #v3.3.2

#
# Type II Error Probabilites (and power) for included studies
#

#' 
#' Calculate the statistical power (= 1 - type II error probability) for a 
#' test of equality between two groups, where the true effect size is known,
#' and the test uses the chosen significance level.
#' 
#' @param n_1 The sample size of the first group in the study
#' @param n_2 The sample size of the second group in the study
#' @param min_d The true effect size (Cohen's d) 
#' @param alpha_two_tail The alpha-level for the two-tailed test of equality between the groups
find_power <- function(n_1, n_2, min_d, alpha_two_tail=0.05)
{
  df <- n_1 + n_2 - 2
  t_crit <- qt(1-(alpha_two_tail/2), df)
  non_centrality <- min_d / sqrt(1/n_1+1/n_2)
  return((1 - pt(t_crit, df, non_centrality)) + pt(-t_crit, df, non_centrality))
}


#
# Calculate and plot the type-II error probabilities for each study as a function of effect size
#
study_a <- "Asakawa et al (2019)"
study_g <- "Gracia-Bafalluy & Noël (2008)"
study_e <- "Erasmus et al (2016)"
study_k <- "Katsipataki (2013)"

d <- seq(0, 1.5, by=0.01)

power_data <- data.frame(d = d, 
                         study = c(rep(study_a, length(d)),
                                   rep(study_g, length(d)),
                                   rep(study_e, length(d)),
                                   rep(study_k, length(d))),
                         power = c(sapply(d, find_power, n_1 = 37, n_2 = 42),
                                   sapply(d, find_power, n_1 = 17, n_2 = 16),
                                   sapply(d, find_power, n_1 = 27, n_2 = 21),
                                   sapply(d, find_power, n_1 = 29, n_2 = 27)))

ggplot(data=power_data, aes(x=d, y=1-power, group=study)) +
  geom_line(aes(linetype=study, color=study)) +
  labs(x="Effect Size", y = "Type II Error Probability") +
  scale_x_continuous(limits=c(0, 1.5), expand = c(0,0)) +
  scale_y_continuous(breaks=c(seq(0, 1, by=0.1), 0.95), limits = c(0,1), expand = c(0,0)) +
  scale_color_grey() + theme_classic() + theme(legend.title=element_blank(), axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

