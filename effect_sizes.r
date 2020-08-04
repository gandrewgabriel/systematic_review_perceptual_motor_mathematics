library(psych) #v2.0.7
library(forestplot) #v1.9

#' Calculate the unstandardised effect size measure for a single study.
#' 
#' @param ctrl_pre pre-test score for group 1 (control group)
#' @param exp_pre pre-test score for group 2 (experimental/intervention group)
#' @param ctrl_post post-test score for group 1 (control group)
#' @param exp_post post-test score for group 2 (experimental/intervention group)
#' @return The difference between the within group increases in mean score from pre-test to post-test (experimental group minus control group)
unstandardised_effect_size <- function(ctrl_pre, exp_pre, ctrl_post, exp_post)
{
  return((exp_post - exp_pre) - (ctrl_post - ctrl_pre))
}

#' Calculate the standardised effect size measure (a form of Cohen's d) for a single study
#'
#' @param unstandardised_effect_size the unstandardised difference in increases in score for the experimental and control groups
#' @param sd_c the sample standard deviation of the control group at pre-test
#' @param sd_e the sample standard deviation of the experimental/intervention group at post-test
#' @param n_c the sample size of the control group at pre-test
#' @param n_e the sample size of the experimental/intervention group at pre-test
#' @return the unstandardised effect size divided by the pooled variance of the control and experimental groups
standardised_effect_size <- function(unstandardised_effect_size, sd_c, sd_e, n_c, n_e)
{
  # Standardise using the pooled sample standard deviation
  s_p = sqrt(((n_c - 1)*sd_c^2 + (n_e - 1)*sd_e^2)/(n_c + n_e - 2))
  return(unstandardised_effect_size/s_p)
}

#' Compute the standardised effect size (a form of Cohen's d) and 95% confidence interval for a single study
#' 
#' @param ctrl_pre pre-test score for group 1 (control group)
#' @param exp_pre pre-test score for group 2 (experimental/intervention group)
#' @param ctrl_post post-test score for group 1 (control group)
#' @param exp_post post-test score for group 2 (experimental/intervention group)
#' @param sd_c the sample standard deviation of the control group at pre-test
#' @param sd_e the sample standard deviation of the experimental/intervention group at post-test
#' @param n_c the sample size of the control group at pre-test
#' @param n_e the sample size of the experimental/intervention group at pre-test
compute_ci <- function(ctrl_pre, exp_pre, ctrl_post, exp_post, sd_c, sd_e, n_c, n_e)
{
  ues = unstandardised_effect_size(ctrl_pre, exp_pre, ctrl_post, exp_post)
  d = standardised_effect_size(ues, sd_c, sd_e, n_c, n_e)
  print(ues)
  print(d)
  return(psych::cohen.d.ci(d = d, n = n_c + n_e, n1 = n_c, n2 = n_e, a=0.05))
}

#
# Calculation of confidence intervals for skill changes
# (Difference in increase from pre- to post-test between intervention and control groups)
#

# Asakawa, et al (2019)
# Arithmetic
c_asa_m <- compute_ci(10.95, 10.63, 11.38, 12.07, 2.00, 1.85, 37, 42)
## Fine Motor
c_asa_pm1 <- compute_ci(26.16, 24.42, 26.24, 27.37, 4.19, 4.28, 37, 42)
## Finger Gnosis
c_asa_pm2 <- compute_ci(8.49, 8.74, 8.54, 8.58, 2.21, 2.08, 37, 42)
## Control Group 3 Week Improvement Effect Size
d_ctrl_asa <- (11.38 - 10.95)/2.00
weeks_effect_asa <- 3 * c_asa_m/d_ctrl_asa


# Gracia-Bafalluy and Noel (2008)
## Calculation
c_gra_m <- compute_ci(9.12, 9.06, 12.82, 15.27, 6.47, 5.31, 17, 16)
## Total Finger Gnosis
c_gra_pm1 <- compute_ci(13.12, 13.36, 13.06, 16.57, 1.86, 2.09, 17, 16)
# Subitizing
c_gra_m2 <- compute_ci(14.13, 14.13, 20, 22.64, 1.08, 1.36, 17, 16)
# Enumeration
c_gra_m3 <- compute_ci(4, 4.07, 4.12, 4.25, 1.36, 1.53, 17, 16)
# Counting
c_gra_m4 <- compute_ci(40.36, 36.64, 68.4, 63.3, 11.77, 11.86, 17, 16)
# Ordinality Judgement
c_gra_m5 <- compute_ci(42.5, 43.23, 43.36, 45.50, 5.03, 5.93, 17, 16)
# Magnitude Comparison
c_gra_m6 <- compute_ci(43.08, 45, 31.80, 35.21, 2.69, 2.03, 17, 16)
## Control Group 8 Week Improvement Effect Size
d_ctrl_gra <- (12.82 - 9.12)/6.47
weeks_effect_gra <- 8 * c_gra_m/d_ctrl_gra


# Erasmus, et al (2016)
# Number Concept
c_era_m <- compute_ci(3.11, 2.71, 3.52, 3.33, 1.01, 0.85, 27, 21)
# Visual Perception
c_era_p1 <- compute_ci(11.7, 11.2, 14.1, 16.58, 2.25, 4.22, 27, 21)
# Spatial Orientation
c_era_s1 <- compute_ci(3.41, 3.71, 4.97, 5.23, 1.01, 1.15, 27, 21)
# Fine Motor Skills
c_era_pm1 <- compute_ci(2.44, 2.62, 4.03, 3.52, 1.19, 1.36, 27, 21)
# Gross Motor Coordination
c_era_pm2 <- compute_ci(1.19, 1.76, 1.63, 3.38, 0.96, 0.62, 27, 21)
## Control Group 10 Week Improvement Effect Size
d_ctrl_era <- (3.52 - 3.11)/1.01
weeks_effect_era <- 10 * c_era_m/d_ctrl_era


# Katsipataki (2013)
## Maths (Difference in Standardised Residuals, PIPS Maths Test)
d_kat_m <- (0.09 - -0.08)/sqrt(((29 - 1)*0.99^2 + (27 - 1)*0.99^2)/(29 + 27 - 2)) # Cohen's d
c_kat_m <- psych::cohen.d.ci(d = d_kat_m, n = 29 + 27, n1 = 29, n2 = 27, a=0.05)
## Pooled Motor (Difference in Standardised Residuals, PIPS PE and ASPECTS)
d_kat_pm1 <- (0.18 - -0.17)/sqrt(((29 - 1)*1.08^2 + (27 - 1)*0.84^2)/(29 + 27 - 2)) # Cohen's d
c_kat_pm1 <- psych::cohen.d.ci(d = d_kat_pm1, n = 29 + 27, n1 = 29, n2 = 27, a=0.05)


#
# Plot confidence intervals for each study
#

make_ci_plot <- function(data, text, separator_position, summary_positions){
  separator_props <- gpar(col="#444444")
  separators = list(separator_props, separator_props, separator_props)
  names(separators) = c("2", separator_position, length(data[,1])+1)
  forestplot(text, 
             data,
             txt_gp = fpTxtGp(label = gpar(cex = 0.5), ticks = gpar(cex = 0.5)),
             lineheight = unit(0.8, "cm"),
             graphwidth = unit(5, "cm"),
             align=c("r", "c", "c"),
             colgap = unit(0.2,"cm"),
             new_page = TRUE,
             is.summary=summary_positions,
             boxsize = 0.1,
             xlog=FALSE, 
             lwd.ci=1,
             lty.ci=c("solid"),
             xticks = c(-2, -1, 0, 1, 2, 2.5),
             grid = structure(c(-2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5), 
                              gp = gpar(lty = "dashed", lwd=1, col = "#dddddd")),
             hrzl_lines = separators,
             col=fpColors(box="#222222",line="#222222", summary="#666666"),
             width = 4,
             height = 1)
}


# Asakawa, et al (2019)
trim_decimal <- function(x, places) trimws(format(round(x, places), nsmall=places))
get_ci_label <- function(ci_data, decimal_places=2) return(paste("[", trim_decimal(ci_data[1, "lower"], decimal_places), ", ", trim_decimal(ci_data[1, "upper"], decimal_places), "]"))

asa_table_text<-cbind(
  c("", "Arithmetic", "Fine Motor Ability", "Finger Gnosis"),
  c("Effect Size", trim_decimal(c_asa_m[1, "effect"], 2), trim_decimal(c_asa_pm1[1, "effect"], 2), trim_decimal(c_asa_pm2[1, "effect"], 2)),
  c("95% CI", get_ci_label(c_asa_m, 2), get_ci_label(c_asa_pm1, 2), get_ci_label(c_asa_pm2, 2)))

asa_ci_data <- 
  structure(list(
    mean  = c(NA, c_asa_m[1, "effect"], c_asa_pm1[1,"effect"], c_asa_pm2[1,"effect"]), 
    lower = c(NA, c_asa_m[1, "lower"], c_asa_pm1[1,"lower"], c_asa_pm2[1,"lower"]),
    upper = c(NA, c_asa_m[1, "upper"], c_asa_pm1[1,"upper"], c_asa_pm2[1,"upper"])),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -4L), 
    class = "data.frame")

asa_measure_divider_pos <- 3
asa_summary_positions <- c(TRUE, FALSE, FALSE, FALSE)

make_ci_plot(asa_ci_data, asa_table_text, asa_measure_divider_pos, asa_summary_positions)


# Gracia-Bafalluy and Noel (2008)
gra_table_text<-cbind(
  c("", "Subitizing", "Magnitude Comparison", "Calculation", "Ordinality Judgement", "Enumeration", "Counting", "Finger Gnosis"),
  c("Effect Size", trim_decimal(c_gra_m2[1, "effect"], 2), trim_decimal(c_gra_m6[1, "effect"], 2), trim_decimal(c_gra_m[1, "effect"], 2), trim_decimal(c_gra_m5[1, "effect"], 2), trim_decimal(c_gra_m3[1, "effect"], 2), trim_decimal(c_gra_m4[1, "effect"], 2), trim_decimal(c_gra_pm1[1, "effect"], 2)),
  c("95% CI", get_ci_label(c_gra_m2, 2), get_ci_label(c_gra_m6, 2), get_ci_label(c_gra_m, 2), get_ci_label(c_gra_m5, 2), get_ci_label(c_gra_m3, 2), get_ci_label(c_gra_m4, 2), get_ci_label(c_gra_pm1, 2)))

gra_ci_data <- 
  structure(list(
    mean  = c(NA, c_gra_m2[1, "effect"], c_gra_m6[1, "effect"], c_gra_m[1, "effect"],  c_gra_m5[1, "effect"], c_gra_m3[1, "effect"], c_gra_m4[1, "effect"], c_gra_pm1[1, "effect"]), 
    lower = c(NA, c_gra_m2[1, "lower"], c_gra_m6[1, "lower"],c_gra_m[1, "lower"], c_gra_m5[1, "lower"], c_gra_m3[1, "lower"], c_gra_m4[1, "lower"], c_gra_pm1[1, "lower"]),
    upper = c(NA, c_gra_m2[1, "upper"],  c_gra_m6[1, "upper"], c_gra_m[1, "upper"], c_gra_m5[1, "upper"], c_gra_m3[1, "upper"], c_gra_m4[1, "upper"], c_gra_pm1[1, "upper"])),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -7L), 
    class = "data.frame")

gra_measure_divider_pos <- 8
gra_summary_positions <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

make_ci_plot(gra_ci_data, gra_table_text, gra_measure_divider_pos, gra_summary_positions)


# Katsipataki (2013)
kat_table_text<-cbind(
  c("", "Mathematics (General)", "Pooled Motor"),
  c("Effect Size", trim_decimal(c_kat_m[1, "effect"], 2), trim_decimal(c_kat_pm1[1, "effect"], 2)),
  c("95% CI", get_ci_label(c_kat_m, 2), get_ci_label(c_kat_pm1, 2)))

kat_ci_data <- 
  structure(list(
    mean  = c(NA, c_kat_m[1, "effect"], c_kat_pm1[1,"effect"]), 
    lower = c(NA, c_kat_m[1, "lower"], c_kat_pm1[1,"lower"]),
    upper = c(NA, c_kat_m[1, "upper"], c_kat_pm1[1,"upper"])),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -3L), 
    class = "data.frame")

kat_measure_divider_pos <- 3
kat_summary_positions <- c(TRUE, FALSE, FALSE)

make_ci_plot(kat_ci_data, kat_table_text, kat_measure_divider_pos, kat_summary_positions)


# Erasmus, et al (2016)
era_table_text<-cbind(
  c("", "Number Concept", "Gross Motor Coordination", "Fine Motor Ability"),
  c("Effect Size", trim_decimal(c_era_m[1, "effect"], 2), trim_decimal(c_era_pm2[1, "effect"], 2), trim_decimal(c_era_pm1[1,"effect"], 2)),
  c("95% CI", get_ci_label(c_era_m, 2), get_ci_label(c_era_pm2, 2), get_ci_label(c_era_pm1, 2)))

era_ci_data <- 
  structure(list(
    mean  = c(NA, c_era_m[1, "effect"], c_era_pm2[1,"effect"], c_era_pm1[1,"effect"]), 
    lower = c(NA, c_era_m[1, "lower"], c_era_pm2[1,"lower"], c_era_pm1[1,"lower"]),
    upper = c(NA, c_era_m[1, "upper"], c_era_pm2[1,"upper"], c_era_pm1[1,"upper"])),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -3L), 
    class = "data.frame")

era_measure_divider_pos <- 3
era_summary_positions <- c(TRUE, FALSE, FALSE, FALSE)

make_ci_plot(era_ci_data, era_table_text, era_measure_divider_pos, era_summary_positions)