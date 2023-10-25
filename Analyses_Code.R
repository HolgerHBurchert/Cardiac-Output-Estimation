## Author:
## Project:
## Title:
## Last Edited:

# Install packages 
packages <- c("ggplot2", "ggpubr", "rootSolve", "cowplot", "car", "AICcmodavg")
install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)

# General comments
# 1. Pack code for each model/plot in function and only call the functions. This makes it easier for users to see what is going on
# 2. Docstrings: Add Roxygen docstrings for each function you define. Functions should be defined at the start of the file
# 3. Did you check the model assumptions for the original data?
# 4. Dependencies in R should be defined in a DESCRIPTION file or in the code itself (I inserted it above)


# Required libraries which are available at https://cran.r-project.org/
library(ggplot2)
library(ggpubr)
library(rootSolve)
library(cowplot)
library(car)
library(AICcmodavg)

# Loading the data files which must be in same folder as the R code file
Stringer <- read.csv("Data_Stringer_AJP_1997.csv" , header = TRUE, sep = ",")
Astrand  <- read.csv("Data_Astrand_AJP_1964.csv"  , header = TRUE, sep = ";")


############################ Stringer: LINEAR MODEL ############################

# Fitting linear model to Stringer's data 
Stringer_linear <- lm(avDiff_mLper100mL ~ pc_VO2max, data = Stringer)

# Linear model summary and 95% Confidence Intervals for model coefficients 
summary(Stringer_linear) 
confint(Stringer_linear) 


# Constructing prediction interval (_pi); & combine it with Stringer_linear data frame
Stringer_linear_pi <- predict(Stringer_linear, interval = "prediction")
Stringer_linear_pi <- cbind(Stringer, Stringer_linear_pi)


# Plotting Stringer's the linear model
Figure2A <- 
  ggplot(data = Stringer_linear_pi, aes(x = pc_VO2max, y = avDiff_mLper100mL)) +
  geom_point(shape = 21, color = "grey21", fill = "black", stroke = 0.1, size = 0.7) +
  geom_smooth(method = lm, se = TRUE, linewidth = 0.2, colour = "black") +
  geom_line(aes(y = lwr), color = "black", linetype = "dashed", linewidth = 0.2) +
  geom_line(aes(y = upr), color = "black", linetype = "dashed", linewidth = 0.2) +
  # Following command adds regression equation to plot
  stat_regline_equation(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~~")), 
    formula = y ~ x, 
    size = 2.5
  ) +
  annotate("text", x = 5, y = 16.5, label = "n = 105", hjust = 0, vjust = 1, size = 2.5) +
  annotate("text", x = 5, y = 15.5, label = "p < 0.001", hjust = 0, vjust = 1, size = 2.5) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  xlab(expression("%VO"[2] ~ max)) +
  ylab(expression(paste("C(a-", bar(v), "DO"[2]*")" ~ "(ml/100ml)"))) + 
  theme_bw() +
  theme(text = element_text(size = 9), axis.text = element_text(size = 8))


# Create R base scatterplot smoother for Stringer's linear Residuals vs Fitted Values plot
lowess_Stringer_linear <-
  as.data.frame(with(
    Stringer_linear$model,
    lowess(x = Stringer_linear$fitted.values, y = Stringer_linear$residuals)
  ))

# Residuals vs Fitted Values for Stringers' linear model
Figure2B <- 
  ggplot(Stringer_linear$model, aes(x = Stringer_linear$fitted.values, y = Stringer_linear$residuals)) +
  geom_point(shape = 21, color = "grey21", fill = "black", stroke = 0.1, size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed",col = "grey") +
  geom_path(data = lowess_Stringer_linear, aes(x = x, y = y), col = "black", linewidth = 0.2) +
  xlab(expression("Fitted Values")) +
  ylab(expression("Residuals")) +
  theme_bw() +
  theme(text = element_text(size = 9), axis.text = element_text(size = 8))



###################### Stringer: 3rd ORDER POLYNOMIAL MODEL #####################


# Fitting 3rd order polynomial function to Stringer's data
Stringer_3polynomial <- lm(avDiff_mLper100mL ~ poly(pc_VO2max, degree=3, raw=TRUE), data=Stringer)

# 3rd order polynomial model summary and 95% Confidence Intervals for coefficients
summary(Stringer_3polynomial) 
confint(Stringer_3polynomial) 

# Finding x_inflection of 3rd order polynomial fit function for Stringer's data 
# is done by using the formula x_inflection = -(b / 3a), where f(x) = ax^3 + bx^2 + cx + d 
# and a != 0
Stringer_inflection <- -(coef(Stringer_3polynomial)[3] / (3 * coef(Stringer_3polynomial)[4]))

# Calculating approximate 95%-confidence interval for inflection point using the delta method
Stringer_inflection_ci <- car::deltaMethod(Stringer_3polynomial, "-b2/(3*b3)", parameterNames = paste0("b", 0:3), level = 0.95)

# Constructing prediction interval (_pi); combining it with Stringer_3polynomial data frame
Stringer_3polynomial_pi <- predict(Stringer_3polynomial, interval = "prediction")
Stringer_3polynomial_pi <- cbind(Stringer, Stringer_3polynomial_pi)


# Plotting 3rd order polynomial model 
Figure2C <- 
  ggplot(data = Stringer_3polynomial_pi, aes(x = pc_VO2max, y = avDiff_mLper100mL)) +
  geom_point(shape = 21, color = "grey21", fill = "black", stroke = 0.1, size = 0.7) +
  geom_smooth(
    method = lm,
    se = TRUE,
    formula = y ~ poly(x, 3, raw = TRUE),
    linewidth = 0.2,
    colour = "black"
  ) +
  geom_line(aes(y = lwr), color = "black", linetype = "dashed", linewidth = 0.2) +
  geom_line(aes(y = upr), color = "black", linetype = "dashed", linewidth = 0.2) +
  # Following command adds regression equation to plot
  stat_regline_equation(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~~")), 
    formula = y ~ poly(x, 3, raw = TRUE), 
    geom = "text", 
    size = 2.5
  ) +
  annotate("text", x = 7, y = 16.8, label = "n = 105", hjust = 0, vjust = 1, size = 2.5) +
  annotate("text", x = 7, y = 15.8, label = "p < 0.001", hjust = 0, vjust = 1, size = 2.5) +
  geom_vline(xintercept = Stringer_inflection, linetype = "dashed", linewidth = 0.2) +
  annotate(
    "rect",
    fill = "grey",
    alpha = 0.3,
    xmin = Stringer_inflection_ci$`2.5 %`,
    xmax = Stringer_inflection_ci$`97.5 %`,
    ymin = -Inf,
    ymax = Inf
  ) +
  annotate(
    "text",
    x = 58,
    y = 3.5,
    label = "Inflection Point (56.8%)",
    hjust = 0,
    vjust = 1,
    size = 2.5
  ) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  xlab(expression("%VO"[2] ~ max)) +
  ylab(expression(paste("C(a-", bar(v), "DO"[2]*")" ~ "(ml/100ml)"))) + 
  theme_bw() +
  theme(text = element_text(size = 9), axis.text = element_text(size = 8))



# R base scatterplot smoother for following Residuals vs Fitted Values plot
lowess_Stringer_3polynomial <-
  as.data.frame(with(
    Stringer_3polynomial$model,
    lowess(x = Stringer_3polynomial$fitted.values, y = Stringer_3polynomial$residuals)
  ))

# Residuals vs Fitted Values for Stringers' 3rd order polynomial model
Figure2D <- 
  ggplot(Stringer_3polynomial$model, aes(x = Stringer_3polynomial$fitted.values, y = Stringer_3polynomial$residuals)) +
  geom_point(shape = 21, color = "grey21", fill = "black", stroke = 0.1, size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "grey") +
  geom_path(data = lowess_Stringer_3polynomial, aes(x = x, y = y), col = "black", linewidth = 0.2) +
  xlab(expression("Fitted Values")) +
  ylab(expression("Residuals")) +
  theme_bw() +
  theme(text = element_text(size = 9), axis.text = element_text(size = 8))


# Partial F-test comparing linear- vs 3rd order polynomial model
anova(Stringer_linear, Stringer_3polynomial, test = "F") 


# Creating multi-panel figure
Figure2 <- plot_grid(
  Figure2A,
  Figure2B,
  Figure2C,
  Figure2D,
  nrow = 2,
  labels = c('A', 'B', 'C', 'D')
)

# Exporting plots from plot_grid command
ggsave("Figure2.pdf", Figure2 , width = 18, height = 11, units = "cm")


########################### Compare Stringer linear and cubic model using Akaike weights #########################

aictab(list(linear = Stringer_linear, cubic = Stringer_3polynomial), sort = FALSE)

########################### PREPARING ASTRAND DATA SET #########################

# Astrand's VO2 data are not in percentage of maximal VO2. A new function, 
# 'pc_func', is defined to calculate the percentage. It takes one argument, 'arg1',
# a set of VO2 data, and calculates the percentage based on the maximum value in the set.
pc_func <- function(arg1) {
  arg1 / max(arg1) * 100
}

# Calculating the percentages of maximum VO2 for each participant using pc_func function
Astrand$VO2_L_pc <- ave(Astrand$VO2_L, Astrand$ID, FUN = pc_func)



########################### ASTRAND LINEAR MODEL  ##############################

# Fits linear model to Astrand's data 
Astrand_linear <- lm(avO2diff_mLper100mL ~ VO2_L_pc, data = Astrand)

# Linear model summary and 95% Confidence Intervals for model coefficients
summary(Astrand_linear) 
confint(Astrand_linear) 


# Constructing prediction interval (_pi); combining it with data in data frame
Astrand_linear_pi <- predict(Astrand_linear, interval = "prediction")
Astrand_linear_pi <- cbind(Astrand, Astrand_linear_pi)


# Constructing Figure 3A
Figure3A <- 
  ggplot(data = Astrand_linear_pi, aes(x = VO2_L_pc, y = avO2diff_mLper100mL)) +
  geom_point(
    aes(colour = factor(Gender), fill = factor(Gender)),
    shape = 21,
    show.legend = FALSE,
    stroke = 0.1,
    size = 0.7
  ) +
  scale_fill_manual(values = c("Female" = "red", "Male" = "black")) +
  scale_color_manual(values = c("Female" = "firebrick2", "Male" = "grey21")) +
  geom_smooth(method  = lm, se = TRUE, formula = y ~ x, linewidth = 0.2, colour = "black") +
  geom_line(aes(y = lwr), color = "black", linetype = "dashed", linewidth = 0.2) +
  geom_line(aes(y = upr), color = "black", linetype = "dashed", linewidth = 0.2) +
  xlab(expression("%VO"[2] ~ max)) +
  ylab(expression(paste("C(a-", bar(v), "DO"[2]*")" ~ "(ml/100ml)"))) + 
  stat_regline_equation(
    aes(label=paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~~")), 
    formula = y ~ x, 
    size = 2.5
  ) +
  annotate("text", x = 5, y = 17.8, label = "n = 126", hjust = 0, vjust = 1, size = 2.5) +
  annotate("text", x = 5, y = 16.5, label = "p < 0.001", hjust = 0, vjust = 1, size = 2.5) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 20,  by = 2)) +
  theme_bw() +
  theme(text = element_text(size = 9), axis.text = element_text(size = 8))



# R base  scatterplot smoother for following Residuals vs Fitted Values plot
lowess_Astrand_linear <-
  as.data.frame(with(
    Astrand_linear$model,
    lowess(x = Astrand_linear$fitted.values, y = Astrand_linear$residuals)
  ))

# Residuals vs Fitted Values for Astrands' linear model #DOUBLE CHECK IF COLOUR CODING OF GROUPS IS CORRECT
Figure3B <- 
  ggplot(Astrand_linear$model, aes(x = Astrand_linear$fitted.values, y = Astrand_linear$residuals)) +
  geom_point(
    aes(colour = factor(Astrand$Gender), fill = factor(Astrand$Gender)),
    shape = 21,
    show.legend = FALSE,
    stroke = 0.1,
    size = 0.7
  ) +
  scale_fill_manual(values = c("Female" = "red", "Male" = "black")) +
  scale_color_manual(values = c("Female" = "firebrick2", "Male" = "grey21")) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "grey") +
  geom_path(data = lowess_Astrand_linear, aes(x = x, y = y), col = "black", linewidth = 0.2) +
  xlab(expression("Fitted Values")) +
  ylab(expression("Residuals")) +
  theme_bw() +
  theme(text = element_text(size = 9), axis.text = element_text(size = 8))


########################### Astrand 3rd ORDER POLYNOMIAL MODEL #################

# Fitting 3rd order polynomial function to Astrand's data
Astrand_3polynomial <- lm(avO2diff_mLper100mL ~ poly(VO2_L_pc, degree=3, raw=TRUE), data=Astrand)

# Summary of the model and 95% confidence intervals for model coefficients
summary(Astrand_3polynomial)
confint(Astrand_3polynomial)


# Finding x_inflection of 3rd order polynomial fit function for Stringer's data 
# is done by using the formula x_inflection = -(b / 3a), where f(x) = ax^3 + bx^2 + cx + d 
# and a != 0
Astrand_inflection <- -(coef(Astrand_3polynomial)[3] / (3 * coef(Astrand_3polynomial)[4]))

# Calculating approximate 95%-confidence interval for inflection point using the delta method
Astrand_inflection_ci <- car::deltaMethod(Astrand_3polynomial, "-b2/(3*b3)", parameterNames = paste0("b", 0:3), level = 0.95)

# Constructing prediction interval (_pi); combining it with data in data frame
Astrand_3polynomial_pi <- predict(Astrand_3polynomial, interval = "prediction")
Astrand_3polynomial_pi <- cbind(Astrand, Astrand_3polynomial_pi)


# Plotting data with 3rd order polynomial fit function
Figure3C <- 
  ggplot(data = Astrand_3polynomial_pi, aes(x = VO2_L_pc, y = avO2diff_mLper100mL)) +
  geom_point(
    aes(colour = factor(Gender), fill = factor(Gender)),
    shape = 21,
    show.legend = FALSE,
    stroke = 0.1,
    size = 0.7
  ) +
  scale_fill_manual(values = c("Female" = "red", "Male" = "black"))+
  scale_color_manual(values = c("Female" = "firebrick2", "Male" = "grey21")) +
  scale_x_continuous(breaks = seq(0, 100, by=10)) +
  scale_y_continuous(breaks = seq(0, 20,  by=2)) +
  geom_smooth(method=lm, se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), linewidth=0.2, colour="black") +
  geom_line(aes(y = lwr), color = "black", linetype = "dashed", linewidth=0.2) +
  geom_line(aes(y = upr), color = "black", linetype = "dashed", linewidth=0.2) +
  xlab(expression("%VO"[2] ~ max)) +
  ylab(expression(paste("C(a-", bar(v), "DO"[2]*")" ~ "(ml/100ml)"))) + 
  geom_vline(xintercept = Astrand_inflection, linetype = "dashed", linewidth=0.2) +
  annotate(
    "rect",
    fill = "grey",
    alpha = 0.3,
    xmin = Astrand_inflection_ci$`2.5 %`,
    xmax = Astrand_inflection_ci$`97.5 %`,
    ymin = -Inf,
    ymax = Inf
  ) +
  annotate("text", x=65, y=1.8, label="Inflection Point (64.3%)", hjust=0, vjust=1, size=2.5) +
  annotate("text", x=5, y=18, label = "n = 126", hjust=0, vjust=1,size=2.5) +
  annotate("text", x=5, y=16.7, label="p < 0.001", hjust=0, vjust=1,size=2.5) + 
  stat_regline_equation(
    label.x = 5,
    label.y = 19,
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~~")),
    formula = y ~ poly(x, 3, raw = TRUE),
    size = 2.5
  ) +
  theme_bw() +
  theme(text=element_text(size=9), axis.text = element_text(size=8))



# R base scatterplot smoother for following Residuals vs Fitted Values plot
lowess_Astrand_3polynomial <-
  as.data.frame(with(
    Astrand_3polynomial$model,
    lowess(x = Astrand_3polynomial$fitted.values, y = Astrand_3polynomial$residuals)
  ))

# Residuals vs Fitted Values for Astrands' linear model #DOUBLE CHECK IF COLOUR CODING OF GROUPS IS CORRECT
Figure3D <- 
  ggplot(Astrand_3polynomial$model, aes(x = Astrand_3polynomial$fitted.values, y = Astrand_3polynomial$residuals)) +
  geom_point(
    aes(colour = factor(Astrand$Gender), fill = factor(Astrand$Gender)),
    shape = 21,
    show.legend = FALSE,
    stroke = 0.1,
    size = 0.7
  ) +
  scale_fill_manual(values = c("Female" = "red", "Male" = "black")) +
  scale_color_manual(values = c("Female" = "firebrick2", "Male" = "grey21")) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "grey") +
  geom_path(data = lowess_Astrand_3polynomial, aes(x = x, y = y), col = "black", linewidth = 0.2) +
  xlab(expression("Fitted Values")) +
  ylab(expression("Residuals")) +
  theme_bw() +
  theme(text = element_text(size = 9), axis.text = element_text(size = 8))


# Creating multi-panel figure
Figure3 <-
  plot_grid(
    Figure3A,
    Figure3B,
    Figure3C,
    Figure3D,
    nrow = 2,
    labels = c('A', 'B', 'C', 'D')
  )

# Exporting plot 
ggsave("Figure3.pdf", Figure3 , width = 18, height = 11, units = "cm")

# Partial F-test comparing linear and 3rd order polynomial fit to Astrand's data
anova(Astrand_linear, Astrand_3polynomial, test = "F")

########################### Compare Astrang linear and cubic model using Akaike weights #########################

aictab(list(linear = Astrand_linear, cubic = Astrand_3polynomial), sort = FALSE)

########################### Astrand 3rd ORDER POLYNOMIAL MODEL by gender #################

# Astrand model with different curves for each gender

Astrand_3polynomial_gender <- lm(avO2diff_mLper100mL ~ poly(VO2_L_pc, degree=3, raw=TRUE)*Gender, data=Astrand)

# Plot curves separately for genders
# Set up the prediction data frame

pred_frame <- expand.grid(
  Gender = factor(c("Female", "Male"))
  , VO2_L_pc = seq(min(Astrand$VO2_L_pc, na.rm = TRUE), max(Astrand$VO2_L_pc, na.rm = TRUE), length = 1000)
)

preds_ci <- predict(Astrand_3polynomial_gender, newdata = pred_frame, interval = "confidence")
pred_frame$fit <- preds_ci[, 1]
pred_frame$ci_lwr <- preds_ci[, 2]
pred_frame$ci_upr <- preds_ci[, 3]
preds_pi <- predict(Astrand_3polynomial_gender, newdata = pred_frame, interval = "prediction")
pred_frame$pi_lwr <- preds_pi[, 2]
pred_frame$pi_upr <- preds_pi[, 3]

# Calculate the inflection points and corresponding confidence intervals

inflect_f <- car::deltaMethod(Astrand_3polynomial_gender, "-b2/(3*b3)", parameterNames = paste0("b", 0:7), level = 0.95) # Inflection point for females
inflect_m <- car::deltaMethod(Astrand_3polynomial_gender, "-(b2 + b6)/(3*(b3 + b7))", parameterNames = paste0("b", 0:7), level = 0.95) # Inflection point for males

inflect_frame <- data.frame(
  Gender = factor(c("Female", "Male"))
  , x = c(inflect_f$Estimate, inflect_m$Estimate)
  , lwr = c(inflect_f$`2.5 %`, inflect_m$`2.5 %`)
  , upr = c(inflect_f$`97.5 %`, inflect_m$`97.5 %`)
)

# The annotation labels (equations, r2, inflection points)

equation_labels <- c(
  sprintf("italic(y) == %.2g %+.2g ~ italic(x) %+.1e ~ italic(x^2) %+.1e ~ italic(x^3) ",
          coef(Astrand_3polynomial_gender)[1], coef(Astrand_3polynomial_gender)[2], coef(Astrand_3polynomial_gender)[3], coef(Astrand_3polynomial_gender)[4])
  , sprintf("italic(y) == %.2g %+.2g ~ italic(x) %+.1e ~ italic(x^2) %+.1e ~ italic(x^3) ",
            coef(Astrand_3polynomial_gender)[1 + 4], coef(Astrand_3polynomial_gender)[2 + 4], coef(Astrand_3polynomial_gender)[3 + 4], coef(Astrand_3polynomial_gender)[4 + 4])
)

text_frame <- data.frame(
  Gender = factor(c("Female", "Male"))
  , x = c(inflect_f$Estimate, inflect_m$Estimate) + 1
  , formula = gsub("e([+-]?[0-9]*)", "%*%10^\\1", equation_labels)
  , r2 = rep(c(sprintf("adj.~italic(R^2) == %.2f", summary(Astrand_3polynomial_gender)$adj.r.squared)), 2)
  , n = c(sprintf("n == %.0f", table(Astrand$Gender)["Female"]), sprintf("n == %.0f", table(Astrand$Gender)["Male"]))
  , inflection = c(
    sprintf("Inflection Point (%.1f%%)", inflect_f$Estimate)
    , sprintf("Inflection Point (%.1f%%)", inflect_m$Estimate)
  )
)

# Make the graphic

Figure_supplement1 <- ggplot(data = Astrand, aes(x = VO2_L_pc, y = avO2diff_mLper100mL)) +
  geom_point(
    aes(colour = factor(Gender), fill = factor(Gender)),
    shape = 21,
    show.legend = FALSE,
    stroke = 0.1,
    size = 0.7
  ) +
  geom_line(data = pred_frame, aes(x = VO2_L_pc, y = fit, group = Gender), linewidth=0.2) +
  geom_ribbon(data = pred_frame, aes(x = VO2_L_pc, y = fit, ymin = ci_lwr, ymax = ci_upr, group = Gender), colour = NA, alpha = 0.2) +
  geom_ribbon(data = pred_frame, aes(x = VO2_L_pc, y = fit, ymin = pi_lwr, ymax = pi_upr, group = Gender), colour = "black", fill = NA, linetype = 2, linewidth = 0.2) +
  geom_vline(data = inflect_frame, aes(xintercept = x, group = Gender), linetype = 2, linewidth = 0.2) +
  geom_rect(data = inflect_frame, aes(xmin = lwr, xmax = upr, ymin = -Inf, ymax = Inf, group = Gender), fill = "grey", alpha = 0.3, inherit.aes = FALSE) +
  facet_wrap(~Gender) +
  geom_label(data = text_frame, aes(label = formula), x = 5, y = 21, parse = TRUE, hjust = 0, size = 2.4, fill = "white", label.size = NA, label.padding = unit(0.1, "lines")) +
  geom_label(data = text_frame, aes(label = r2), x = 5, y = 21-2.1, parse = TRUE, hjust = 0, size = 2.4, fill = "white", label.size = NA, label.padding = unit(0.1, "lines")) +
  geom_label(data = text_frame, aes(label = n), x = 5, y = 21-2.1*2, parse = TRUE, hjust = 0, size = 2.4, fill = "white", label.size = NA, label.padding = unit(0.1, "lines")) +
  geom_text(data = text_frame, aes(label = inflection, x = x), y = 1, parse = FALSE, hjust = 0, size = 2) +
  scale_fill_manual(values = c("Female" = "red", "Male" = "black"))+
  scale_color_manual(values = c("Female" = "firebrick2", "Male" = "grey21")) +
  scale_x_continuous(breaks = seq(0, 120, by=10)) +
  scale_y_continuous(breaks = seq(0, 20,  by=2), limits = c(0, 22)) +
  labs(
    y = expression(paste("C(a-", bar(v), "DO"[2]*")" ~ "(ml/100ml)"))
    , x =expression("%VO"[2] ~ max)
  ) +
  theme_bw() +
  theme(
    text=element_text(size=9)
    , axis.text = element_text(size=8)
    , strip.text.x = element_text(size = 8)
  )

# Residual graphic

lowess_Astrand_3polynomial <-
  as.data.frame(with(
    Astrand_3polynomial_gender$model,
    lowess(x = Astrand_3polynomial_gender$fitted.values, y = Astrand_3polynomial_gender$residuals)
  ))

# Residuals vs Fitted Values for Astrands' linear model #DOUBLE CHECK IF COLOUR CODING OF GROUPS IS CORRECT

Figure_supplement_resid <- 
  ggplot(Astrand_3polynomial_gender$model, aes(x = Astrand_3polynomial_gender$fitted.values, y = Astrand_3polynomial_gender$residuals)) +
  geom_point(
    aes(colour = factor(Astrand$Gender), fill = factor(Astrand$Gender)),
    shape = 21,
    show.legend = FALSE,
    stroke = 0.1,
    size = 1
  ) +
  scale_fill_manual(values = c("Female" = "red", "Male" = "black")) +
  scale_color_manual(values = c("Female" = "firebrick2", "Male" = "grey21")) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "grey") +
  geom_path(data = lowess_Astrand_3polynomial, aes(x = x, y = y), col = "black", linewidth = 0.2) +
  xlab(expression("Fitted Values")) +
  ylab(expression("Residuals")) +
  theme_bw() +
  theme(text = element_text(size = 9), axis.text = element_text(size = 8))


# Creating multi-panel figure
Figure_supplement <- plot_grid(
  Figure_supplement1
  , Figure_supplement_resid
  , nrow = 2
  , labels = c('A', 'B')
)

ggsave("Figure_supplement.pdf", Figure_supplement , width = 25*0.6, height = 20*0.6, dpi = 400, units = "cm")

######################## PLOT MODEL COMPARISON ##################################

# Store 3rd order polynomial model of Astrand's data as function named Astrand_fit_function
Astrand_fit_function <- function(x) {
  coef(Astrand_3polynomial)[1]         +
    coef(Astrand_3polynomial)[2] * x     +
    coef(Astrand_3polynomial)[3] * x ^ 2 +
    coef(Astrand_3polynomial)[4] * x ^ 3
}

# Plot Comparing the models
Figure4A <- 
  ggplot(data = Stringer_linear_pi, aes(x = pc_VO2max, y = avDiff_mLper100mL)) +
  geom_point(shape = 21, color = "grey21", fill  = "black", stroke = 0.1, size=0.7) +
  geom_smooth(method = lm, se = FALSE, linewidth = 0.2, col = "red") +
  geom_smooth(method = lm, se = FALSE, formula = y ~ poly(x, 3, raw = TRUE), linewidth = 0.2, col="red") +
  geom_vline(xintercept = Stringer_inflection, linetype = "dashed", linewidth=0.2) +
  annotate(
    "text",
    x = 58,
    y = 3.7,
    label = "Inflection Point (56.8%)",
    hjust = 0,
    vjust = 1,
    size = 2.5
  ) +
  geom_function(fun = Astrand_fit_function, linewidth = 0.2) + # Astrand_fit_function from above
  scale_x_continuous(breaks=seq(0, 100, by = 10)) +
  scale_y_continuous(breaks=seq(0,  20, by = 2)) +
  xlab(expression("%VO"[2] ~ max)) +
  ylab(expression(paste("C(a-", bar(v), "DO"[2]*")" ~ "(ml/100ml)"))) + 
  theme_bw() +
  theme(text = element_text(size = 9))


################### INFLECTION POINT OF SEVERINGHAUS ODC #######################

# Severinghaus function for Oxygen Dissociation Curve as expression
Severinghaus <- expression((((PO2^3 + 150*PO2)^-1 * 23400) + 1)^-1)


# Calculating the 3 derivatives of the Severinghaus function
first_derivative_Severinghaus  <- D(Severinghaus, 'PO2')
second_derivative_Severinghaus <- D(first_derivative_Severinghaus, 'PO2')
third_derivative_Severinghaus  <- D(second_derivative_Severinghaus, 'PO2')


# Calculating the roots of the 2nd derivative in the PO2 interval 0 to 100 mmHg
Severinghaus_inflection_PO2 <- uniroot.all(
  function(PO2) eval(second_derivative_Severinghaus),
  interval = c(0, 100)
)
Severinghaus_inflection_PO2


# Checking 3rd derivative at inflection. If < 0 = TRUE, then concave up to concave down
PO2 <- Severinghaus_inflection_PO2
eval(third_derivative_Severinghaus) < 0 


# Get saturation of inflection by solving Severinghaus equation at inflection
PO2 <- Severinghaus_inflection_PO2
Severinghaus_inflection_saturation <- eval(Severinghaus)
Severinghaus_inflection_saturation


# Calculate the slope of the Severinghaus function at inflection point
PO2 <- Severinghaus_inflection_PO2
Severinghaus_inflection_slope <- eval(first_derivative_Severinghaus)


# Use Severinghaus equation to create an R function that outputs Saturation for Po2 values
Severinghaus_function <- function(x){ (((x^3 + 150*x)^-1 * 23400) + 1)^-1 }


# To find the slope tangent of the Severinghaus function's inflection point, 
# calculate its intercept using the formula y = m * x + b, where b = y - m * x.
Severinghaus_inflection_intercept <- Severinghaus_inflection_saturation - 
  Severinghaus_inflection_slope * 
  Severinghaus_inflection_PO2


# Plotting Severinghaus_function with slope tangent of inflection
Figure4B <- 
  ggplot(data.frame(x = c(0, 100)), aes(x = x)) +
  stat_function(fun = Severinghaus_function, colour = "black" , linewidth = 0.5, xlim = c(0,100)) + 
  # Plots horizontal dashed line corresponding to saturation at inflection 
  geom_segment(
    aes(
      x = 0,
      xend = Severinghaus_inflection_PO2,
      y = Severinghaus_inflection_saturation,
      yend = Severinghaus_inflection_saturation,
    ),
    linetype = "dashed",
    linewidth = 0.2
  )+
  # Plots slope tangent of the inflection point
  geom_abline(
    intercept = Severinghaus_inflection_intercept,
    slope = Severinghaus_inflection_slope,
    linetype = "twodash",
    color = "black",
    linewidth = 0.5
  ) +
  # Plots vertical dashed line corresponding to PO2 at inflection
  geom_segment(
    aes(
      x = Severinghaus_inflection_PO2,
      xend = Severinghaus_inflection_PO2,
      y = 0,
      yend = Severinghaus_inflection_saturation
    ),
    linetype = "dashed",
    linewidth = 0.2
  ) +
  # Plots vertical dashed line corresponding to PO2 = 40 mmHg (general resting PO2 mixed venous blood)
  geom_segment(
    aes(
      x = 40,
      xend = 40,
      y = 0,
      yend = Severinghaus_function(40)
    ),
    linetype = "dashed",
    linewidth = 0.2
  ) +
  # Plots horizontal dashed line at saturation corresponding to PO2 = 40 mmHg 
  geom_segment(aes(
    x = 0,
    xend = 40,
    y = Severinghaus_function(40),
    yend = Severinghaus_function(40)
  ),
  linetype = "dashed",
  linewidth = 0.2) +
  scale_y_continuous(
    name = "Fractional Saturation",
    breaks = seq(0.1, 1.0, 0.1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    name = expression("PO"[2] * " (mmHg)"),
    breaks = seq(0, 100, 10),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(text = element_text(size = 9))


# Creating multi-panel figure
Figure4 <- plot_grid(Figure4A, Figure4B, nrow = 1, labels = c('A', 'B'))

# Exporting plots from plot_grid command
ggsave("Figure4.pdf", Figure4 , width = 18, height = 5.5, units = "cm")


# Stringer's 3rd order polynomial fit to estimate the arterio-mixed-venous O2 
# difference (avDiff) at 56.8% VO2max.
VO2max_56.8_percent <- data.frame(pc_VO2max=c(56.8))
avDiff_at_VO2max_56.8_percent <- predict(Stringer_3polynomial, newdata = VO2max_56.8_percent)
avDiff_at_VO2max_56.8_percent

# Stringer's 3rd order polynomial fit to estimate the arterio-mixed-venous O2 
# difference (avDiff) at 60% VO2max.
VO2max_60_percent <- data.frame(pc_VO2max=c(60))
avDiff_at_VO2max_60_percent <- predict(Stringer_3polynomial, newdata = VO2max_60_percent)
avDiff_at_VO2max_60_percent