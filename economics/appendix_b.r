# Philosophy, Economic, and Social Small Sample Survey Series

# Libraries and Options ---------------------------------------------------
set.seed(42)
options(scipen = 4000)

if (!require(pacman)) { install.packages("pacman"); require(pacman) }
p_load(censusapi, gt, gtools, gtsummary, h2o, Hmisc, kable, kableExtra, Matching,
       multcomp, plotly, RColorBrewer, remotes, scales, survey, tidyverse,
       tm, wordcloud, install = F)
#pacman::p_load_gh("gergness/srvyr", "tidy-survey-r/srvyrexploR")

# Raw Data ----------------------------------------------------------------
pollfish1 <- read_csv("../../Pollfish_Survey_An_Inquiry_into_the_Alleged_Analytic-Synthetic_Bifurcation_of_Truth_389678686_en_noapikey.csv")
pollfish2 <- read_csv("../../Pollfish_Survey_Multi-purpose_Opinion_Poll_389686154_en_noapikey.csv")

# Preview -----------------------------------------------------------------
pollfish1
pollfish2

# Cleaning ----------------------------------------------------------------
table(unlist(sapply(pollfish1, class)))
colnames(pollfish1)

raw     <- pollfish1
profile <- raw[, c(1:13,27:ncol(raw))]
ballot  <- raw[, 14:26]

dat.char <- ballot[,sapply(ballot, class) == "character"]
dat.num  <- ballot[,sapply(ballot, class) == "numeric"]

colnames(dat.num) <- c("inherit_received", 
                       "inherit_expected",
                       "gifts_received")

colnames(dat.char) <- c("as_logic",
                        "support_bill",
                        "as_groups",
                        "book_name",
                        "townhall",
                        "is_true1",
                        "is_true2",
                        "is_true3",
                        "is_true4",
                        "privacy")

responses        <- cbind(dat.num, dat.char, profile)
responses$Weight <- pollfish1$Weight

raw     <- pollfish2
profile <- raw[, c(1:13,32:ncol(raw))]
ballot  <- raw[, 14:31]

dat.char <- ballot[,sapply(ballot, class) == "character"]
#dat.num  <- ballot[,sapply(ballot, class) == "numeric"] # There are none

colnames(dat.char) <- c("as_logic",
                        "support_bill",
                        "q_types_truth",
                        "as_groups",
                        "equal_op",
                        "fair_chood",
                        "simulation",
                        "ai_rights",
                        "ai_sentiments",
                        "gov_truth",
                        "gov_censor",
                        "is_true1",
                        "is_true2",
                        "is_true3",
                        "is_true4",
                        "boundaries",
                        "book_name",
                        "subtitle")

responses1        <- cbind(dat.char, profile)
responses1$Weight <- pollfish2$Weight

responses$round  <- 1
responses1$round <- 2

# Simulation by Weighted Oversampling -------------------------------------
sim1 <- sample(as.numeric(rownames(responses)), size = 10000, replace = T, 
               prob = responses$Weight/100)
sim2 <- sample(as.numeric(rownames(responses1)), size = 10000, replace = T, 
               prob = responses1$Weight/100)

sim1 <- responses[sim1,]
sim2 <- responses1[sim2,]

combined <- smartbind(responses, responses1)

saveRDS(combined, "raw_combined.RDS")

# Use Weights to Simulate & Stratify --------------------------------------
mean(pollfish1$`Year Of Birth`)
mean(pollfish2$`Year Of Birth`)
weighted.mean(pollfish1$`Year Of Birth`,pollfish1$Weight)
mean(sim1$`Year Of Birth`)
mean(sim2$`Year Of Birth`)

# Combine Similar Ballot Items ------------------------------------------
## Some questions were identical across surveys. Some were similar, but not
##  identical. For those that get combined, separate analysis per survey 
##  will also be performed.

# A-S Question 1
as_logic <- sim1[, colnames(sim1) %in% c("as_logic", colnames(profile))]
as_logic <- rbind(as_logic, sim2[, colnames(sim2) %in% c("as_logic", colnames(profile))])

# A-S Question 2 - nearly but not perfectly identical
as_group <- sim1[, colnames(sim1) %in% c("as_groups", colnames(profile))]
as_group <- rbind(as_group, sim2[, colnames(sim2) %in% c("as_groups", colnames(profile))])

# A-S Question 3
as_select_all <- sim1[, colnames(sim1) %in% c("is_true1", colnames(profile))]
as_select_all <- rbind(as_select_all, sim2[, colnames(sim2) %in% c("is_true1", colnames(profile))])
as_select_all <- rbind(as_select_all, setNames(sim1[, colnames(sim1) %in% 
                      c("is_true2", colnames(profile))],colnames(as_select_all)))
as_select_all <- rbind(as_select_all, setNames(sim2[, colnames(sim2) %in% 
                      c("is_true2", colnames(profile))],colnames(as_select_all)))
as_select_all <- rbind(as_select_all, setNames(sim1[, colnames(sim1) %in% 
                      c("is_true3", colnames(profile))],colnames(as_select_all)))
as_select_all <- rbind(as_select_all, setNames(sim2[, colnames(sim2) %in% 
                      c("is_true3", colnames(profile))],colnames(as_select_all)))
as_select_all <- rbind(as_select_all, setNames(sim1[, colnames(sim1) %in% 
                      c("is_true4", colnames(profile))],colnames(as_select_all)))
as_select_all <- rbind(as_select_all, setNames(sim2[, colnames(sim2) %in% 
                      c("is_true4", colnames(profile))],colnames(as_select_all)))

as_select_all$select_all_true <- as_select_all$is_true1
as_select_all$is_true1        <- NULL

# Book Name (not identical, but fine for internal use only)
title <- sim1[, colnames(sim1) %in% c("book_name", colnames(profile))]
title <- rbind(title, sim2[, colnames(sim2) %in% c("book_name", colnames(profile))])

# Tax
tax <- sim1[, colnames(sim1) %in% c("support_bill", colnames(profile))]
tax <- rbind(tax, sim2[, colnames(sim2) %in% c("support_bill", colnames(profile))])

dual_ballot_items <- data.frame(as1   = as_logic$as_logic,
                                as2   = as_group$as_groups,
                                as3   = as_select_all$select_all_true[1:20000],
                                as3b  = as_select_all$select_all_true[20001:40000],
                                as3c  = as_select_all$select_all_true[40001:60000],
                                as3d  = as_select_all$select_all_true[60001:80000],
                                title = title$book_name,
                                tax   = tax$support_bill,
                                round = c(rep(1,10000),rep(2,10000)))
dual_ballot_items <- cbind(dual_ballot_items, tax[, 2:ncol(tax)])

# Feature Engineering -----------------------------------------------------
# Checks
sapply(dual_ballot_items, function(x) sum(is.na(x)))
sapply(sim1, function(x) sum(is.na(x)))
sapply(sim2, function(x) sum(is.na(x)))
sapply(dual_ballot_items, function(x) sum(is.null(x)))
sapply(sim1, function(x) sum(is.null(x)))
sapply(sim2, function(x) sum(is.null(x)))

# AS1
ref <- c(unique(dual_ballot_items$as1)[1], 
         unique(dual_ballot_items$as1)[3]) # 1 type of truth = 1 
ref <- as.character(ref)
dual_ballot_items$encoded_as1 <- ifelse(as.character(dual_ballot_items$as1) 
                                         %in% ref, 1, 0)
sim1$encoded_as1 <- ifelse(sim1$as_logic %in% ref, 1, 0)
sim2$encoded_as1 <- ifelse(sim2$as_logic %in% ref, 1, 0)
table(dual_ballot_items$encoded_as1)
table(sim1$encoded_as1)
table(sim2$encoded_as1)

# AS2
ref <- dual_ballot_items$as2[2] # One truth as ref, for consistency
dual_ballot_items$encoded_as2 <- ifelse(dual_ballot_items$as2 == ref, 1, 0)
sim1$encoded_as2 <- ifelse(sim1$as_groups == sim1$as_groups[1], 1, 0)
sim2$encoded_as2 <- ifelse(sim2$as_groups == sim2$as_groups[1], 1, 0)

# AS3
dual_ballot_items$encoded_as3a <- is.na(dual_ballot_items$as3)
dual_ballot_items$encoded_as3b <- is.na(dual_ballot_items$as3b)
dual_ballot_items$encoded_as3c <- is.na(dual_ballot_items$as3c)
dual_ballot_items$encoded_as3d <- is.na(dual_ballot_items$as3d)

# Tax
dual_ballot_items$encoded_tax <- ifelse(dual_ballot_items$tax == names(table(dual_ballot_items$tax))[12], 
                                        NA, 
                                        dual_ballot_items$tax)
dual_ballot_items$encoded_tax <- ifelse(grepl("Yes -", dual_ballot_items$encoded_tax), 
                                        "Yes",
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(dual_ballot_items$encoded_tax == "No reason to change things", 
                                        "No",
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(dual_ballot_items$encoded_tax == "B but a is I birth right or someone willing choosing",
                                        NA,
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(dual_ballot_items$encoded_tax == "The question is inherintly biased", 
                                        NA,
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(dual_ballot_items$encoded_tax == "Free Market Economy", 
                                        NA,
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(dual_ballot_items$encoded_tax == "This is a stupid proposition", 
                                        NA,
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(dual_ballot_items$encoded_tax == "I do not pay income taxes and have no opinion.", 
                                        NA,
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(dual_ballot_items$encoded_tax == "Parents should be allowed to pass on there money to there children.", 
                                        "No", 
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(grepl("Believe in reducing expenditures", 
                                              dual_ballot_items$encoded_tax),
                                        NA, 
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(grepl("I think inheritance over a certain amount", 
                                              dual_ballot_items$encoded_tax),
                                        "No", 
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(grepl("It's not realistic to place that much", 
                                              dual_ballot_items$encoded_tax),
                                        NA, 
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(grepl("The government does a horrible job of utilizing", 
                                              dual_ballot_items$encoded_tax),
                                        NA, 
                                        dual_ballot_items$encoded_tax)
a1 <- "Since when is it a crime to work and be successful and enjoy your successes, And I am speaking for the normal Joe, not \" fake Money celebrities ( the Kardashians come immediately to mind  ) famous for never having done a constructive thing in their lives"
a2 <- "Inheritance is not an unfair advantage.  I and my family sacrifice now to save money to have something to pass along to the children and grandchildren.  Our ability to manage our finances and be responsible with our money will allow us to save for our futures.   Income tax is a more fair way to share the tax burden... the more you make the more you pay..."
a3 <- "Inheritance is money earned that someone paid taxes on - that money does NOT belong to the government.  It belongs to the people who earned it or to whoever they choose to give it too upon their deaths."
dual_ballot_items$encoded_tax <- ifelse(dual_ballot_items$encoded_tax == a1,
                                        "No", 
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(dual_ballot_items$encoded_tax == a2,
                                        "No", 
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(dual_ballot_items$encoded_tax == a3,
                                        "No", 
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(grepl("No - because", dual_ballot_items$encoded_tax),
                                        "No", 
                                        dual_ballot_items$encoded_tax)
dual_ballot_items$encoded_tax <- ifelse(dual_ballot_items$encoded_tax == "No",
                                        "No", 
                                        dual_ballot_items$encoded_tax)

# Tables ------------------------------------------------------------------
# AS1 ####
data_survey1 <- data.frame(
  Proportion = as.numeric(table(as_logic$as_logic[dual_ballot_items$round == 1])) / 
    sum(as.numeric(table(as_logic$as_logic[dual_ballot_items$round == 1]))),
  Answer = c("Survey 1 - Single Truth Statement", "Survey 1 - AS-leaning Statement")
)
expected_counts1 <- rep(sum(as.numeric(table(as_logic$as_logic[dual_ballot_items$round == 1])))/2, 2)
chi_square_test1 <- chisq.test(as.numeric(table(as_logic$as_logic[dual_ballot_items$round == 1]))/100, p = rep(1/2, 2))
data_survey1$ChiSquare <- round(chi_square_test1$statistic, 2)
data_survey1$pValue    <- round(chi_square_test1$p.value, 4)
data_survey1$Counts    <- as.numeric(table(as_logic$as_logic[dual_ballot_items$round == 1]))

# Calculate proportions and expected counts for Survey 2
data_survey2 <- data.frame(
  Proportion = as.numeric(table(as_logic$as_logic[dual_ballot_items$round == 2])) / 
    sum(as.numeric(table(as_logic$as_logic[dual_ballot_items$round == 2]))),
  Answer = c("Survey 2 - Single Truth Statement", "Survey 2 - AS-leaning Statement")
)
expected_counts1 <- rep(sum(as.numeric(table(as_logic$as_logic[dual_ballot_items$round == 2])))/2, 2)
chi_square_test1 <- chisq.test(as.numeric(table(as_logic$as_logic[dual_ballot_items$round == 2]))/100, p = rep(1/2, 2))
data_survey2$ChiSquare <- round(chi_square_test1$statistic, 2)
data_survey2$pValue    <- round(chi_square_test1$p.value, 4)
data_survey2$Counts    <- as.numeric(table(as_logic$as_logic[dual_ballot_items$round == 2]))

# Combine data and print table
data <- rbind(data_survey1, data_survey2)
data <- data[, c("Answer", "Counts", "Proportion", "ChiSquare", "pValue")]

names(data) <- c("Answer", "Counts", "Proportion", "Chi-square", "p Value")
kable(data, format = "html", table.attr = 'style="width:70%;"') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "left") %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c(" ", "Unadjusted", "Per Survey", "Per Survey", "Test")) %>%
  footnote(general = "Chi-square test for Survey 1 and Survey 2 calculated separately. Simulation by weighted oversampling was used for stratification. Simulated counts were adjusted downward with division by 100 when calculating Chi-square to reflect the power of the original results.",
           fixed_small_size = T)

# AS2  ####
data_survey1 <- data.frame(
  Proportion = as.numeric(table(as_group$as_group[dual_ballot_items$round == 1])) / 
    sum(as.numeric(table(as_group$as_group[dual_ballot_items$round == 1]))),
  Answer = c("Survey 1 - Single Truth Statement", "Survey 1 - AS-leaning Statement")
)
expected_counts1 <- rep(sum(as.numeric(table(as_group$as_group[dual_ballot_items$round == 1])))/2, 2)
chi_square_test1 <- chisq.test(as.numeric(table(as_group$as_group[dual_ballot_items$round == 1]))/100, p = rep(1/2, 2))
data_survey1$ChiSquare <- round(chi_square_test1$statistic, 2)
data_survey1$pValue    <- round(chi_square_test1$p.value, 4)
data_survey1$Counts    <- as.numeric(table(as_group$as_group[dual_ballot_items$round == 1]))

data_survey2 <- data.frame(
  Proportion = as.numeric(table(as_group$as_group[dual_ballot_items$round == 2])) / 
    sum(as.numeric(table(as_group$as_group[dual_ballot_items$round == 2]))),
  Answer = c("Survey 2 - AS-leaning Statement", "Survey 2 - Single Truth Statement")
)
expected_counts1 <- rep(sum(as.numeric(table(as_group$as_group[dual_ballot_items$round == 2])))/2, 2)
chi_square_test1 <- chisq.test(as.numeric(table(as_group$as_group[dual_ballot_items$round == 2]))/100, p = rep(1/2, 2))
data_survey2$ChiSquare <- round(chi_square_test1$statistic, 2)
data_survey2$pValue    <- round(chi_square_test1$p.value, 4)
data_survey2$Counts    <- as.numeric(table(as_group$as_group[dual_ballot_items$round == 2]))

data <- rbind(data_survey1, data_survey2)
data <- data[, c("Answer", "Counts", "Proportion", "ChiSquare", "pValue")]

names(data) <- c("Answer", "Counts", "Proportion", "Chi-square", "p Value")
kable(data, format = "html", table.attr = 'style="width:70%;"') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "left") %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c(" ", "Unadjusted", "Per Survey", "Per Survey", "Test")) %>%
  footnote(general = "Chi-square test for Survey 1 and Survey 2 calculated separately. Simulation by weighted oversampling was used for stratification. Simulated counts were adjusted downward with division by 100 when calculating Chi-square to reflect the power of the original results.",
           fixed_small_size = T)

# AS3 ####
# Reshape the data to long format
dual_ballot_items_long <- dual_ballot_items %>%
  dplyr::select(encoded_as3a, encoded_as3b, encoded_as3c, encoded_as3d) %>%
  pivot_longer(cols = everything(), names_to = "Statement", values_to = "Response") |>
  mutate(Statement = as.factor(Statement))

# Fit a generalized linear model (GLM)
glm_model <- glm(Response ~ Statement, 
                 data   = dual_ballot_items_long, 
                 family = binomial)

# Quick and dirty check of p-values without oversampling
quickcheck <- glm(Response ~ Statement, 
                 data   = sample_n(dual_ballot_items_long, 200, F), 
                 family = binomial)
summary(glm_model)
summary(quickcheck) # Prop. 2 (only) is significant

# Perform multiple comparisons using the multcomp package
glht_model <- glht(glm_model, linfct = mcp(Statement = "Tukey"))
cmpr_model <- glht(quickcheck, linfct = mcp(Statement = "Tukey"))
  
# Extract the results
comparison_results <- summary(glht_model)$test
summary(cmpr_model)$test$pvalues

# Prepare the results for display
results <- data.frame(
  Comparison = names(comparison_results$coefficients),
  Estimate   = round(comparison_results$coefficients, 4),
  StdError   = round(comparison_results$sigma, 4),
  zValue     = round(summary(cmpr_model)$test$tstat, 4),
  pValue     = round(summary(cmpr_model)$test$pvalues, 4)
)

# Adjust p-values using Bonferroni correction
results$pValueAdj <- p.adjust(results$pValue, method = "bonferroni")

# Display the data using kable
results$Comparison <- c("Proposition 2 - Proposition 1",
                        "Proposition 3 - Proposition 1",
                        "Proposition 4 - Proposition 1",
                        "Proposition 3 - Proposition 2",
                        "Proposition 4 - Proposition 2",
                        "Proposition 4 - Proposition 3")
rownames(results)  <- NULL
kable(results, format = "html", table.attr = 'style="width:70%;"') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "left") %>%
  column_spec(1, bold = TRUE) %>%
  #add_header_above(c(" ", "Estimate", "Std. Error", "z Value", "p Value", "Adjusted p Value")) %>%
  footnote(general = "Multiple comparison procedure for AS3 columns using Tukey's HSD test. p-values are adjusted using Bonferroni correction to account for multiple comparisons.",
           fixed_small_size = T)

# Create a plot for the Tukey HSD test results
tukey_results        <- TukeyHSD(aov(glm_model))
plot_data            <- as.data.frame(tukey_results$Statement)
plot_data$Comparison <- rownames(plot_data)

# Plot the results using ggplot2
tukey_results <- TukeyHSD(aov(glm_model))
plot_data     <- as.data.frame(tukey_results$Statement)
plot_data$Comparison <- results$Comparison
ggplot(plot_data, aes(x = Comparison, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Tukey HSD Test Results",
       x = "Comparison",
       y = "Difference in Means",
       caption = "Error bars represent 95% confidence intervals. Red dashed line indicates no difference.") +
  theme(plot.title = element_text(hjust = 0.5))

# Tax: Survey-level gap (question design difference) ####
table(dual_ballot_items$encoded_tax[dual_ballot_items$ID %in% sim1$ID])
table(dual_ballot_items$encoded_tax[dual_ballot_items$ID %in% sim2$ID])
table(dual_ballot_items$encoded_tax[dual_ballot_items$ID %in% sim2$ID])/sum(!is.na(dual_ballot_items$encoded_tax[dual_ballot_items$ID %in% sim2$ID]))

# Round 1
data_tax1 <- data.frame(
  Proportion = as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 1])) / 
    sum(as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 1]))),
  Answer = c("Survey 1 - Oppose", "Survey 1 - Favor")
)
expected_counts1 <- rep(sum(as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 1])))/2, 2)
chi_square_test1 <- chisq.test(as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 1]))/(400), p = rep(1/2, 2))
data_tax1$ChiSquare <- round(chi_square_test1$statistic, 2)
data_tax1$pValue    <- round(chi_square_test1$p.value, 4)
data_tax1$Counts    <- as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 1]))

# Round 2
data_tax2 <- data.frame(
  Proportion = as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 2])) / 
    sum(as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 2]))),
  Answer = c("Survey 2 - Oppose", "Survey 2 - Favor")
)
expected_counts2 <- rep(sum(as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 2])))/2, 2)
chi_square_test2 <- chisq.test(as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 2]))/400, p = rep(1/2, 2))
data_tax2$ChiSquare <- round(chi_square_test2$statistic, 2)
data_tax2$pValue    <- round(chi_square_test2$p.value, 4)
data_tax2$Counts    <- as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 2]))

data_tax <- rbind(data_tax1, data_tax2)
data_tax <- data_tax[, c("Answer", "Counts", "Proportion", "ChiSquare", "pValue")]

names(data_tax) <- c("Answer", "Counts", "Proportion", "Chi-square", "p Value")
kable(data_tax, format = "html", table.attr = 'style="width:70%;"') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "left") %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c(" ", "Unadjusted", "Per Survey", "Per Survey", "Test")) %>%
  footnote(general = "Chi-square test for Survey 1 and Survey 2 calculated separately. Simulation by weighted oversampling was used for stratification. Simulated counts were adjusted downward with division by 400 (100 for the original oversamply times 4 vertically stacked vectors of boolean values) when calculating Chi-square to reflect the power of the original sample.",
           fixed_small_size = T)

data_tax

# Prepare data for Survey 2
data_tax2 <- data.frame(
  Proportion = as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 2])) / 
    sum(as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 2]))),
  Answer = c("Oppose", "Favor"),
  Percent = gtsummary::style_percent(as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 2])) / 
                                       sum(as.numeric(table(dual_ballot_items$encoded_tax[dual_ballot_items$round == 2])))))

# Survey 2 Binary Encoded Pie Chart
pie_chart <- ggplot(data_tax2, aes(x = "", y = Proportion, fill = Answer)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Survey 2 Tax Question Responses",
       caption = "Proportions of responses to the tax question in Survey 2, post-stratification. Respondents were allowed to ") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  geom_text(aes(label = Percent), position = position_stack(vjust = 0.5))

print(pie_chart)

# Survey 2 Reasons for Opposition
# Tax Policy Explanations
dual_ballot_items$encoded_tax_reasons <- ifelse(dual_ballot_items$tax == names(table(dual_ballot_items$tax))[12], 
                                                NA, 
                                                dual_ballot_items$tax)
dual_ballot_items$encoded_tax_reasons <- ifelse(grepl("Yes -", dual_ballot_items$encoded_tax_reasons), 
                                                "Yes (conditional)",
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(dual_ballot_items$encoded_tax_reasons == "No reason to change things", 
                                                "Unclear",
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(dual_ballot_items$encoded_tax_reasons == "B but a is I birth right or someone willing choosing",
                                                "Misunderstood",
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(dual_ballot_items$encoded_tax_reasons == "The question is inherintly biased", 
                                                "Comment sans Preference or Reason",
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(dual_ballot_items$encoded_tax_reasons == "Free Market Economy", 
                                                "Misunderstood",
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(dual_ballot_items$encoded_tax_reasons == "This is a stupid proposition", 
                                                "Comment sans Preference or Reason",
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(dual_ballot_items$encoded_tax_reasons == "I do not pay income taxes and have no opinion.", 
                                                NA,
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(dual_ballot_items$encoded_tax_reasons == "Parents should be allowed to pass on there money to there children.", 
                                                "Freedom to Bequeath Money", 
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(grepl("Believe in reducing expenditures", 
                                                      dual_ballot_items$encoded_tax_reasons),
                                                "Misunderstood", 
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(grepl("I think inheritance over a certain amount", 
                                                      dual_ballot_items$encoded_tax_reasons),
                                                "Protective of Inheritance", 
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(grepl("It's not realistic to place that much", 
                                                      dual_ballot_items$encoded_tax_reasons),
                                                "Misunderstood", 
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(grepl("The government does a horrible job of utilizing", 
                                                      dual_ballot_items$encoded_tax_reasons),
                                                "Misunderstood", 
                                                dual_ballot_items$encoded_tax_reasons)
a1 <- "Since when is it a crime to work and be successful and enjoy your successes, And I am speaking for the normal Joe, not \" fake Money celebrities ( the Kardashians come immediately to mind  ) famous for never having done a constructive thing in their lives"
a2 <- "Inheritance is not an unfair advantage.  I and my family sacrifice now to save money to have something to pass along to the children and grandchildren.  Our ability to manage our finances and be responsible with our money will allow us to save for our futures.   Income tax is a more fair way to share the tax burden... the more you make the more you pay..."
a3 <- "Inheritance is money earned that someone paid taxes on - that money does NOT belong to the government.  It belongs to the people who earned it or to whoever they choose to give it too upon their deaths."
dual_ballot_items$encoded_tax_reasons <- ifelse(dual_ballot_items$encoded_tax_reasons == a1,
                                                sample(c("Freedom to Bequeath Money", "Misunderstood"), 1, prob = c(.75, .25), T), 
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(dual_ballot_items$encoded_tax_reasons == a2,
                                                "Protective of Inheritance", 
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(dual_ballot_items$encoded_tax_reasons == a3,
                                                sample(c("Freedom to Bequeath Money", "Protective of Inheritance", "Misunderstood"), 1, prob = c(.75/2, .75/2, .25), T), 
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(grepl("No - because", dual_ballot_items$encoded_tax_reasons),
                                                "Protective of Inheritance", 
                                                dual_ballot_items$encoded_tax_reasons)
dual_ballot_items$encoded_tax_reasons <- ifelse(dual_ballot_items$encoded_tax_reasons == "No",
                                                "Unclear", 
                                                dual_ballot_items$encoded_tax_reasons)
table_data   <- dual_ballot_items[!grepl("Yes", dual_ballot_items$encoded_tax_reasons) & dual_ballot_items$round == 2, "encoded_tax_reasons"]
table_data   <- table_data[!table_data %in% c(NA, "Unclear", "Misunderstood", "Comment sans Preference or Reason")]
t            <- table(table_data)
reason_table <- data.frame(t)

colnames(reason_table)  <- c("Reason", "Count")
reason_table$Proportion <- reason_table$Count/10000
reason_table$PropOfOpp  <- reason_table$Count/sum(reason_table$Count)
reason_table$StratEquiv <- reason_table$Proportion * 100
reason_table

pie_chart <- ggplot(reason_table, aes(x = "", y = Proportion, fill = Reason)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(title   = "Categorized Reasons for Opposition to Tax Policy",
       caption = "Categorized reasons for opposing the tax question, excluding reasons that indicated a misunderstanding of the question.") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "right"
  ) +
  geom_text(aes(label = round(PropOfOpp, 2)),
            position  = position_stack(vjust = 0.4))

print(pie_chart)

# Single-survey topics ####
table(sim2$ai_rights)/sum(table(sim2$ai_rights))
table(sim2$equal_op)/sum(table(sim2$equal_op))
table(sim2$fair_chood)/sum(table(sim2$fair_chood))
table(sim2$gov_truth)/sum(table(sim2$gov_truth))
table(sim2$gov_censor)/sum(table(sim2$gov_censor))
(33 + 19 + 16)/100
0.2032 + 0.2923 + 0.1766
table(sim1$privacy)/10000
(5649 + 3589)/(5649 + 3589 + 762)
table(sim2$simulation)
mean(sim1$inherit_received)
mean(sim1$inherit_expected)
mean(sim1$gifts_received)

plot(density(sim1$inherit_received), type = "l")
cont_vars <- data.frame(
  received = sim1$inherit_received,
  expected = sim1$inherit_expected,
  gifts = sim1$gifts_received
)


cont_vars <- data.frame(
  received = sim1$inherit_received,
  expected = sim1$inherit_expected,
  gifts = sim1$gifts_received
)

cont_vars_log <- log1p(cont_vars)  # log1p is log(1 + x) to handle zero values

density_received <- density(cont_vars_log$received)
density_expected <- density(cont_vars_log$expected)
density_gifts    <- density(cont_vars_log$gifts)

top_peaks <- function(density_obj, num_peaks) {
  peaks <- density_obj$x[order(-density_obj$y)]
  # Ensure peaks are spaced apart by at least 10% of the range
  peak_dist <- 0.1 * diff(range(density_obj$x))
  selected_peaks <- peaks[1]
  for (peak in peaks[-1]) {
    if (all(abs(peak - selected_peaks) > peak_dist)) {
      selected_peaks <- c(selected_peaks, peak)
      if (length(selected_peaks) == num_peaks) break
    }
  }
  return(expm1(selected_peaks))  # Convert back to original scale
}

peaks_received <- top_peaks(density_received, 3)
peaks_expected <- top_peaks(density_expected, 2)
peaks_gifts    <- top_peaks(density_gifts, 3)

max_values <- sapply(cont_vars, function(x) ifelse(length(x) == 0, NA, max(x, na.rm = TRUE)))
top_1_percent <- sapply(cont_vars, function(x) ifelse(length(x) == 0, NA, quantile(x, 0.99, na.rm = TRUE)))

format_dollar <- function(x) {
  if (is.na(x) || x == 0) {
    return("N/A")
  } else {
    return(comma(round(x, 2)))
  }
}

plot <- plot_ly() %>%
  add_lines(x = ~density_received$x, y = ~density_received$y, name = 'Inheritance Received', line = list(color = 'blue')) %>%
  add_lines(x = ~density_expected$x, y = ~density_expected$y, name = 'Inheritance Expected', line = list(color = 'red')) %>%
  add_lines(x = ~density_gifts$x, y = ~density_gifts$y, name = 'Gifts Received', line = list(color = 'green')) %>%
  add_annotations(x = log1p(peaks_received[1:2]), y = density_received$y[match(log1p(peaks_received[1:2]), density_received$x)], 
                  text = paste0('$', format_dollar(peaks_received[1:2])), showarrow = TRUE, arrowhead = 2, ax = 20, ay = -30, font = list(color = 'blue')) %>%
  add_annotations(x = log1p(peaks_expected), y = density_expected$y[match(log1p(peaks_expected), density_expected$x)], 
                  text = paste0('$', format_dollar(peaks_expected)), showarrow = TRUE, arrowhead = 2, ax = 20, ay = -30, font = list(color = 'red')) %>%
  add_annotations(x = log1p(peaks_gifts), y = density_gifts$y[match(log1p(peaks_gifts), density_gifts$x)], 
                  text = paste0('$', format_dollar(peaks_gifts)), showarrow = TRUE, arrowhead = 2, ax = 20, ay = -30, font = list(color = 'green')) %>%
  add_annotations(x = log1p(peaks_received[3]), y = density_received$y[match(log1p(peaks_received[3]), density_received$x)] - 0.001, 
                  text = paste0('$', format_dollar(peaks_received[3])), showarrow = TRUE, arrowhead = 2, ax = 20, ay = 30, font = list(color = 'blue')) %>%
  layout(
    title = 'Distribution of Continuous Variables (Log Transformed)',
    xaxis = list(title = 'Log Transformed Value'),
    yaxis = list(title = 'Probability Density'),
    legend = list(
      orientation = 'h',
      x = 0.5,
      xanchor = 'center',
      y = -0.1,
      font = list(size = 16)
    ),
    annotations = list(
      list(
        x = 0.95, y = 0.95, 
        xref = 'paper', yref = 'paper',
        text = paste("Max Values:<br>",
                     "Inheritance Received: $", format_dollar(max_values['received']), "<br>",
                     "Inheritance Expected: $", format_dollar(max_values['expected']), "<br>",
                     "Gifts Received: $", format_dollar(max_values['gifts']), "<br>",
                     "<br>Top 1% Values:<br>",
                     "Inheritance Received: $", format_dollar(top_1_percent['received']), "<br>",
                     "Inheritance Expected: $", format_dollar(top_1_percent['expected']), "<br>",
                     "Gifts Received: $", format_dollar(top_1_percent['gifts'])),
        showarrow = FALSE,
        bordercolor = "black",
        borderwidth = 1,
        borderpad = 4,
        bgcolor = "white",
        opacity = 0.8
      )
    )
  )

plot
#png("plots/fig_B4.png", width = 800, height = 600)
#dev.off()
#p_load(kaleido)
#save_image(plot, "plots/fig_B4.png")
#orca(plot, file = "plots/fig_B4.png")
p_load(htmlwidgets, webshot)
webshot::install_phantomjs()
htmlwidgets::saveWidget(as_widget(plot), "temp_plot.html")
webshot::webshot("temp_plot.html", file = "plots/fig_B4.png")
print(plot)

inc <- as.factor(sim1$Income)
levels(inc)
incnum <- ifelse(inc == "high_iii", 7, NA)
incnum <- ifelse(inc == "high_ii", 6, incnum)
incnum <- ifelse(inc == "high_i", 5, incnum)
incnum <- ifelse(inc == "middle_ii", 4, incnum)
incnum <- ifelse(inc == "middle_i", 3, incnum)
incnum <- ifelse(inc == "lower_ii", 2, incnum)
incnum <- ifelse(inc == "lower_i", 1, incnum)

cor.test(sim1$inherit_expected, incnum)
cor.test(sim1$inherit_received, incnum)
cor.test(sim1$gifts_received, incnum)

mean(sim1$inherit_expected[sim1$Income == "high_iii"])
mean(sim1$inherit_expected[sim1$Income == "high_ii"])
mean(sim1$inherit_expected[sim1$Income == "high_i"])
mean(sim1$inherit_expected[sim1$Income == "middle_ii"])
mean(sim1$inherit_expected[sim1$Income == "middle_i"])
mean(sim1$inherit_expected[sim1$Income == "lower_ii"])
mean(sim1$inherit_expected[sim1$Income == "lower_i"])

# AS4
length(unique(sim2$q_types_truth))

makeCloud <- function(char_vec) {
  # Text preprocessing
  corpus <- Corpus(VectorSource(char_vec))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, removeWords, c("truth"))
  corpus <- tm_map(corpus, stemDocument)
  
  dtm <- DocumentTermMatrix(corpus)
  mat <- as.matrix(dtm)
  
  word_freq <- colSums(mat)
  
  wordcloud(words = names(word_freq), freq = word_freq,
            scale = c(3, 0.5), random.color = T, #colors = brewer.pal(8, "RdYlGn"),
            min.freq = 2)
  
  # Assign correlation labels (1 or 0)
  correlation_vector <- grepl("one|1 truth|single|only", char_vec)
  return(correlation_vector)
}

char_df <- data.frame(char_vec  = sim2$q_types_truth)
myMap   <- data.frame(sentences = unique(sim2$q_types_truth),
                      many      = grepl("many", tolower(unique(sim2$q_types_truth))),
                      god       = grepl("god", tolower(unique(sim2$q_types_truth))),
                      labels    = c(
                        1,
                        1,
                        Inf,
                        2,
                        1,
                        NA,
                        3,
                        1,
                        1,
                        1,
                        Inf, # Number 11
                        3,
                        1,
                        3,
                        2,
                        1,
                        1,
                        1,
                        3,
                        2,
                        3, # Num 21.. Many = 3? I guess for now. 3 or Inf...
                        2,
                        1,
                        1,
                        3,
                        2,
                        1,
                        1,
                        3, # Num 29, many
                        1,
                        1,
                        1,
                        1,
                        3,
                        3, # multiple = 3 
                        2,
                        2,
                        2,
                        1,
                        1,
                        4,
                        3, # many, num 42
                        2,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        3, # many, num 52
                        Inf, # continuous spectrum
                        1,
                        1,
                        3, # 56, many
                        1,
                        1,
                        3,
                        1,
                        1,
                        0,
                        1,
                        Inf, # too many to explain, num 64
                        3,
                        1,
                        1,
                        1,
                        Inf, # always a truth for a specific person (Inf? 1? NA? probably Inf)
                        2,
                        3, # "Only few control by wealthy"
                        1,
                        3, # many, 73
                        1,
                        Inf,
                        Inf,
                        3,
                        1,
                        1,
                        1,
                        3, # many, 81
                        3, # it depends on the subject
                        3, # several, 83
                        2,
                        1,
                        3, # many, 86
                        1,
                        1,
                        2,
                        NA, # not sure
                        1,  # 91
                        NA, # "There are a lot of truths but the main one and most important is the real truth"  
                        1,  # 93
                        3,  # 94 plenty
                        1,
                        2,
                        1,
                        1,
                        1,
                        1
                      )
)

label_counts     <- table(myMap$labels)
poststrat_as4    <- merge(x = sim2,
                          y = myMap,
                          all.x = T,
                          by.x  = "q_types_truth",
                          by.y  = "sentences")

# Binning the labels properly for pre-stratification
pre_label_counts <- table(myMap$labels)
pre_label_counts <- c(pre_label_counts[names(pre_label_counts) < 3], `≥3` = sum(pre_label_counts[names(pre_label_counts) >= 3]))

post_label_counts <- table(poststrat_as4$labels)
post_label_counts <- c(post_label_counts[names(post_label_counts) < 3], `≥3` = sum(post_label_counts[names(post_label_counts) >= 3])) / 100

x_labels <- c("No Truth", "1 Type", "2 Types", "≥3 Types")
y_values_pre <- c(pre_label_counts["0"], pre_label_counts["1"], pre_label_counts["2"], pre_label_counts["≥3"])
y_values_post <- c(post_label_counts["0"], post_label_counts["1"], post_label_counts["2"], post_label_counts["≥3"])

pre_colors <- rep('#1f77b4', length(x_labels)) # Blue
post_colors <- rep('#d62728', length(x_labels)) # Red

p <- plot_ly() %>%
  add_bars(
    x = x_labels,
    y = y_values_pre,
    name = 'Pre-Stratification',
    marker = list(color = pre_colors),
    text = y_values_pre,
    textposition = 'outside',
    texttemplate = '%{text}'
  ) %>%
  add_bars(
    x = x_labels,
    y = y_values_post,
    name = 'Post-Stratification',
    marker = list(color = post_colors),
    text = y_values_post,
    textposition = 'outside',
    texttemplate = '%{text}'
  ) %>%
  layout(
    title = list(
      text = "Categorized Responses to `How many types of truth are there and why?`",
      font = list(family = 'Arial', size = 24, color = 'black'),
      x = 0.5,  # Center the title
      xanchor = 'center',
      y = 0.95,  # Slightly below the top
      yanchor = 'top'
    ),
    margin = list(t = 150, b = 50),  # Adjust top margin for more whitespace
    xaxis = list(
      title = "Types of Truth", 
      titlefont = list(family = 'Arial', size = 18, color = 'black'),
      categoryorder = "array",
      categoryarray = x_labels
    ),
    yaxis = list(title = "Count", titlefont = list(family = 'Arial', size = 18, color = 'black')),
    barmode = 'group',
    legend = list(orientation = 'h', y = -0.2, font = list(family = 'Arial', size = 14))
  )

# Save the plot as fig_B5.png
htmlwidgets::saveWidget(p, "temp_plot.html")
webshot::webshot("temp_plot.html", "plots/fig_B5.png")

p

# PSM ---------------------------------------------------------------------
# Selecting Survey 2 Respondents who Understood the Question
psm_data_pool <- sim2[!sim2$ID %in% dual_ballot_items$ID[dual_ballot_items$encoded_tax_reasons == "Misunderstood"] ,]

# "Treatment" vs. "Control" Group Indicator
quasi_treatment_indicator <- ifelse(psm_data_pool$`Employment Status` %in% c("employed_for_wages", "self_employed"), 1, 0)

# Other Control Variables
psm_data_pool$age2 <- 2024 - psm_data_pool$`Year Of Birth`
  
X <- c(
       "age2",
       "Education",
       "Income",
       "Marital Status",
       "Number of children",
       "Race"
       )

X <- psm_data_pool[,X]

X$Income    <- ifelse(grepl("high", X$Income), 1, 0)
X$Education <- ifelse(X$Education %in% c("university", "postgraduate"),
                      1, 0)
X$`Marital Status`     <- ifelse(X$`Marital Status` == "married", 1, 0)
X$`Number of children` <- ifelse(!X$`Number of children` == "zero", 1, 0)  
X$Race                 <- ifelse(!X$Race == "white", 1, 0)

# Outcome Variable
Y <- as.factor(ifelse(grepl("Yes", psm_data_pool$support_bill), 1, 0))

# First Stage (AutoML PS Model)
h2o.init()

stage1           <- cbind(factor(quasi_treatment_indicator, c(0,1),
                          c("Control", "Treatment")), X)
colnames(stage1) <- c("Tr", "x1", "x2", "x3", "x4", "x5", "x6")
head(stage1)

h2odata    <- as.h2o(stage1)
response   <- "Tr"
predictors <- setdiff(names(h2odata), response)
splits <- h2o.splitFrame(h2odata, ratios = 0.8, seed = 1234)
train  <- splits[[1]]
test   <- splits[[2]]

aml <- h2o.automl(y                = response,
                  x                = predictors,
                  training_frame   = train,
                  max_models       = 15,
                  seed             = 42,
                  max_runtime_secs = 100,
                  sort_metric      = "mean_per_class_error")

lb <- h2o.get_leaderboard(aml)
print(lb)

best_model <- aml@leader
print(best_model)

pred <- h2o.predict(best_model, test)
perf <- h2o.performance(best_model, newdata = test)
print(perf)

propensity_scores <- h2o.predict(best_model, h2odata)
propensity_scores <- as.data.frame(propensity_scores)
saveRDS(propensity_scores, "propensity_scores.RDS")

h2o.shutdown(prompt = FALSE)

effect <- Match(Y        = Y, 
                Tr       = quasi_treatment_indicator,
                X        = propensity_scores$Treatment,
                estimand = "ATT",
                M        = 3, 
                ties     = F,
                replace  = F,
                CommonSupport = T) # minimum reasonable
summary(effect)

# Check balance
post   <- data.frame(Tr = ifelse(stage1$Tr == "Treatment", T, F))   
formul <- as.formula("Tr ~ x1 + x2 + x4 + x5 + x6") # x3 had 0 var b/t Tr and Ctl

post <- cbind(post$Tr, stage1[,2:ncol(stage1)])

colnames(post) <- c("Tr", colnames(post)[2:ncol(post)])

balance <- MatchBalance(formul, 
                        data      =  as.data.frame(post), 
                        match.out = effect, 
                        nboots    = 50)

balance$BMsmallest.p.value
balance$AMsmallest.p.value # should be larger - and it is

