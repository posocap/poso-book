# PCCFT.R
# Note: the chapter number in this file's name no longer corresponds to the book, due to reorganization during the editing and publishing processes.

# Libraries and Options ---------------------------------------------------
if (!require(pacman)) { install.packages("pacman"); library(pacman) }  
p_load(data.table, 
       futile.logger, 
       ggplot2, 
       psidR, 
       RCurl, 
       readxl, 
       usincometaxes, 
       yaml)

setwd("economics") # If needed to make economics the working directory
source("myPSID.R") # Fixes psidR connection issue and RDA loading bug
express <- T # Only true if using saved data

# Raw Data ----------------------------------------------------------------
if (!dir.exists("data/PSID")) {
  dir.create("data/PSID", recursive = T)
}

# Total Personal Income Taxes Data from the IRS
if(!express) {
  download.file("https://www.irs.gov/pub/irs-soi/21in41ts.xls",
                "data/taxes.xls")
}

total <- read_excel("data/taxes.xls", 
                     col_names = FALSE, 
                     skip      = 95,
                     n_max     = 21)
if(!express) {
  saveRDS(total, "data/raw_personal_income_taxes_2001_2021.RDS")
}

# Estate Tax Data from Tax Policy Center
if(!express) {
  url <- "https://www.taxpolicycenter.org/file/188633/download?token=Qgb2DmtG"
  download.file(url, "data/estate.xlsx")
}

estate <- read_excel("data/estate.xlsx", 
                  col_names = FALSE, 
                  skip      = 8,
                  n_max     = 66)

# 2021 Panel Study of Income Dynamics (PSID)
if(!express) {
  credentials <- yaml::read_yaml("config.yaml", readLines.warn = F)
  username    <- credentials$psid$user
  password    <- credentials$psid$pass
} else {
  username <- NULL
  password <- NULL
}

wealth_var_code            = "ER81838"
retirement_assets_var_code = "ER79342"
death_year_var_code        = "ER32050"

fam_vars_2021 <- data.frame(
  year = 2021,
  variable = c(wealth_var_code, retirement_assets_var_code),
  name     = c("net_wealth", "retirement_assets")
)

ind_vars_2021 <- data.frame(
  year     = 2021,
  variable = c(death_year_var_code),
  name     = c("death_year")
)

fam_vars_2021 <- dcast(data.table(fam_vars_2021), 
                       year ~ name, value.var = "variable")
ind_vars_2021 <- dcast(data.table(ind_vars_2021), 
                       year ~ name, value.var = "variable")

if(!express) {
  psid <- get_data(datadir  = "data/PSID", 
                   fam.vars = fam_vars_2021, 
                   ind.vars = ind_vars_2021, 
                   sample   = "SRC", 
                   design   = "all",
                   user     = username,
                   pass     = password)
  
  saveRDS(psid, file = "data/PSID/psid_2021_selected_vars.RDS")
} else {
  readRDS("data/PSID/psid_2021_selected_vars.RDS")
}

# Data Cleaning -----------------------------------------------------------
# Total Personal Income Taxes
total <- total$...2
taxes <- data.frame(year  = seq(2001, 2021), 
                    taxes = (total * 1000),
                    taxes_billions = total/1000000)

if(!express) {
  saveRDS(taxes, "data/clean_personal_income_taxes_2001_2021.RDS")
}

# Estate Taxes (Tax Policy Center)
cn <- c("year",
        "total_adult_deaths",
        "taxed_adult_deaths",
        "pct_taxed",
        "gross_millions",
        "estate_tax",
        "effective_tax_rate"
)
colnames(estate) <- cn

clean_taxes <- function(x) {
  x <- gsub("$", "", x)
  x <- gsub("%", "", x)
  x <- as.numeric(x)
  return(x)
}

estate[estate$year == "2010", 3:7] <- "0" # 2010 had no estate taxes
estate <- as.data.frame(apply(estate, 2, clean_taxes))

estate$gross_billions <- estate$gross_millions / 1000
estate$gross_dollars  <- estate$gross_millions * 1000000

# PSID
## Select reference individual (head of household)
psid.ref <- psid[psid$relation.head == 10,]

## Add Retirement Assets
### Following Cooper, Dynan, and Rhodenhiser (2019) & Gibson-Davis et al. (2022)
estate_values_min <- psid.ref$retirement_assets + psid.ref$net_wealth
estate_values_max <- ifelse(estate_values_min > 0, 
                            estate_values_min,
                            0)

summary(estate_values_min)
summary(estate_values_max)

if(!express) {
  saveRDS(estate_values_min, "data/estate_values_min.RDS")
  saveRDS(estate_values_max, "data/estate_values_max.RDS")
  write.csv(estate, "estate_plot_data.csv", row.names = F)
}

# Visualizations ----------------------------------------------------------
# Estate Taxes
plot(estate$year, estate$gross_billions, xlab = "year", ylab = "Billions ($)",
     type = "b")

p <- ggplot(estate, aes(x = year, y = gross_billions)) +
  geom_line(color  = "blue", size = 1.2) +
  geom_point(color = "red",  size = 2) +
  geom_text(data = subset(estate, year > 1974),
            aes(label = paste0("$", round(gross_billions), "B")), 
            vjust = -1, size = 2.5, hjust = 1.25, 
            color = "black", fontface = "bold") +
  labs(title = "Estate Taxes Over Time",
       x     = "Year",
       y     = "Gross Billions ($)") +
  theme_minimal() +
  theme(plot.title       = element_text(hjust = 0.5),
        legend.position  = "none",
        axis.text.x      = element_text(angle = 45, hjust = 1),
        axis.line        = element_line(size = 1, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(min(estate$year), max(estate$year), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(estate$gross_billions[!is.na(estate$gross_billions)]), by = 25))

print(p)


# Estimate Potential Tax Revenue ------------------------------------------
est_estate_val_trillions <- function(estate_values_min, 
                                     estate_values_max, 
                                     type = "range",
                                     num_estates = NA) {

  num_estates <- ifelse(is.na(num_estates),
                       estate$total_adult_deaths[estate$year == max(estate$year)],
                       num_estates)  
  mean_val_min <- mean(estate_values_min)
  mean_val_max <- mean(estate_values_max)
  
  num1 <- (mean_val_min * num_estates)/1000000000000
  num2 <- (mean_val_max * num_estates)/1000000000000
  
  value <- ifelse(type == "point", 
                  mean(c(num1, num2)), 
                  list(c(num1, num2))) 

  return(value)
}

# 2021 Number of Estates Estimates
num_estates_method1 <- estate$total_adult_deaths[estate$year == max(estate$year)]
num_estates_method2 <- 3458697 - 19724 - 3773 - 5955 - 38234
num_estates_method3 <- mean(c(num_estates_method1, 
                              num_estates_method2))

# Adjusted (only) Point Estimates  
est_estate_val_trillions(estate_values_max, # Both are 'max' here
                         estate_values_max, 
                         type = "point",
                         num_estates = num_estates_method1)
est_estate_val_trillions(estate_values_max, 
                         estate_values_max, 
                         type = "point",
                         num_estates = num_estates_method2)
est_estate_val_trillions(estate_values_max, 
                         estate_values_max, 
                         type = "point",
                         num_estates = num_estates_method3)

# Mean of Adjusted and Unadjusted
est_estate_val_trillions(estate_values_min, # Includes naively unadjusted 'min' here
                         estate_values_max, 
                         type = "point",
                         num_estates = num_estates_method1)
est_estate_val_trillions(estate_values_min, 
                         estate_values_max, 
                         type = "point",
                         num_estates = num_estates_method2)
est_estate_val_trillions(estate_values_min, 
                         estate_values_max, 
                         type = "point",
                         num_estates = num_estates_method3)

covid_adj <- num_estates_method2 - 460513 - 155 + 63 + 175 + 1634
covid_adj

est_estate_val_trillions(estate_values_max, 
                         estate_values_max, 
                         type = "point",
                         num_estates = covid_adj)

