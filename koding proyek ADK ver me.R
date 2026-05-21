library(foreign)
alldata <- read.spss(file = "C:/Users/riza4/Downloads/WVS_Cross-National_Wave_7_spss_v6_0.sav", as.data.frame=TRUE)
alldata <- data.frame(alldata)
str(alldata)
levels(alldata$B_COUNTRY)
IND <- subset(alldata, B_COUNTRY == "Indonesia")
library(writexl)
write_xlsx(IND, path = "C:/Users/riza4/Downloads/F00013191-WVS_Wave_7_Indonesia_Excel_v5.1.xlsx")
library(readxl)
dat <- read_xlsx(path = "C:/Users/riza4/Downloads/F00013191-WVS_Wave_7_Indonesia_Excel_v5.1.xlsx", sheet = 1)
head(dat)
df <- subset(dat, select = c("Q50","Q288","Q275","Q262","Q47","Q273"))
df[df == "Don't know"] <- NA
df[df == "Missing; Unknown"] <- NA
df[df == "No answer"] <- NA
sapply(df, function(x) any(is.na(x)))
str(df)
lapply(df, table)

library(dplyr)
library(summarytools)

# Financial (dari Q50 → kategori ordinal)
df$Financial <- as.numeric(factor(df$Q50,
                                  levels = c("Dissatisfied","2","3","4","5","6","7","8","9","Satisfied")))

df <- df %>%
  mutate(Financial_cat = case_when(
    Financial %in% c(1,2) ~ "Sangat tidak puas",
    Financial %in% c(3,4) ~ "Tidak puas",
    Financial %in% c(5,6) ~ "Agak puas",
    Financial %in% c(7,8) ~ "Puas",
    Financial %in% c(9,10) ~ "Sangat puas",
    TRUE ~ NA_character_))

df$Financial_cat <- factor(df$Financial_cat,
                           levels = c("Sangat tidak puas","Tidak puas","Agak puas","Puas","Sangat puas"),
                           ordered = TRUE)

# Age
df$Age <- as.numeric(df$Q262)

# Health (gabung dari awal biar aman)
df <- df %>%
  mutate(Health2 = case_when(
    Q47 %in% c("Very poor","Poor") ~ "Poor",
    Q47 == "Fair" ~ "Fair",
    Q47 %in% c("Good","Very good") ~ "Good",
    TRUE ~ NA_character_))
df$Health2 <- factor(df$Health2, levels = c("Poor","Fair","Good"), ordered = TRUE)


# Marital (gabung dari awal)
df <- df %>%
  mutate(Marital2 = case_when(
    Q273 == "Single" ~ "Single",
    Q273 %in% c("Married","Living together as married") ~ "Married",
    Q273 %in% c("Divorced","Separated","Widowed") ~ "Ever married",
    TRUE ~ NA_character_))
df$Marital2 <- factor(df$Marital2)


# Education
df <- df %>%
  mutate(Educ = case_when(
    Q275 %in% c("Early childhood education (ISCED 0) / no education",
                "Primary education (ISCED 1)",
                "Lower secondary education (ISCED 2)") ~ "Low",
    Q275 == "Upper secondary education (ISCED 3)" ~ "Middle",
    Q275 %in% c("Short-cycle tertiary education (ISCED 5)",
                "Bachelor or equivalent (ISCED 6)",
                "Master or equivalent (ISCED 7)",
                "Doctoral or equivalent (ISCED 8)") ~ "High",
    TRUE ~ NA_character_))
df$Educ <- factor(df$Educ, levels = c("Low","Middle","High"))


# Income
df <- df %>%
  mutate(HIncome = case_when(
    Q288 %in% c("Lower step","Second step","Third step") ~ "Low",
    Q288 %in% c("Fourth step","Fifth step","Sixth step","Seventh step") ~ "Middle",
    Q288 %in% c("Eight step","Nineth step","Tenth step") ~ "High",
    TRUE ~ NA_character_))

df$HIncome <- factor(df$HIncome, levels = c("Low","Middle","High"))

df$Marital2 <- relevel(df$Marital2, ref = "Married")
df$Educ <- relevel(df$Educ, ref = "Low")
df$HIncome <- relevel(df$HIncome, ref = "Low")
df2 <- df %>%
  select(Financial_cat, Age, Marital2, Educ, HIncome, Health2)

df3 <- na.omit(df2)
sapply(df2, function(x) sum(is.na(x)))
dim(df3)
dfSummary(df3)

ct_marital <- xtabs(~Marital2 + Financial_cat, data=df3)
ct_educ    <- xtabs(~Educ + Financial_cat, data=df3)
ct_income  <- xtabs(~HIncome + Financial_cat, data=df3)
ct_health  <- xtabs(~Health2 + Financial_cat, data=df3)

freq_result <- list(
  marital = list(
    round(prop.table(ct_marital, 2)*100,2),
    round(chisq.test(ct_marital)$statistic,2),
    chisq.test(ct_marital)$p.value),
  
  educ = list(
    round(prop.table(ct_educ, 2)*100,2),
    round(chisq.test(ct_educ)$statistic,2),
    chisq.test(ct_educ)$p.value),
  
  income = list(
    round(prop.table(ct_income, 2)*100,2),
    round(chisq.test(ct_income)$statistic,2),
    chisq.test(ct_income)$p.value),
  
  health = list(
    round(prop.table(ct_health, 2)*100,2),
    round(chisq.test(ct_health)$statistic,2),
    chisq.test(ct_health)$p.value)
)

freq_result

num.var <- df3 %>%
  select(where(is.numeric), Financial_cat) %>%
  group_by(Financial_cat) %>%
  summarise(across(where(is.numeric),
                   list(mean = mean, sd = sd),
                   .names = "{.col}_{.fn}")) %>%
  as.data.frame()
num.var

library(ggplot2)

#BOXPLOT (Age vs Financial)
ggplot(df3, aes(x = Financial_cat, y = Age, fill = Financial_cat)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  geom_jitter(width = 0.2, alpha = 0.05, color = "darkblue") +
  labs(title = "Umur vs Kepuasan Finansial",
       x = "Tingkat Kepuasan Finansial",
       y = "Umur") +
  theme_bw() +
  theme(legend.position = "none")

library(ggplot2)

# BARPLOT - EDUCATION
ggplot(df3, aes(x = Financial_cat, fill = Educ)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Kepuasan Finansial berdasarkan Pendidikan",
       x = "Kepuasan Finansial",
       fill = "Pendidikan") +
  theme_bw()

# BARPLOT - INCOME
ggplot(df3, aes(x = Financial_cat, fill = HIncome)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Kepuasan Finansial berdasarkan Pendapatan",
       x = "Kepuasan Finansial",
       fill = "Pendapatan") +
  theme_bw()


# BARPLOT - HEALTH
ggplot(df3, aes(x = Financial_cat, fill = Health2)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Kepuasan Finansial berdasarkan Kesehatan",
       x = "Kepuasan Finansial",
       fill = "Kesehatan") +
  theme_bw()


# BARPLOT + FACET
ggplot(df3, aes(x = Financial_cat, fill = Educ)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  facet_grid(HIncome ~ Marital2) +
  labs(title = "Kepuasan Finansial (Income & Marital)",
       x = "Kepuasan Finansial",
       fill = "Pendidikan") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(VGAM)
mod <- vglm(Financial_cat ~ Age + Marital2 + Educ + HIncome + Health2,
            family = cumulative(parallel = TRUE),
            data = df3)
summary(mod)

library(VGAM)


mod_full <- vglm(Financial_cat ~ Age + Marital2 + Educ + HIncome + Health2,
                 family = cumulative(parallel = TRUE),
                 data = df3)
mod_null<-vglm(Financial_cat ~ 1,
               family = cumulative(parallel = TRUE),
               data = df3)

mod_noAge <- vglm(Financial_cat ~ Marital2 + Educ + HIncome + Health2,
                  family = cumulative(parallel = TRUE), data = df3)


mod_noMarital <- vglm(Financial_cat ~ Age + Educ + HIncome + Health2,
                      family = cumulative(parallel = TRUE), data = df3)


mod_noEduc <- vglm(Financial_cat ~ Age + Marital2 + HIncome + Health2,
                   family = cumulative(parallel = TRUE), data = df3)


mod_noIncome <- vglm(Financial_cat ~ Age + Marital2 + Educ + Health2,
                     family = cumulative(parallel = TRUE), data = df3)


mod_noHealth <- vglm(Financial_cat ~ Age + Marital2 + Educ + HIncome,
                     family = cumulative(parallel = TRUE), data = df3)


lrt_test <- function(mod_full, mod_reduced, varname) {
  lr_stat <- 2 * (logLik(mod_full) - logLik(mod_reduced))
  df_diff <- df.residual(mod_reduced) - df.residual(mod_full)
  p_val   <- pchisq(lr_stat, df = df_diff, lower.tail = FALSE)
  cat(sprintf("LM Test - %s: Chi2 = %.4f, df = %d, p-value = %.4f\n",
              varname, lr_stat, df_diff, p_val))
}
lrt_test(mod_full, mod_null,  "Full")
lrt_test(mod_full, mod_noAge,     "Age")
lrt_test(mod_full, mod_noMarital, "Marital2")
lrt_test(mod_full, mod_noEduc,    "Educ")
lrt_test(mod_full, mod_noIncome,  "HIncome")
lrt_test(mod_full, mod_noHealth,  "Health2")


lrt_table <- function(mod_full, models_list) {
  results <- lapply(names(models_list), function(nm) {
    m <- models_list[[nm]]
    lr   <- 2 * (logLik(mod_full) - logLik(m))
    ddf  <- df.residual(m) - df.residual(mod_full)
    pval <- pchisq(lr, df = ddf, lower.tail = FALSE)
    data.frame(Variabel = nm,
               Chi2     = round(lr, 3),
               df       = ddf,
               p_value  = round(pval, 4),
               Signif   = ifelse(pval < 0.001, "***",
                                 ifelse(pval < 0.01,  "**",
                                        ifelse(pval < 0.05,  "*",
                                               ifelse(pval < 0.1,   ".", "ns")))))
  })
  do.call(rbind, results)
}

models_list <- list(
  Full    = mod_null,
  Age     = mod_noAge,
  Marital2 = mod_noMarital,
  Educ    = mod_noEduc,
  HIncome = mod_noIncome,
  Health2 = mod_noHealth
)

lrt_summary <- lrt_table(mod_full, models_list)
print(lrt_summary, row.names = FALSE)
anova(mod)
exp(coef(mod))
odds_ratios <- data.frame(
  Variabel = names(coef(mod)),
  OR = exp(coef(mod)),
  SE = sqrt(diag(vcov(mod))),
  CI_lower = exp(coef(mod) - 1.96 * sqrt(diag(vcov(mod)))),
  CI_upper = exp(coef(mod) + 1.96 * sqrt(diag(vcov(mod)))))
round(odds_ratios[,-1], 3)
levels(alldata$Q50)
colSums(is.na(df3))






#coba
library(bayesm)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(hexbin)
library(ggdist)
library(colorspace) 
library(tidyverse)
library(cowplot)
library(colorspace)
library(ggrepel)
library(ggstatsplot)
library(colorspace)
library(cowplot)

library(ggdist)
library(hrbrthemes)

ggplot(df3, aes(x = Financial_cat, y = Age, fill = Financial_cat, color = Financial_cat)) +
  stat_halfeye(adjust = 0.5, width = 0.6, .width = 0,
               justification = -.2, alpha = 0.7, point_colour = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA,
               fill = "white", linewidth = 0.8, color = "gray30") +
  geom_jitter(width = 0.05, alpha = 0.25, size = 1.2) + 
  scale_fill_brewer(palette = "RdYlGn") +
  scale_color_brewer(palette = "RdYlGn") +  
  theme_ipsum(base_size = 13, grid = "Y") +
  labs(title = "Distribusi Usia per Kategori Kepuasan Finansial",
       x = "Kepuasan Finansial", y = "Usia") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9))  
library(ggstatsplot)

ggbetweenstats(
  data = df3,
  x = HIncome,
  y = Age,
  type = "nonparametric",
  plot.type = "boxviolin",
  results.subtitle = TRUE,
  package = "ggsci",
  palette = "nrc_npg"
) +
  labs(x = "Tingkat Pendapatan",
       y = "Usia",
       title = "Perbandingan Usia berdasarkan Kategori Pendapatan")

