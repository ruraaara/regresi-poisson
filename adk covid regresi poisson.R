covid <- read.csv(file = "C:/Users/riza4/Downloads/full_data_4192020.csv", header = TRUE)

data <- covid[covid$location == "Indonesia", ]
str(data)

data$waktu <- 1:nrow(data)

hist(data$new_deaths, main = "Histogram Jumlah Kematian", xlab = "Kematian Harian")

fit <- glm(new_deaths ~ waktu, family = poisson(link = log), data = data)
summary(fit)

fit0 <- glm(new_deaths ~ 1, family = poisson(link = log), data = data)

library(lmtest)
lrtest(fit0, fit)

pred_link <- predict(fit, type = "link", se.fit = TRUE)
z95       <- qnorm(0.975)

alpha  <- 0.05
n      <- length(data$new_deaths)
mu.hat <- mean(data$new_deaths)

CI <- mu.hat + qnorm(c(alpha/2, 1 - alpha/2)) * sqrt(mu.hat / n)
CI


mu_hat   <- exp(pred_link$fit)
CI_lower <- exp(pred_link$fit - z95 * pred_link$se.fit)
CI_upper <- exp(pred_link$fit + z95 * pred_link$se.fit)

hasil_CI <- data.frame(
  Tanggal  = data$date,
  Waktu    = data$waktu,
  mu_hat   = round(mu_hat, 4),
  CI_Bawah = round(CI_lower, 4),
  CI_Atas  = round(CI_upper, 4)
)
print(hasil_CI)

library(ggplot2)
library(scales)

df_fit <- data.frame(
  waktu      = data$waktu,
  new_deaths = data$new_deaths,
  mu_hat     = mu_hat,
  CI_lower   = CI_lower,
  CI_upper   = CI_upper
)

ggplot(df_fit, aes(x = waktu)) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper),
              fill = "#FFA500", alpha = 0.25) +
  # Titik DULU (di bawah garis)
  geom_point(aes(y = new_deaths, fill = new_deaths),
             shape = 21, color = "white",
             size = 2.5, stroke = 0.5) +
  # Garis Poisson SETELAH (di atas titik)
  geom_line(aes(y = mu_hat), color = "#C0392B", linewidth = 1.2) +
  scale_fill_gradientn(
    colours = c("#2ECC71", "#F1C40F", "#E67E22", "#C0392B"),
    name    = "Kematian\nHarian"
  ) +
  labs(
    title    = "Regresi Poisson COVID-19 Indonesia",
    subtitle = "Data kematian harian: 31 Des 2019 – 18 Apr 2020",
    x        = "Waktu (hari ke-)",
    y        = "Jumlah Kematian Harian",
    caption  = "Sumber: Our World in Data | full_data_4192020.csv"
  ) +
  annotate("segment", x = 5, xend = 18, y = 52, yend = 52,
           color = "#C0392B", linewidth = 1.2) +
  annotate("segment", x = 5, xend = 18, y = 46, yend = 46,
           color = "#FFA500", linewidth = 4, alpha = 0.4) +
  annotate("point", x = 11.5, y = 40, shape = 21,
           fill = "#E67E22", color = "white", size = 2.5) +
  annotate("text", x = 20, y = 52, label = "Kurva Poisson", hjust = 0, size = 3.5) +
  annotate("text", x = 20, y = 46, label = "CI Wald 95%",   hjust = 0, size = 3.5) +
  annotate("text", x = 20, y = 40, label = "Data Teramati", hjust = 0, size = 3.5) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "gray50", size = 10),
    plot.caption     = element_text(color = "gray60", size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    legend.position  = "right"
  )


new_data <- data.frame(waktu = 104:111)

pred_new  <- predict(fit, newdata = new_data, type = "link", se.fit = TRUE)
mu_pred   <- exp(pred_new$fit)
CI_low_pr <- exp(pred_new$fit - z95 * pred_new$se.fit)
CI_up_pr  <- exp(pred_new$fit + z95 * pred_new$se.fit)

hasil_pred <- data.frame(
  Tanggal  = seq(as.Date("2020-04-19"), as.Date("2020-04-26"), by = "day"),
  Waktu    = 104:111,
  Prediksi = round(mu_pred, 2),
  CI_Bawah = round(CI_low_pr, 2),
  CI_Atas  = round(CI_up_pr, 2)
)
print(hasil_pred)

