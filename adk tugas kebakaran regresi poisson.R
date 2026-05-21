library(readxl)
library(ggplot2)
library(scales)
library(lmtest)
library(GGally)
library(gridExtra)
library(ggdist)   
library(hrbrthemes)

kebakaran <- read_xlsx("C:/Users/riza4/Downloads/kebakaran.xlsx")

p1 <- ggplot(kebakaran, aes(x = suhu, y = jumlah_kebakaran)) +
  geom_point(aes(fill = jumlah_kebakaran), shape = 21, color = "white",
             size = 2.5, stroke = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#C0392B", linewidth = 1) +
  scale_fill_gradientn(
    colours = c("#2ECC71", "#F1C40F", "#E67E22", "#C0392B")
  ) +
  labs(title = "Suhu vs Kebakaran", x = "Suhu (°C)", y = "Jumlah Kebakaran") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "none"
  )

p2 <- ggplot(kebakaran, aes(x = curah_hujan, y = jumlah_kebakaran)) +
  geom_point(aes(fill = jumlah_kebakaran), shape = 21, color = "white",
             size = 2.5, stroke = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#2980B9", linewidth = 1) +
  scale_fill_gradientn(
    colours = c("#2ECC71", "#F1C40F", "#E67E22", "#C0392B")
  ) +
  labs(title = "Curah Hujan vs Kebakaran", x = "Curah Hujan (mm)", y = "Jumlah Kebakaran") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "none"
  )

p3 <- ggplot(kebakaran, aes(x = kecepatan_angin, y = jumlah_kebakaran)) +
  geom_point(aes(fill = jumlah_kebakaran), shape = 21, color = "white",
             size = 2.5, stroke = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#8E44AD", linewidth = 1) +
  scale_fill_gradientn(
    colours = c("#2ECC71", "#F1C40F", "#E67E22", "#C0392B")
  ) +
  labs(title = "Kecepatan Angin vs Kebakaran", x = "Kecepatan Angin (km/jam)", y = "Jumlah Kebakaran") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "none"
  )

p4 <- ggplot(kebakaran, aes(x = jumlah_kebakaran)) +
  geom_histogram(fill = "#3498DB", color = "white", bins = 15, alpha = 0.8) +
  labs(title = "Distribusi Jumlah Kebakaran", x = "Jumlah Kebakaran", y = "Frekuensi") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  ) 


grid.arrange(p1, p2, p3, p4, ncol = 2, top = "Visualisasi Data Kebakaran Hutan")


library(readxl)
library(lmtest)
library(car)

kebakaran <- read_xlsx("C:/Users/riza4/Downloads/kebakaran.xlsx")

fit  <- glm(jumlah_kebakaran ~ suhu + curah_hujan + kecepatan_angin,
            family = poisson(link = "log"), data = kebakaran)
fit0 <- glm(jumlah_kebakaran ~ 1,
            family = poisson(link = "log"), data = kebakaran)
fit_tanpa_suhu  <- glm(jumlah_kebakaran ~ curah_hujan + kecepatan_angin,
                       family = poisson(link = "log"), data = kebakaran)
fit_tanpa_hujan <- glm(jumlah_kebakaran ~ suhu + kecepatan_angin,
                       family = poisson(link = "log"), data = kebakaran)

#B
summary(fit)

#C
IRR <- exp(coef(fit))
CI  <- exp(confint(fit))
cbind(IRR, CI)
lrtest(fit0, fit)
Anova(fit, type = "II", test.statistic = "LR")

#D
lrtest(fit_tanpa_suhu, fit)
exp(coef(fit)["suhu"])
exp(confint(fit)["suhu",])

#E
lrtest(fit_tanpa_hujan, fit)
exp(coef(fit)["curah_hujan"])
exp(confint(fit)["curah_hujan",])
(1 - exp(10 * coef(fit)["curah_hujan"])) * 100
deviance(fit) / df.residual(fit)






