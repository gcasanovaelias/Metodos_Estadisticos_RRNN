# Packages ----------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(readxl)

tidymodels_prefer(quiet = F)

# Data --------------------------------------------------------------------

(data_full <- read_excel("Lab01/data/CrecimientoPR.xlsx"))

# Randomly selection of rows

set.seed(1)
(data_sub <- slice_sample(.data = data_full, n = 1000))

glimpse(data_sub)

summary(data_sub)

attach(data_sub)


# Exploratory Data Analysis -----------------------------------------------

theme_set(theme_bw())

(data_tidy_nest <- {
  data_sub %>%
    pivot_longer(cols = -num,
                 names_to = "IV",
                 values_to = "Values") %>%
    full_join(select(.data = data_sub, num, IAPD)) %>%
    mutate(
      # Delete repeated data
      IAPD = {
      case_when(IV == "IAPD" ~ NA_real_,
                TRUE ~ IAPD)
    },
      IV = as_factor(IV)
    ) %>%
    # Nest by IV
    group_by(IV) %>% nest() %>%
    # EDA columns
    mutate(
      .mean = {
        map_dbl(.x = data,
                .f = ~ .x %>% pull(Values) %>% mean())
      },
      .median = {
        map_dbl(.x = data,
                .f = ~ .x %>% pull(Values) %>% median())
      },
      .sd = {
        map_dbl(.x = data,
                .f = ~ .x %>% pull(Values) %>% sd())
      },
      .VC = .sd / .mean,
      Dist. = {
        map2(
          .x = data,
          .y = IV,
          .f = ~ qplot(
            data = .x,
            x = Values,
            geom = "freqpoly",
            main = paste("Distribution of", as.character(IV)),
            xlab = as.character(IV)
          )
        )
      },
      Box = {
        map2(
          .x = data,
          .y = IV,
          .f = ~ qplot(
            data = .x,
            y = Values,
            geom = "boxplot",
            main = paste("Boxplot of", as.character(IV)),
            ylab = as.character(IV)
          )
        )
      },
      Scatter = {
        map2(
          .x = data,
          .y = IV,
          .f = ~ qplot(
            data = .x,
            x = Values,
            y = IAPD,
            main = paste("Scatter plot of", as.character(IV), "vs IAPD"),
            xlab = as.character(IV)
          ) + stat_smooth(method = "lm",
                          se = F)
        )
      },
    )
})

data_tidy_nest %>% pluck("Dist.",19)


# Fitting -----------------------------------------------------------------

(
  model_nest <- data_sub %>%
    uncount(weights = 2, .id = "ID") %>%
    group_by(ID) %>%
    nest() %>%
    ungroup() %>%
    mutate(
      formula = {
        list(
          "I(log(IAPG + 0.1)) ~ I(log(DAP)) + I(log(BAL + 0.1)) + I(log(H200_D))",
          "IAPG ~ DAP + BAL + GHA_D + EDAD"
        )
      },
      model = {
        map2(
          .x = data,
          .y = formula,
          .f = ~ lm(formula = as.formula(.y), data = .x)
        )
      },
      .tidy = {
        map(.x = model,
            .f = ~ tidy(.x))
      },
      .glance = {
        map(.x = model,
            .f = ~ glance(.x))
      },
      .augment = {
        map2(.x = model,
             .y = data,
             .f = ~ augment(.x))
      },
      R2 = {map_dbl(
        .x = .glance,
        .f = ~ pull(.data = .x, r.squared)
      )},
      RMSE = {map_dbl(
        .x = .augment,
        .f = ~ pull(.data = .x, .resid) %>% sd()
      )},
      MAE = {map_dbl(
        .x = .augment,
        .f = ~ pull(.data = .x, .resid) %>% abs() %>% mean()
      )},
      gg_a = {map2(
        .x = .augment,
        .y = ID,
        .f = ~ qplot(data = .x, x = .fitted, y = .resid, main = paste("Modelo", .y, "- resid vs pred"))
      )},
      gg_b = {map2(
        .x = .augment,
        .y = ID,
        .f = ~ qplot(data = .x, x = DAP, y = .resid, main = paste("Modelo", .y, "- resid vs DAP"))
      )},
      gg_e = {map2(
        .x = .augment,
        .y = ID,
        .f = ~ ggplot(data = .x, aes(sample = IAPD)) + stat_qq() + stat_qq_line() + labs(title = paste("Modelo", .y, "- QQ"))
      )}
    )
)

nls(
  formula = IAPG ~ a*(DAP^b)*(BAL^c)*(H200_D^d),
  data = data_sub,
  start = c(a = 0.1, b = 0.1, c = .1, d = .1)
)


############
model_nest %>% pluck("model", 1) %>% plot()

model_nest %>% pluck(".augment", 1) %>% ggplot(aes(x = `I(log(IAPG + 0.1))`, y = .hat)) + geom_point()

# Sesgo
model_nest %>% pluck(".augment", 1, ".resid") %>% sum()/1000
model_nest %>% pluck(".augment", 1, ".resid") %>% mean()

# Variabilidad
model_nest %>% pluck(".augment", 1, ".resid") %>% var()


model_nest %>% pluck("model", 1, "fit") %>% plot()

# Se escoge el segundo modelo ya que, en relación al primero, este explica un mayor porcentaje de la variabiliad de IAPD (mayor R2) además de presentar una menor variabilidad en la distribución de los errores (menor RMSE) y una mayor precisión (menor MAE).


# Diagnósticos de regresión -----------------------------------------------

model_nest %>% pluck("model", 2) %>% outlierTest()

model_nest %>% pluck("model", 1) %>% lm.influence() %>% pluck("hat")

plot()

# Supuestos ---------------------------------------------------------------

# Normalidad de residuos
model_nest %>% pluck("model", 1, "fit", "residuals") %>% shapiro.test() # p-value < 0.05. Hay suficiente evidencia estadística para rechazar H0: distribución de errores normales.

model_nest %>% pluck("model", 2, "fit", "residuals") %>% shapiro.test() # p-value < 0.05. Hay suficiente evidencia estadística para rechazar H0: distribución de errores normales.

# Homocedasticidad
model_nest %>% pluck("model", 2) %>% ncvTest()

lm(I(log(IAPG + 0.1)) ~ I(log(DAP)) + I(log(BAL + 0.1)) + I(log(H200_D)), data = data_sub) %>% ncvTest()

lm(IAPG ~ DAP + BAL + GHA_D + EDAD, data = data_sub) %>% ncvTest()

# Multicolinealidad
lm(I(log(IAPG + 0.1)) ~ I(log(DAP)) + I(log(BAL + 0.1)) + I(log(H200_D)), data = data_sub) %>% vif() > 5
lm(I(log(IAPG + 0.1)) ~ I(log(DAP)) + I(log(BAL + 0.1)) + I(log(H200_D)), data = data_sub) %>% vif() %>% sqrt() > 2
lm(I(log(IAPG + 0.1)) ~ I(log(DAP)) + I(log(BAL + 0.1)) + I(log(H200_D)), data = data_sub) %>% vif() %>% mean() > 1

lm(IAPG ~ DAP + BAL + GHA_D + EDAD, data = data_sub) %>% vif() > 5
lm(IAPG ~ DAP + BAL + GHA_D + EDAD, data = data_sub) %>% vif() %>% sqrt() > 2
lm(IAPG ~ DAP + BAL + GHA_D + EDAD, data = data_sub) %>% vif() %>% mean() > 1

cor(data_sub$DAP, data_sub$DOM)

# No independencia de errores (autocorrelación)
lm(I(log(IAPG + 0.1)) ~ I(log(DAP)) + I(log(BAL + 0.1)) + I(log(H200_D)), data = data_sub) %>% durbinWatsonTest()
lm(IAPG ~ DAP + BAL + GHA_D + EDAD, data = data_sub) %>% durbinWatsonTest()

