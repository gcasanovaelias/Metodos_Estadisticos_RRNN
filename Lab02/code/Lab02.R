
# Packages ----------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(multilevelmod)
library(broom.mixed)
library(patchwork)

library(magrittr)
require(readxl)
require(car)
require(performance)

tidymodels_prefer()


# Data --------------------------------------------------------------------

(data <- read_xlsx("Lab02/data/arbolesInventario.xlsx") %>% 
   mutate(rodal = as_factor(rodal),
          parcela = as_factor(parcela),
          arbol = as_factor(arbol)))

# Selección de 30 rodales
set.seed(1)
(selection <- data %>% filter(!parcela %in% c(1:5)) %>% distinct(rodal) %>% slice_sample(n = 30) %>% pull())

(data_selected <- data %>% filter(rodal %in% selection))

# Splitting data with rsample

set.seed(1)
(data_split <- initial_split(
  data = data_selected,
  prop = 3/4,
  strata = dap
))

(data_train <- training(data_split))
(data_test <- testing(data_split))


glimpse(data_train)

summary(data_train)

attach(data_train)


# Exploratory Data Analysis (EDA) -----------------------------------------

# Boxplot
qplot(x = rodal, y = altura, geom = "boxplot")

# Scatter plot
qplot(x = dap, y = altura, color = rodal, geom = "point")

# Histogram
qplot(x = altura)


# ANOVA
aov(dap ~ rodal) %>% summary()

# Correlation
cor.test(x = altura, y = dap, method = "pearson")


# Estadigrafos
data_selected %>% summarise(across(
  .cols = c(altura, dap),
  .fns = list(
    mean = mean,
    median = median,
    var = var,
    sd = sd
  )
))


# Nested modelation -------------------------------------------------------

theme_set(theme_bw())

(
  model_nest <- data_train %>%
    uncount(weights = 9, .id = "ID") %>%
    group_by(ID) %>%
    nest() %>%
    ungroup() %>%
    mutate(
      ID = as_factor(LETTERS[1:9]),
      formula = {
        list(
          "I(log(altura)) ~ I(1/sqrt(dap))",
          "I(log(altura)) ~ I(1/sqrt(dap)) + (1|rodal)",
          "I(log(altura)) ~ I(1/sqrt(dap)) + (0 + dap|rodal)",
          "I(log(altura)) ~ I(1/sqrt(dap)) + (dap|rodal)",
          "I(log(altura)) ~ I(1/sqrt(dap)) + (1|parcela)",
          "I(log(altura)) ~ I(1/sqrt(dap)) + (0 + dap|parcela)",
          "I(log(altura)) ~ I(1/sqrt(dap)) + (dap|parcela)",
          "I(log(altura)) ~ I(1/sqrt(dap)) + (1|rodal) + (0 + dap|parcela)",
          "I(log(altura)) ~ I(1/sqrt(dap)) + (0 + dap|rodal) + (1|parcela)"
        )
      },
      formula = map(.x = formula,
                    .f = ~ as.formula(.x)),
      engine = c("lm", rep("lmer", 8)) %>% as.list(),
      model = {
        map2(
          .x = engine,
          .y = formula,
          .f = ~ linear_reg(engine = .x) %>% fit(formula = .y, data = data_train)
        )
      },
      .tidy = map(.x = model,
                  .f = ~ tidy(.x)),
      .glance = map(.x = model,
                    .f = ~ glance(.x)),
      .predict = {
        map(
          .x = model,
          .f = ~ data_test %>% select(altura) %>% mutate(altura = log(altura)) %>% bind_cols(predict(
            object = .x, new_data = data_test
          )) %>% mutate(.resid = altura - .pred)
        )
      },
      AIC = map_dbl(.x = .glance,
                    .f = ~ pull(.x, AIC)),
      bias = map_dbl(.x = .predict,
                     .f = ~ pull(.x, .resid) %>% mean() %>% multiply_by(-1)),
      mae = map_dbl(
        .x = .predict,
        .f = ~ pull(.x, .resid) %>% abs() %>% mean()
      ),
      rmse = map_dbl(.x = .predict,
                     .f = ~ pull(.x, .resid) %>% sd()),
      resid_pred = {
        map2(
          .x = model,
          .y = ID,
          .f = ~ qplot(
            x = .x %>% extract_fit_engine() %>% fitted(),
            y = .x %>% extract_fit_engine() %>% resid(),
            geom = "point",
            point = 1,
            colour = I("royalblue2"),
            alpha = I(0.4),
            main = paste("Residuos vs Predichos - model", .y),
            xlab = "Fitted values",
            ylab = "Residuals"
          )
        )
      },
      qq = {
        map2(
          .x = model,
          .y = ID,
          .f = ~ .x %>% extract_fit_engine() %>% resid() %>% as_tibble() %>% rename(.resid = value) %>% ggplot(aes(sample = .resid)) + stat_qq(shape = 1, color = "firebrick4") + stat_qq_line() + labs(
            title = paste("Normal Q-Q Plot - model", .y),
            x = "Theoretical Quantiles",
            y = "Sample Quantiles"
          )
        )
      },
      graphics = {
        map2(.x = resid_pred,
             .y = qq,
             .f = ~ .y + .x)
      }
    ) %>%
    select(-c(resid_pred, qq))
)

model_nest %>% pluck("graphics")

# ICC
(model_nest <- model_nest %>% filter(ID != "A") %>% mutate( 
  icc = map_dbl(
    .x = model, 
    .f = ~ extract_fit_engine(.x) %>% icc() %>% pluck("ICC_adjusted")
  )
) %>% bind_rows(
  model_nest %>% filter(ID == "A") %>% mutate(icc = NA)
) %>% arrange(ID))

# Comparación de modelos
(model_comp <- anova(
  model_nest %>% pluck("model", 2, "fit"),
  model_nest %>% pluck("model", 3, "fit"),
  model_nest %>% pluck("model", 4, "fit"),
  model_nest %>% pluck("model", 5, "fit"),
  model_nest %>% pluck("model", 6, "fit"),
  model_nest %>% pluck("model", 7, "fit"),
  model_nest %>% pluck("model", 8, "fit"),
  model_nest %>% pluck("model", 9, "fit")
))


a <- model_nest %>% pluck("model", 3) %>% extract_fit_engine()

a %>% summary() %>% pull(Variance)

a %>% glimpse()

a %>% performance::icc() %>% str()

model_nest %>% pluck("model", 1) %>% extract_fit_engine() %>% icc() %>% pluck("ICC_adjusted")


model_nest %>% 
  mutate(
    icc = case_when(
      ID == "B" ~ model %>% extract_fit_engine() %>% icc() %>% pluck("ICC_adjusted")
    )
  )

model_nest %>% 
  mutate(
    icc = map_dbl(
      .x = model,
      .f = case_when(
        ID != "A" ~ .x %>% extract_fit_engine() %>% icc() %>% pluck("ICC_adjusted")
      )
    )
  )



(v_mixed <- a %>% summary() %>% pluck("vcov", "factors", "correlation", "x", 2) %>% raise_to_power(2))

(v_error <- a %>% glance() %>% pluck("sigma") %>% raise_to_power(2))

v_mixed/(v_mixed + v_error)

model_nest %>% pluck("model", 3) %>% extract_fit_engine() %>% icc()


