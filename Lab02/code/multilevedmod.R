# Packages ----------------------------------------------------------------

library(lme4)
library(nlme)
library(tidyverse)
library(tidymodels)

library(multilevelmod)

tidymodels_prefer()

require(readxl)

# Data --------------------------------------------------------------------

(data <- read_xlsx("Lab02/data/arbolesInventario.xlsx") %>% 
   mutate(rodal = as_factor(rodal),
          parcela = as_factor(parcela),
          arbol = as_factor(arbol)))

glimpse(data)

summary(data)

attach(data)

# Selección de 30 rodales -------------------------------------------------

set.seed(2)
(select_rodal <- data %>% slice_sample(n = 30, replace = F) %>% pull(rodal))

(data_selected <- data %>% filter(rodal %in% select_rodal))

# ¿Cuántas parcelas tiene cada rodal escogido?
data_selected %>% group_by(rodal) %>% distinct(parcela) %>% summarise(n_parcelas = n())


# Modeling ----------------------------------------------------------------

# Splitting data with rsample

(data_split <- initial_split(
  data = data_selected,
  prop = 3/4,
  strata = dap
))

(data_train <- training(data_split))
(data_test <- testing(data_split))


# Workflow (parsnip + recipes + workflows)

(wf <- workflow(
  # parnsip
  spec = linear_reg(
    engine = "lm"
    ),
  # recipes
  preprocessor = recipe(
    formula = altura ~ dap,
    data = data_train
    ) %>%
    step_log(altura) %>%
    step_sqrt(dap) %>%
    step_inverse(dap)
))


# Resamples (Cross-validation)

(model_fit <- wf %>% fit_resamples(
  resamples = vfold_cv(data = data_train,
                       v = 10,
                       repeats = 10),
  metrics = metric_set(rmse, rsq, mae)
))

# Obtain the average of each of the k = 10 fit metrics evaluated on the k holdout

model_fit %>% collect_metrics()


# Fit the last model to the entire training set and evaluate on the test set

(fit_last <- last_fit(
  object = workflow_a,
  split = data_split,
  metrics = metric_set(rmse, rsq, mae)
))

# Show metrics evaluated on the testing set

fit_last %>% 
  collect_metrics()



#############

linear_reg() %>%
  set_engine("lme", random = ~ 1 | rodal) %>%
  fit(I(log(altura)) ~ I(1 / sqrt(dap)),
      data = data_train)

linear_reg(engine = "lmer") %>%
  fit(I(log(altura)) ~ I(1 / sqrt(dap)) + (1 | rodal),
      data = data_train)


linear_reg(engine = "lmer") %>%
  fit(I(log(altura)) ~ I(1 / sqrt(dap)),
      data = data_train)

# Intento 1: add_variables()
(workflow_c <- workflow() %>% 
  add_variables(
    outcomes = altura,
    predictors = c(rodal, parcela, arbol, dap)
  ) %>% 
  add_model(
    spec = linear_reg(engine = "lmer"),
    formula = I(log(altura)) ~ I(1 / sqrt(dap)) + (dap|rodal)
  ))

workflow_b %>% 
  fit(
    data = data_train
  )



# intento2: add_recipe()

workflow() %>% 
  add_recipe(
    recipe = recipe(altura ~ dap + rodal + arbol, data = data_train)
  ) %>% 
  add_model(
    spec = linear_reg(engine = "lmer"),
    formula = I(log(altura)) ~ I(1 / sqrt(dap)) + (1 | rodal)
  ) %>% 
  fit(
    data = data_train
  )
  
  fit_resamples(
    resample = cv_folds,
    metrics = metric_set(rmse, rsq, mae)
  ) %>% 
  collect_metrics()


# workflowsets ------------------------------------------------------------

  (all_workflows <- workflow_set(
    preproc = list(
      workflow_variables(
        outcomes = altura,
        predictors = c(rodal, parcela, arbol, dap)
      )
    ),
    models = list(
      "A" = linear_reg() %>% set_engine("lmer"))
  ))

# Express ----------------------------------------------------------------

(model_a <- linear_reg(
  engine = "lm"
) %>% 
  fit(
    I(log(altura)) ~ I(1/sqrt(dap)),
    data = data_train
  ))

(model_b <- linear_reg(
  engine = "lmer"
) %>% 
    fit(
      I(log(altura)) ~ I(1/sqrt(dap)) + (1|rodal),
      data = data_selected
    ))

(model_c <- linear_reg(
  engine = "lme"
) %>% 
    fit(
      I(log(altura)) ~ I(1/sqrt(dap)),
      random = ~ dap|rodal,
      data = data_selected
    ))
