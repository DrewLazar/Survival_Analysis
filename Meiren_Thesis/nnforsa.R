library(survivalmodels)

install.packages(c("ggplot2", "mlr3benchmark", "mlr3pipelines", "mlr3proba", "mlr3tuning", 
                   "survivalmodels"))
remotes::install_github("mlr-org/mlr3extralearners")


library(survivalmodels)

install_pycox(pip = TRUE, install_torch = TRUE)
install_keras(pip = TRUE, install_tensorflow = TRUE)