# #MULTIPLE LOGISTIC REGRESSION
# 
# ## Create a data frame of numeric variables
# ## Select only those variables that are numeric or can be made numeric
# library(psych)
# corr.test(Data.num, 
#           use = "pairwise",
#           method="spearman",
#           adjust="none",      # Can adjust p-values; see ?p.adjust for options
#           alpha=.05)
# 
# #MULTIPLE LOGISTIC REGRESSION
# ### Create new data frame with all missing values removed (NA???s)
# Data.omit = na.omit(Data)
# 
# ### Define full and null models and do step procedure
# model.null = glm(Complicaties ~ 1, 
#                  data=Data.omit,
#                  family = binomial(link="logit")
# )
# 
# model.full = glm(Complicaties ~ Comorbiditeiten + Medicatie + Leeftijd.moment.van.OK ,
#                  data=Data.omit,
#                  family = binomial(link="logit")
# )
# 
# model.null = glm(Succes ~ 1, 
#                  data=Data.omit,
#                  family = binomial(link="logit")
# )
# 
# model.full = glm(Succes ~ Comorbiditeiten + Medicatie + Leeftijd.moment.van.OK + geslacht,
#                  data=Data.omit,
#                  family = binomial(link="logit")
# )
# 
# step(model.null,
#      scope = list(upper=model.full),
#      direction="both",
#      test="Chisq",
#      data=Data
# )
# 
# ###Final model
# model.final = glm(Succes ~ Comorbiditeiten + Medicatie + Leeftijd.moment.van.OK + geslacht,
#                   data=Data,
#                   family = binomial(link="logit"),
#                   na.action(na.omit)
# )
# 
# summary(model.step1)
# summary(model.final)
# 
# ### Analysis of variance for individual terms
# 
# library(car)
# Anova(model.final, type="II", test="Wald")
# 
# ###Pseudo-R-squared
# source("http://rcompanion.org/r_script/nagelkerke.r")
# nagelkerke(model.final)
# 
# ### Create data frame with variables in final model and NA???s omitted
# 
# library(dplyr)
# Data.final = 
#   select(TVS,
#          geslacht, 
#          Leeftijd.moment.van.OK,
#          Succes,
#          Comorbiditeiten,
#          Roken,
#          Medicatie);
# 
# Data.final = na.omit(Data.final)
# 
# 
# ### Define null models and compare to final model
# model.null = glm(Succes ~ 1,
#                  data=Data.final,
#                  family = binomial(link="logit")
# )
# 
# anova(model.final, 
#       model.null, 
#       test="Chisq")
# 
# library(lmtest)
# lrtest(model.final)
# 
# plot(fitted(model.final), 
#      rstandard(model.final)
# )
# 
# ### Create data frame with variables in final model and NA???s omitted
# library(dplyr)
# Data.final = 
#   select(TVS,
#          geslacht, 
#          Leeftijd.moment.van.OK,
#          Succes,
#          Comorbiditeiten,
#          Roken,
#          Medicatie);
# 
# Data.final = na.omit(Data.final)
# 
# Data.final$predy = predict(model.final,
#                            type="response")
# 
# ### Plot
# 
# plot(Succes ~ predy, 
#      data = Data.final,
#      pch = 16,
#      xlab="Predicted probability of 1 response",
#      ylab="Actual response"
# )
# 
# ### Check for overdispersion
# ### Overdispersion is a situation where the residual deviance of the glm is large relative to the residual degrees of freedom.  These values are shown in the summary of the model.  One guideline is that if the ratio of the residual deviance to the residual degrees of freedom exceeds 1.5, then the model is overdispersed.  Overdispersion indicates that the model doesn???t fit the data well:  the explanatory variables may not well describe the dependent variable or the model may not be specified correctly for these data.  If there is overdispersion, one potential solution is to use the quasibinomial family option in glm.
# 
# summary(model.final)$deviance / summary(model.final)$df.residual
# 
# ### Create data frame with just final terms and no NA???s
# 
# library(dplyr)
# Data.final = 
#   select(TVS,
#          geslacht, 
#          Leeftijd.moment.van.OK,
#          Succes,
#          Comorbiditeiten,
#          Roken,
#          Medicatie);
# 
# Data.final = na.omit(Data.final)
# 
# ### Define models to compare.
# model.1=glm(Succes ~ 1, 
#             data=Data.omit, family=binomial())
# model.2=glm(Succes ~ Leeftijd.moment.van.OK, 
#             data=Data.omit, family=binomial())
# model.3=glm(Succes ~ Leeftijd.moment.van.OK + Medicatie, 
#             data=Data.omit, family=binomial())
# model.4=glm(Succes ~ Leeftijd.moment.van.OK + Medicatie + Comorbiditeiten, 
#             data=Data.omit, family=binomial())
# 
# vif(model.3)
# ### Use compare.glm to assess fit statistics.
# 
# # source("http://rcompanion.org/r_script/compare.glm.r") -> zit nu in rcompanion
# 
# compare.glm(model.1, model.2, model.3, model.4)
# 
# ### Use anova to compare each model to the previous one.
# 
# anova(model.1, model.2, model.3,model.4, test="Chisq")
# 