library(MASS)
## Create vectors for outcome and predictors
outcome <- c("sudi")
predictors <- c("yod","bw","dhb","dep","sex","eth")
dataset    <- sudit
## The lines below should not need modification.
## Create list of models
list.of.models <- lapply(seq_along((predictors)), function(n) {
  left.hand.side  <- outcome
  right.hand.side <- apply(X = combn(predictors, n), MARGIN = 2, paste, collapse = " + ")
  paste(left.hand.side, right.hand.side, sep = "  ~  ")
})
## Convert to a vector
vector.of.models <- unlist(list.of.models)
## Fit logit glm to all models
list.of.fits <- lapply(vector.of.models, function(x) {
  formula    <- as.formula(x)
  fit        <- glm(formula, data = dataset, family = "binomial")
  result.AIC <- extractAIC(fit)
  coef.fit   <- summary(fit)
  data.frame(num.predictors = result.AIC[1],
             AIC            = result.AIC[2],
             model          = x)
})
## Collapse to a data frame
result <- do.call(rbind, list.of.fits)
## Sort and print
library(doBy)
orderBy(~ AIC, result)
write.csv(result, "model results.csv")
rm(list.of.fits,list.of.models,offs,outcome,predictors,vector.of.models, dataset)
