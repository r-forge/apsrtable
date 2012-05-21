
setGeneric("modelInfo", function(x) standardGeneric("modelInfo") )


modelInfo.summary.lm <- function(x) {
  env <- sys.parent()
  digits <- evalq(digits, envir=env)
  model.info <- list(
                     "$N$"=formatC(sum(x$df[1:2]),format="d"),
                     "$R^2$"=formatC(x$r.squared,format="f",digits=digits),
                     "adj. $R^2$"=formatC(x$adj.r.squared,format="f",digits=digits),
                     "Resid. sd" = formatC(x$sigma,format="f",digits=digits))
  class(model.info) <- "model.info"
  invisible(model.info)
}

modelInfo.summary.glm <- function(x) {
  env <- sys.parent()
  digits <- evalq(digits, envir=env)
  model.info <- list(
                       "$N$"=formatC(sum(x$df[1:2]),format="d"),

                       AIC=formatC(x$aic,format="f",digits=digits),
                       BIC= formatC(
                         ( (x$aic - 2*(length(x$coef)) ) +
                             log(sum(x$df[1:2]))*
                             length(coef(x)) ),
                         format="f",digits=digits),
                       "$\\log L$"=formatC( ((x$aic - 2*(length(x$coef))) / -2),
                         format="f",digits=digits))
  class(model.info) <- "model.info"
  invisible(model.info)
}



## 2009-02-25 mjm
## modelInfo request from Antonio Ramos for AER Tobit function
## Should be similar for 'survreg' objects, but without (necessarily)
## censoring info..
"modelInfo.summary.tobit" <- function(x) {
 env <- sys.parent()
 digits <- evalq(digits, envir=env)
 model.info <- list(
                    "Total $N$"=formatC(as.integer(x$n[1]),format="d"),
                    "Censored $N$"=formatC(sum(x$n[c(2,4)]),format="d"),
                    "$\\log L$" =
formatC(x$loglik[2],format="f",digits=digits),
                    "p(Wald)"=formatC(pchisq(x$wald,
                      sum(x$df) - x$idf,
                      lower.tail = FALSE),
                      digits=digits,format="f")
                    )
 class(model.info) <- "model.info"
 return(model.info)
}
"modelInfo.summary.gee" <- function(x) {
 env <- sys.parent()
 digits <- evalq(digits, envir=env)
 model.info <- list(" " = ""
                    )
 class(model.info) <- "model.info"
 return(model.info)
}

"modelInfo.summary.coxph" <- function (x) {
       env <- sys.parent()
       digits <- evalq(digits, envir=env)
       model.info <- list()
       model.info[["$N$"]] <- x$n
       pv <- formatC(x$waldtest["pvalue"], format="f", digits=digits)
       rsq <- formatC(x$rsq["rsq"], format="f", digits=digits)
       maxrsq <- formatC(x$rsq["maxrsq"], format="f", digits=digits)
       model.info$Wald <- sprintf("%.0f on %.0f df, p = %s",
                                  x$waldtest["test"], x$waldtest["df"],
                                  pv)
       model.info[["$R^2$"]] <- sprintf("%s (Max %s)", rsq, maxrsq)
       class(model.info) <- "model.info"
       invisible(model.info)
}

modelInfo.summary.polr <- function(x) {
    env <- sys.parent()
    digits<- evalq(digits, envir=env)

    model.info <- list(N= x$n,
                       AIC=format(x$deviance + 2 * x$edf, nsmall = 2L))
    class(model.info) <- "model.info"
    invisible(model.info)
}


"modelInfo.summary.lrm" <- function(x) {
  env <- sys.parent()
  digits<- evalq(digits, envir=env)
  x <- as.numeric(x$modelinfo)
  ##         number of observations
  ##         used in the fit, maximum absolute value of first derivative
  ##         of log likelihood, model likelihood ratio chi-square, d.f.,
  ##         P-value, c index (area under ROC curve), Somers' D_{xy},
  ##         Goodman-Kruskal gamma, Kendall's tau-a rank correlations
  ##         between predicted probabilities and observed response, the
  ##         Nagelkerke R^2 index, and the Brier score
  model.info <-
      list(
           "$N$"=formatC(x[1],format="d"),
           "Max.Deriv."=formatC(x[2],format="f",digits=digits),
           "LR $\\chi^2$"=formatC(x[3],format="f",digits=digits),
           "d.f."=formatC(x[4],format="d"),
           "$P$"=formatC(x[5],format="f",digits=digits),
           "C-index"=formatC(x[6],format="f",digits=digits),
           "Somers $D_{xy}$"=formatC(x[7],format="f",digits=digits),
           ##"$\\gamma$"=formatC(x[8],format="f",digits=digits),
           ##"Kendall's tau-a"=formatC(x[9],format="f",digits=digits),
           "Nagelkerke $R^2$"=formatC(x[10],format="f",digits=digits),
           "Brier" = formatC(x[11],format="f",digits=digits))
  class(model.info) <- "model.info"
  invisible(model.info)
}


setMethod("modelInfo", "summary.lm", modelInfo.summary.lm )
setMethod("modelInfo","summary.glm", modelInfo.summary.glm )
setMethod("modelInfo","summary.tobit", modelInfo.summary.tobit)
setMethod("modelInfo","summary.gee",modelInfo.summary.gee)
setMethod("modelInfo","summary.coxph",modelInfo.summary.coxph)
setMethod("modelInfo","summary.negbin",modelInfo.summary.glm)
setMethod("modelInfo", "summary.lrm", modelInfo.summary.lrm )
setMethod("modelInfo", "summary.svyglm",
          apsrtable:::modelInfo.summary.glm )
setMethod("modelInfo","summary.polr", modelInfo.summary.polr)
