

##' Model fit and diagnostic functions for output
##' 
##' Model diagnostic / summary information to be included in apsrtable output.
##' 
##' Returns a list containing model diagnostic information, with an interface
##' described here to allow the user to change the information returned and
##' thus presented. The method is called by \code{apsrtable} within an
##' \code{lapply} on a list of model summaries. The modelInfo methods for a
##' given model summary object simply return a list of arbitrary name-value
##' pairs and give themselves the S3 class \code{modelInfo}. The modelInfo
##' method dispach uses formal S4 classes, however.
##' 
##' The example shows how one can change the summary for \code{lm} objects to
##' include only the \eqn{N} and residual \eqn{\sigma}.
##' 
##' If you register a \code{modelInfo} method and it appears not to work, try
##' calling \code{\link[methods]{setOldClass}} in order to register new
##' \code{modelInfo} methods for your model summary object. Method dispatch in
##' R has some subtleties.
##' 
##' @aliases modelInfo modelInfo,summary.lm-method modelInfo,summary.glm-method
##' modelInfo,summary.svyglm-method modelInfo,summary.tobit-method
##' modelInfo,summary.gee-method modelInfo,summary.coxph-method
##' modelInfo,summary.clogit-method modelInfo,summary.negbin-method
##' modelInfo,summary.lrm-method
##' @param x A \code{summary} object.
##' @return A list of named character objects representing the lines of model
##' diagnostic information to be included for a given class of model. For
##' example, the default for \code{\link[stats]{lm}} reports the \eqn{N, R^2},
##' adjusted \eqn{R^2}, and residual \eqn{\sigma}. The default for
##' \code{\link[stats]{glm}} includes the \eqn{N}, AIC, BIC, and
##' log-likelihood. Common names across model classes in the same table --
##' e.g., the \eqn{N} -- are matched by name, exactly like the model
##' coefficients (indeed, the same functions aggregate terms and order across
##' models.)
##' @author Michael Malecki <malecki at wustl.edu>
##' @seealso \link[base]{sys.frame}
##' @examples
##'  
##' 
##' setMethod("modelInfo", "summary.lm", function(x) {
##'   env <- sys.parent()
##'   digits <- evalq(digits, env)
##'   model.info <- list(
##'                      "$N$"=formatC(sum(x$df[1:2]),format="d"),
##'                      "Resid. sd" = formatC(x$sigma,format="f",digits=digits))
##'   class(model.info) <- "model.info"
##'   return(model.info)
##' } )
##' 
##' example(apsrtable)
##' 
##' 
##' ### Switch back to the default
##' setMethod("modelInfo", "summary.lm", apsrtable:::modelInfo.summary.lm)
##' \dontrun{
##' example(apsrtable)
##' }
##' 
NULL





##' Table notes
##' 
##' Prepare notes about standard errors and statistical significance
##' 
##' Table notes are part of the tabular environment and may be based on the
##' content of the table itself. For example, the \code{stars} argument to
##' \code{\link{apsrtable}} determines whether one or many levels of
##' statistical significance are indicated in the output. The \code{stars.note}
##' function creates text to place in such a note.
##' 
##' By default the output uses the notation \eqn{ * p <.05} and the example
##' below shows a replacement function that states, \dQuote{significant at
##' \code{lev} percent.}.
##' 
##' To access variables in the call to \code{apsrtable} from functions in
##' \code{notes}, include the arugment \code{env} in any custom functions. This
##' is the \code{apsrtable} call environment.
##' 
##' Remember, to escape characters in Latex output, backslashes have to be
##' doubled in R character strings.
##' 
##' @aliases notefunctions se.note stars.note pval.note
##' @param env The environment of the \code{apsrtable()} call, because note
##' functions may need to make use of some variables such as \code{lev} or
##' \code{digits}.
##' @return A character string to place within the tabular environment in
##' footnotesize beneath other output.
##' @author Michael Malecki <malecki at wustl.edu>
##' @examples
##' 
##' ### Custom note function
##' 
##' signif.pct <- function(env) {
##'   paste("$^*$ significant at", evalq(lev,envir=env)*100, "percent")
##' }
##' ### Continue the example from apsrtable
##' \dontrun{
##' apsrtable(lm.D90, lm.D9, glm.D9, digits=1, align="left",
##'           stars=1, lev=0.05, model.counter=0, order="rl",
##'           notes=list(se.note, signif.pct, 
##'             "Plant weight data from the lm() example" )
##' 	 )
##' }
##' 
NULL



