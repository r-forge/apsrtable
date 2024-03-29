﻿apsrtable <- function (..., 
                       se=c("robust","vcov","both","pval"),
                       # model.names can be shorter, others numbered;
                       # numbers start at value of model.counter
                       model.names=NULL, model.counter=1, digits=2,
                       # stars="default" prints R default
                       # this function default is one star at .05
                       stars=1,lev=.05,
                       align=c("left","center","right"),
                       order=c("lr","rl","longest"),
                       notes=list(se.note(),stars.note() ),
                       omitcoef=NULL,coef.names=NULL,
                       coef.rows=2,
                       multicolumn.align=c("center","left","right"),
                       col.hspace=NULL,
                       Sweave=FALSE, float="table",
                       Minionfig=FALSE,
                       label=NULL,caption=NULL
                       ) {
  x <- list()
  signif.stars <- TRUE
  order <- match.arg(order,c("lr","rl","longest"))
  opts <- match.call(expand.dots=FALSE)
  se <- match.arg(se,c("robust","vcov","both","pval"))
  align <- substr(align,1,1)
  align <- match.arg(align,c("l","c","r"))
  multicolumn.align <- match.arg(substr(multicolumn.align,1,1),
                                 c("c","l","r"))
  adigits <- ifelse(align=="c",
                    -1,
                    digits)
  models <- list(...)
  nmodels <- length(models)

  ## used to multiply later for column counts
  coef.cols <- ifelse(coef.rows==2, 1, 2)

  ## Two default behaviors for col.hspace:
  ##  if in two-column-per-model mode, default to 2em
  ##  otherwise, empty. If not "", add some latex around it.
  if (is.null(col.hspace)) {
  col.hspace <- ifelse(coef.cols==1,"",
                       "2em")
  }
  if(col.hspace != "") {
    col.hspace <- paste("@{\\hspace{",col.hspace,"}}",sep="")
  }
  colspec <- paste("{",
                   align,
                   paste(rep(paste("D{.}{.}{",
                                   rep(adigits,coef.cols),
                                    "}",
                                   sep="",collapse=""),nmodels),
                         collapse=col.hspace)
                   ,"}",collapse="")
  if(float=="longtable") {
    long <- TRUE
    floatspec <- paste("\\begin{",float,"}",colspec,"\n",
                       "\\caption{",caption,"}\n\\label{",label,"}",
                       sep="")
  } else
  {
    long <- FALSE
    floatspec <- paste(ifelse(!Sweave,
                              paste("\\begin{",float,"}[!ht]\n",
                                    "\\caption{",caption,
                                    "}\n\\label{",label,"}",sep=""),
                              "" ),
                       paste("\n\\begin{tabular}",colspec,sep=""))
  }
  x <- paste(floatspec,
             ifelse(long,"\\\\",""),
             sep="")

  if(Minionfig){
    x <- c(x,"%Uncomment the following line and the end one to change figure versions\n%if you are using a full-featured family such as Minion Pro.\n\\figureversion{tabular}\n")
  }

  
  ## get the summaries for the objects
  model.summaries <- lapply(models,
                            ## If an apsrtableSummary exists, use it
                            ## Otherwise, use summary.
                            function(x) {
                              s <- try(apsrtableSummary(x), silent=TRUE) 
                              if (inherits(s, "try-error")) {
                                s <- summary(x)
                              }
                              if(!is.null(x$se) && se != "vcov") {
                                est <- coef(x)
                                if(class(x$se) == "matrix") {
                                  x$se <- sqrt(diag(x$se))
                                } 
                                s$coefficients[,3] <- tval <- est / x$se
                                s$coefficients[,4] <-
                                  2 * pt(abs(tval),
                                         length(x$residuals) - x$rank,
                                         lower.tail=FALSE)
                                s$se <- x$se }
                              if(se == "pval") {
                                s$coefficients[,2] <- s$coefficients[,4]
                                
                              }
                              return(s)
                            } )
  
  ## Quietly switch the se.note to the pval.note as needed
  if(se=="pval") { se.note <- pval.note }

  ## Set up the model names
  ## If there's a vector of names, use that, or as many as there are
  ## and either all or the remainder.
  ## Optionally, model.number.start allows you to resetcounter
  ## TO DO: allow model "name" attribute to be used
  ##        but overridden by vector here.
  if (is.null(model.names)) {
    m.first = model.counter; m.last=m.first+(nmodels-1)
    model.names=paste("Model", m.first:m.last)
  } else if (!is.null(model.names) && (length(model.names) < nmodels) ) {
    m.first = length(model.names)+1
    model.names=c(model.names, paste( "Model", m.first:nmodels))
  }
  
## get and order the coefficient names from all models
  coefnames <- orderCoef(model.summaries, order=order)

  ## mark those to omit from the output
  incl <- rep(TRUE,length(coefnames))
  names(incl) <- coefnames
  if(!is.null(omitcoef)) {
    ## Boris Shor <boris@bshor.com> asked how to omitcoef by regex
    ##  this line enables omitcoef=expression() 2010-03-17
    ##  OR if you want to mix modes or provide multiple expr
    ##  you can supply a list() eg list(expression(grep), 15) 
    omitcoef <- unlist(sapply(omitcoef, eval.parent, n=2 ))
    #print(omitcoef)
    incl[omitcoef] <- FALSE
  }
## now figure out position of each coef in each model
model.summaries <- coefPosition(model.summaries, coefnames)
  
  ## Now that the coef name matching is done, switch to pretty names
  ## if they are supplied. 
    if(!is.null(coef.names)) {
      if(length(coef.names) != sum(incl)) {
        warning("Supplied coef.names not the same length as output. Check automatic names before supplying 'pretty' names.\n")
      }
      coefnames[incl] <- coef.names
    } else {
      coefnames[incl] <- sanitize(coefnames[incl])
    }
  
  
  out.table <- lapply(model.summaries, function(x){
    var.pos <- attr(x,"var.pos")
    model.out <- model.se.out <- star.out <- rep(NA,length(coefnames))
    model.out[var.pos] <- x$coefficients[,1]
    star.out[var.pos] <- apsrStars(x$coefficients,stars=stars,lev=lev,signif.stars=TRUE)
    model.out <- ifelse(!is.na(model.out),
                        paste(formatC(model.out,digits=digits,format="f"),
                              star.out),
                        "")
    
    
    
    model.se.out[var.pos] <- x$coefficients[,2]
    if( !is.null(x$se) & se %in% c("robust","both") ) {
      model.se.out[var.pos] <- x$se
    }
    
    model.se.out <- ifelse(!is.na(model.se.out),
                           paste("(",
                                 formatC(model.se.out,
                                         digits=digits,
                                         format="f"),
                                 ")",sep=""),
                           "")
    if(se=="both" && !is.null(x$se)){      
      model.se.out[var.pos] <- ifelse(model.se.out != "",
                             paste(model.se.out," [",
                                   formatC(x$coefficients[,2],
                                           digits=digits,
                                           format="f"),
                                   "]",sep=""),
                             "")
    }
    
    if(coef.rows==2) {
      ## Create two side by side columns and mesh them together
      model.out <- rep(model.out[incl], each=2)
      model.se.out <- rep(model.se.out[incl], each=2)
      pos.se <- (1:length(model.out))[(1:length(model.out) %% 2==0)]
      model.out[pos.se] <- model.se.out[pos.se]
      ## Add a new model info attribute to the model's output entry
      ## To change modelInfo for a given model, change the method for it
      ## see ?modelInfo, it is reasonably well documented.
    } else {
      ## two columns per model
      model.out <- model.out[incl]
      model.out <- cbind(model.out, model.se.out[incl])
    }
    attr(model.out,"model.info") <- modelInfo(x)  
    return(model.out)
  })
  
  out.matrix <- matrix(unlist(out.table),
                       length(coefnames[incl])*coef.rows,
                       nmodels*coef.cols)

  out.matrix <- cbind(rep(coefnames[incl],each=coef.rows), out.matrix)
  if(coef.rows==2) {
    out.matrix[ (row(out.matrix)[,1] %% 2 ==0) , 1] <- ""  
  }
  out.info <- lapply(out.table, attr, "model.info")
  info.names <- orderCoef(out.info)
  out.info <- coefPosition( out.info, orderCoef(out.info) )
  out.info <- lapply(out.info, function(x) {
    var.pos <- attr(x,"var.pos")
    model.out <- rep("",length(info.names))
    model.out[var.pos] <- coef(x)
    return(model.out)
  } )

  out.info <- matrix(unlist(out.info), length(info.names), nmodels)
  out.info <- cbind(as.character(info.names), out.info)
  
  if(coef.rows==2) {
    out.matrix <- rbind(c("%",model.names ),out.matrix)
  }
  outrows <- nrow(out.matrix)
  
  ## This does the pretty latex formatting, where commented model names
  ## line up with appropriately sized columns of numbers.

  ## Paul Johnson suggested a 'wide' or two column format for tables
  ## which then means model info needs to be underneath the two
  ## in a multicolumn span. But, for normal (two row, one column per coef)
  ## format, this is extraneous markup and hard to read.
  if(coef.cols==1) {
    out.matrix <- rbind(out.matrix,out.info)
    out.matrix[,-1] <- format(out.matrix[,-1])
    out.matrix[,1] <- format(out.matrix)[,1]
    out.matrix <- apply(out.matrix, 1, paste, collapse=" & ")
    out.info <- out.matrix[ (1+outrows) : length(out.matrix) ]
    out.matrix <- out.matrix[ 1:outrows ]
  } else {
    out.matrix <- format(out.matrix)
    out.matrix <- apply(out.matrix, 1, paste, collapse=" & ")
    ## now do the out.info as multicolumn blocks
    out.info[,-1] <- format(out.info[,-1])
    out.info[,-1] <- sapply(as.matrix(out.info[,-1]), function(x) {
      paste("\\multicolumn{",coef.cols,"}{",multicolumn.align,
            "}{",x,"}",sep="")
    })
    out.info[,1] <- format(out.info[,1])
    out.info <- apply(out.info, 1, paste, collapse=" & ")
  }
  
  headrow <- paste("\n\\hline \n",
                   paste(" &", paste("\\multicolumn{",coef.cols,"}{",
                               multicolumn.align,"}{",
                               model.names,"}", collapse=" & ")  ),
               "\\\\ \\hline\n")
  if(long) { headrow <- paste(headrow,"\\endhead\n",sep="") }
  x <- c(x, headrow)
  
  #x <- c(x,"")
  x <- c(x,paste(out.matrix, collapse="\\\\ \n"))
  x <- c(x,"\\\\\n")
  x <- c(x,paste(out.info, collapse="\\\\ \n"))
  
  ## Do notes
   ## Evaluate the notes list
    ## Switch the se to either robust or regular --
  ## Robust is the default, but if only vcov are given,
  ## quietly switch the argument.
  se <- ifelse((se != "vcov" &&
                sum(unlist(lapply(model.summaries,
                                  function(x) !is.null(x$se))) >0 ) ) ,
               "robust","vcov")
  notes <- lapply(notes,evalq,env=parent.frame())
  
  x <- c(x,"\\\\ \\hline\n")
  notes <- lapply(notes, function(x) {  # eek! note coef cols was wrong
                                        # fixed 2009-05-07 mjm
    paste("\\multicolumn{",(nmodels*coef.cols)+1,"}{l}{\\footnotesize{", x , "}}",sep="")
             } )
  x <- c(x, paste(notes, collapse="\\\\\n"))
 
  if(!long) { x <- c(x,"\n\\end{tabular}") }
  if(long) { x <- c(x,"\n\\end{longtable}") }
  x <- c(x,"\n")
  if(Minionfig) {x <- c(x,"\n\\figureversion{proportional}\n") }
  if(!Sweave & !long) { x <- c(x,paste("\\end{",float,"}\n",sep="")) }
  class(x) <- "apsrtable"
  return(x)
}



apsrStars <- function (x, digits = max(3, getOption("digits") - 2),
                       signif.stars = getOption("show.signif.stars"), 
                       signif.legend = signif.stars,
                       dig.tst = max(1, min(5, digits - 1)), cs.ind = 1:k,
                       tst.ind = k + 1, zap.ind = integer(0), 
                       P.values = NULL,
                       has.Pvalue = nc >= 3 && # used to be 4
                       substr(colnames(x)[nc],
                                      1, 3) == "Pr(" ||
                       grep("z",colnames(x)[nc]) == TRUE,
                       eps.Pvalue = .Machine$double.eps, na.print = "NA",
                       stars="default",lev=.05,
    ...) 
{
    if (is.null(d <- dim(x)) || length(d) != 2) 
        stop("'x' must be coefficient matrix/data frame")
    nc <- d[2]
    if (is.null(P.values)) {
        scp <- getOption("show.coef.Pvalues")
        if (!is.logical(scp) || is.na(scp)) {
            warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
            scp <- TRUE
        }
        P.values <- has.Pvalue && scp
    }
    else if (P.values && !has.Pvalue) 
        stop("'P.values' is TRUE, but 'has.Pvalue' is not")
    if (has.Pvalue && !P.values) {
        d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
        nc <- nc - 1
        has.Pvalue <- FALSE
    }
    else xm <- data.matrix(x)
    k <- nc - has.Pvalue - (if (missing(tst.ind)) 
        1
    else length(tst.ind))
    if (!missing(cs.ind) && length(cs.ind) > k) 
        stop("wrong k / cs.ind")
    Cf <- array("", dim = d, dimnames = dimnames(xm))
    ok <- !(ina <- is.na(xm))
    if (length(cs.ind) > 0) {
        acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
        if (any(is.finite(acs))) {
            digmin <- 1 + floor(log10(range(acs[acs != 0], na.rm = TRUE)))
            Cf[, cs.ind] <- format(round(coef.se, max(1, digits - 
                digmin)), digits = digits)
        }
    }
    if (length(tst.ind) > 0) 
        Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst), 
            digits = digits)
    if (length(zap.ind) > 0) 
        Cf[, zap.ind] <- format(zapsmall(xm[, zap.ind], digits = digits), 
            digits = digits)
    if (any(r.ind <- !((1:nc) %in% c(cs.ind, tst.ind, zap.ind, 
        if (has.Pvalue) nc)))) 
        Cf[, r.ind] <- format(xm[, r.ind], digits = digits)
    okP <- if (has.Pvalue) 
        ok[, -nc]
    else ok
    x1 <- Cf[okP]
    dec <- getOption("OutDec")
    if (dec != ".") 
        x1 <- chartr(dec, ".", x1)
    x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
    if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
        Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1, 
            digits - 1))
    }
    if (any(ina)) 
        Cf[ina] <- na.print
    
    if (P.values) {
        if (!is.logical(signif.stars) || is.na(signif.stars)) {
            warning("option \"show.signif.stars\" is invalid: assuming TRUE")
            signif.stars <- TRUE
        }
        
        if (any(okP <- ok[, nc])) {
          pv <- as.vector(xm[, nc])
          Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst, 
                                     eps = eps.Pvalue)
          signif.stars <- signif.stars && any(pv[okP] < 0.1)
          Signif <- ""
          if (signif.stars && stars=="default") {
            Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                             symbols = c("^{***}", "^{**}", "^*", "^\\dagger\ ", " "))
            Cf <- cbind(Cf, format(Signif))
          }
          else if (signif.stars && stars==1) {
           Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                             cutpoints = c(0,lev,1), 
                             symbols = c("^*"," "))
          }
          return(Signif)
        }
      }

    return()
  }


setGeneric("modelInfo", function(x) standardGeneric("modelInfo") )


modelInfo.summary.lm <- function(x) {
  env <- sys.parent()
  digits <- evalq(digits, env)
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
  digits <- evalq(digits, env)
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
 digits <- evalq(digits, env)
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
 digits <- evalq(digits, env)
 model.info <- list(" " = ""
                    )
 class(model.info) <- "model.info"
 return(model.info)
}

"modelInfo.summary.mer" <- function (x) {
	env <- sys.parent()
	digits <- evalq(digits,env)
	model.info <- list()
	model
  

"modelInfo.summary.coxph" <- function (x) {
       env <- sys.parent()
       digits <- evalq(digits, env)
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

## tobit requested by Antonio Ramos added by mjm 2009-02-25
## gee requested by Dustin Tingley started by mjm 2009-04-24
## coxph provided by David Hugh-Jones 2009-11-20 added mjm 2009-12-08

setGeneric("modelInfo", def=function(x){standardGeneric("modelInfo")})
setOldClass("summary.lm")
setOldClass("summary.glm")
setOldClass("summary.tobit")
setOldClass("summary.gee")
setOldClass("summary.coxph")
setOldClass("summary.negbin")

setMethod("modelInfo", "summary.lm", modelInfo.summary.lm )
setMethod("modelInfo","summary.glm", modelInfo.summary.glm )
setMethod("modelInfo","summary.tobit", modelInfo.summary.tobit)
setMethod("modelInfo","summary.gee",modelInfo.summary.gee)
setMethod("modelInfo","summary.coxph",modelInfo.summary.coxph)
setMethod("modelInfo","summary.negbin",modelInfo.summary.glm)
"coef.model.info" <- function(object,...) {
  x <- as.matrix(unlist(object)); invisible(x)
} 

## RULES: All according to longest model,
##        then left to right
## RESULT: union of all models' coefficient names in requested order.
orderCoef <- function(model.summaries,order="lr") {
  nmodels <- length(model.summaries)
  mlength <- sapply(model.summaries, function(x) length(coef(x)) )
  longest <- which.max(mlength) # longest model
  if(order=="rl") {
    modelorder <- nmodels:1 } else {
      modelorder <- 1:nmodels }
  if(order=="longest") {
    coefnames <-  rownames(coef(model.summaries[[longest]]))
  } else {
    coefnames <- rownames(coef(model.summaries[[modelorder[1]]])) }
  
  for(i in seq_along(model.summaries)) {
    matched <- match(rownames(coef(model.summaries[[i]])), coefnames, nomatch=0)
    unmatched <- which(is.na(matched) | matched==0)
    coefnames <- c(coefnames,
                   rownames(coef(model.summaries[[i]]))[unmatched]
                   )
  }
  return(coefnames)
}
## Given a list of model summaries (or anything with a coef method),
## and a master (unioned) list of coef names,
##
## Append an attribute to each element containing its coefs' position in the
## master coefficient list
"coefPosition" <- function(model.summaries, coefnames) {
  model.summaries <- lapply(model.summaries, function(x) {
    pos <- match(rownames(coef(x)), coefnames)
    attr(x,"var.pos") <- pos
    return(x)
  })
return(model.summaries)
}

"se.note" <- function(env) {
  note <- paste(ifelse( evalq(se,envir=env) != "vcov","Robust s","S"),
                "tandard errors in parentheses",
                ifelse(evalq(se,envir=env)=="both",
                       paste("\\\\\n\\multicolumn{",
                             evalq(nmodels,envir=env)+1,"}{l}{",
                             'Na\\"ive standard errors in brackets',
                             collapse="",sep=""),
                       "" ) ,sep="")
  return(note)
}

## Added pval support
"pval.note" <- function(env) {
  note <- paste(ifelse(evalq(se,envir=env) != "vcov", "Robust ", ""),
                "$p$ values in parentheses",sep="")
  return(note)
}
"stars.note" <- function(env) {
  paste(ifelse(evalq(stars,envir=env)=="default",
               paste("$^\\dagger$ significant at $p<.10$; $^* p<.05$; $^{**} p<.01$; $^{***} p<.001$"),
               paste("$^*$ indicates significance at $p<",evalq(lev,env),"$")))
}

## apsrtableSummary enables easy S3 method masking of summary functions
## where the model-package provides a summary not suitable for apsrtable,
## such as z scores instead of pnorms.

"apsrtableSummary" <- function(x) {
  UseMethod("apsrtableSummary") }

"apsrtableSummary.gee" <- function(x) {
  s <- summary(x)
  newCoef <- coef(s)
  ## which columns have z scores? (two of them in robust case)
  zcols <- grep("z",colnames(newCoef))
  newCoef[,zcols] <- pnorm(abs(newCoef[,zcols]), lower.tail=FALSE)
  colnames(newCoef)[zcols] <- "Pr(z)"
  s$coefficients <- newCoef
  ## put the robust se in $se so that notefunction works automatically
  ## the se checker will overwrite [,4] with pt, but this doesn't matter
  ## because the last column Pr(z) is used by apsrstars() anyway
  ## and the se are pulled from $se.
  if( class(x)[1] == "gee.robust") {
    s$se <- coef(s)[,4]
  }
  return(s)
}

"apsrtableSummary.clogit" <- apsrtableSummary.coxph <- function (x) {
       s <- summary(x)
       if("robust se" %in% colnames(coef(s))) s$se <- coef(s)[,"robust se"]
       s$coefficients <- coef(s)[,c("coef","se(coef)", "Pr(>|z|)")]
       return(s)
}
"apsrtableSummary.negbin" <- function (x) {
       s <- summary(x)
       coefs <- coef(s)
       theta <- matrix(c(s$theta, s$SE.theta,NA,NA),1,4)
       theta[,3] <- theta[,1]/theta[,2] ;
       theta[,4] <- pnorm(abs(theta[,3]),lower.tail=FALSE)
       rownames(theta) <- "$\\theta$"
       s$coefficients <- rbind(coefs,theta)
       return(s)
}


"print.apsrtable" <- function(x,...) {
  cat(paste(x))
}

## ADM <admartin@wustl.edu> requested sanitizing coefnames 2010-03-10
## this function taken from print.xtable v1.5-6.
sanitize <- function(str) {
  result <- str
  result <- gsub("\\\\","SANITIZE.BACKSLASH",result)
  result <- gsub("$","\\$",result,fixed=TRUE)
  result <- gsub(">","$>$",result,fixed=TRUE)
  result <- gsub("<","$<$",result,fixed=TRUE)
  result <- gsub("|","$|$",result,fixed=TRUE)
  result <- gsub("{","\\{",result,fixed=TRUE)
  result <- gsub("}","\\}",result,fixed=TRUE)
  result <- gsub("%","\\%",result,fixed=TRUE)
  result <- gsub("&","\\&",result,fixed=TRUE)
  result <- gsub("_","\\_",result,fixed=TRUE)
  result <- gsub("#","\\#",result,fixed=TRUE)
  result <- gsub("^","\\verb|^|",result,fixed=TRUE)
  result <- gsub("~","\\~{}",result,fixed=TRUE)
  result <- gsub("SANITIZE.BACKSLASH","$\\backslash$",result,fixed=TRUE)
  return(result)
}
       


## A couple of test calls here for random features
## library(apsrtable);example(apsrtable);apsrtable(lm.D90, lm.D9, glm.D9, digits=1, align="center", stars="default", model.counter=0, order="rl", omitcoef="(Intercept)")
## library(apsrtable);example(apsrtable);apsrtable(lm.D90,coef.names=c("\\#1","\\#0"))
