
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
                 paste("$^*$ indicates significance at $p<",
                       evalq(lev,envir=env),"$")))
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
    result <- gsub("SANITIZE.BACKSLASH","$\\backslash$",
                   result,fixed=TRUE)
    return(result)
}
