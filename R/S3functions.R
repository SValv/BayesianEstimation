#' @method summary Ssvs
#' @export
summary.Ssvs=function(obj, coefs=T){
  if (coefs == T){
    cat("Coefficients: \n")
    print(obj$.__enclos_env__$private$result)
  }
  cat("\n")
  cat("N:", nrow(obj$.__enclos_env__$private$x)," ")
  cat(paste("Burins:", obj$.__enclos_env__$private$nburn," "))
  cat(paste("Draws:", obj$.__enclos_env__$private$nsave + obj$.__enclos_env__$private$nburn," \n"))
  cat("Tau0: ",obj$.__enclos_env__$private$tau0 ," Tau1: ", obj$.__enclos_env__$private$tau1 , " S0: ", obj$.__enclos_env__$private$S0,"\n")
  cat("Sig.mean: ",obj$.__enclos_env__$private$SIG.mean)
}

#' @method plot Ssvs
#' @export
plot.Ssvs= function(obj, coef=T){
  if (coef==T){
    obj$coefPlot()
  }else if(coef != F){
    obj$coefPlot(coef)
  }else{
    print("pass")
  }
}

#' @method coef Ssvs
#' @export
coef.Ssvs=function(obj){
  return(obj$.__enclos_env__$private$result)

}
