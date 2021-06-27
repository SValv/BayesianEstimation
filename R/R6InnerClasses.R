#' @import ggplot2
#' @import dplyr
#' @import tidyr
#'
svss_class = R6::R6Class(
  "Ssvs",
  private = list(
    ALPHA.store = 0,
    A.mean = 0,
    SIGMA.store = 0,
    SIG.mean = 0,
    Gamma.store = 0,
    PIP.mean = 0,
    y = 0,
    x = 0,
    nsave = 0,
    nburn = 0,
    tau0 = 0,
    tau1 = 0,
    S0 = 0,
    result=0

  ),


  public = list(
    initialize = function(y, x, nsave, nburn, tau0, tau1, S0) {
      private$x = x
      private$y = y
      private$nsave = nsave
      private$nburn = nburn
      private$tau0 = tau0
      private$tau1 = tau1
      private$S0 = S0

      ntot <- nsave + nburn
      X <- as.matrix(x)
      Y <- matrix(y)
      N <- nrow(Y)
      K <- ncol(X)

      A.OLS <- solve(crossprod(X)) %*% crossprod(X, Y)
      SSE <- crossprod(Y - X %*% A.OLS)
      SIG.OLS <- SSE / (N - K)
      gamma <- matrix(1, K, 1)
      sigma2.draw <- as.numeric(SIG.OLS)
      V.prior <-
        diag(as.numeric(gamma * tau1 + (1 - gamma) * tau0))


      ALPHA.store <- matrix(NA, nsave, K)
      SIGMA.store <- matrix(NA, nsave, 1)
      Gamma.store <- matrix(NA, nsave, K)

      for (irep in 1:ntot) {
        V.post <-
          solve(crossprod(X) * 1 / sigma2.draw + diag(1 / diag(V.prior)))
        A.post <-
          V.post %*% (crossprod(X, Y) * 1 / sigma2.draw)
        A.draw <- A.post + t(chol(V.post)) %*% rnorm(K)

        for (jj in 1:K) {
          p0 <- dnorm(A.draw[[jj]], 0, sqrt(tau0))
          p1 <- dnorm(A.draw[[jj]], 0, sqrt(tau1))
          p11 <- p1 / (p0 + p1)

          if (p11 > runif(1))
            gamma[[jj]] <- 1
          else
            gamma[[jj]] <- 0
        }

        V.prior <-
          diag(as.numeric(gamma * tau1 + (1 - gamma) * tau0))


        S.post <- crossprod(Y - X %*% A.draw) / 2 + S0
        s.post <- S0 + N / 2
        sigma2.draw <- 1 / rgamma(1, s.post, S.post)

        if (irep > nburn) {
          ALPHA.store[irep - nburn, ] <- A.draw
          SIGMA.store[irep - nburn, ] <- sigma2.draw
          Gamma.store[irep - nburn, ] <- gamma
        }
      }
      PIP.mean <- apply(Gamma.store, 2, mean)
      A.mean <- apply(ALPHA.store, 2, mean)
      SIG.mean <- apply(SIGMA.store, 2, mean)
      colnames(ALPHA.store)=colnames(x)

      private$ALPHA.store = ALPHA.store
      private$A.mean = A.mean
      private$SIGMA.store = SIGMA.store
      private$SIG.mean = SIG.mean
      private$Gamma.store = Gamma.store
      private$PIP.mean = PIP.mean
      private$result=data.frame(row.names=colnames(x),PIP.mean,SIG.mean,A.mean)


    },

    coefPlot = function(ncoef = "all") {
      xx1 = as.data.frame.matrix(private$ALPHA.store)
      if (ncoef == "all") {
        p=pivot_longer(xx1, cols = everything()) %>%
          ggplot(aes(x = value)) + geom_density() + geom_vline(xintercept = 0,col="red")+
          facet_wrap(. ~ name) + theme_bw()

      } else{
        p=ggplot(xx1,aes(x=get(ncoef)))+
          geom_density()+
          theme_bw()+
          geom_vline(xintercept = 0,col="red")+
          labs(x=ncoef)
      }
      return(p)
    }

  )
)
