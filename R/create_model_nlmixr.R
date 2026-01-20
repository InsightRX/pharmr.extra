#' Temporary function that returns a hardcoded nlmixr2 model
#' 
#' @param ... Any arguments to the function are just ignored, this is just for demo
#' purposes
#' 
#' @returns TODO
#' 
#' @export
create_model_nlmixr <- function(...) { # nocov start
  mod <- function() {
    ini({
      # --- THETAS ---
      POP_KA <- c(0.0, 0.5, Inf)
      POP_CL <- c(0.0, 5, Inf)
      POP_V <- c(0.0, 150, Inf)
      POP_BIO <- c(0.0, 0.5, 1.0)
      
      # --- ETAS ---
      ETA_1 ~ 0.3
      ETA_2 ~ 0.3
      ETA_3 ~ 0.3
      
      # --- EPSILONS ---
      RUV_ADD <- c(0.0, 0.5, Inf)
    })
    model({
      BIO <- log(POP_BIO/(1 - POP_BIO))
      TVKA <- POP_KA
      TVCL <- POP_CL
      TVV <- POP_V
      KA <- TVKA*exp(ETA_1)
      CL <- TVCL*exp(ETA_2)
      V <- TVV*exp(ETA_3)
      S2 <- V/1000
      F_BIO <- 1/(1 + exp(-BIO))
      
      # --- DIFF EQUATIONS ---
      d/dt(A_DEPOT) = -KA*A_DEPOT
      d/dt(A_CENTRAL) = -CL*A_CENTRAL/V + KA*A_DEPOT
      
      f(A_DEPOT) <- F_BIO
      F <- A_CENTRAL/S2
      W <- 1
      IPRED <- F
      Y <- IPRED
      add_error <- RUV_ADD*W
      prop_error <- 0
      Y ~ add(add_error) + prop(prop_error)
    })
  }
  mod 
} # nocov end

# R CMD check gets confused about the rxode2 syntax, this is to suppress R CMD
# check NOTEs about "no visible binding for global variable"
utils::globalVariables(
  c(
    "ETA_1",
    "ETA_2",
    "ETA_3",
    "A_CENTRAL",
    "f<-"
  )
)
