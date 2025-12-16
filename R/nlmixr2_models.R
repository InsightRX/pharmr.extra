#' nlmixr2 model: 1-cmt linear model (oral)
#' Linear absorption without lagtime
#'
#' @importFrom rxode2 ini model
#' @export
nlmixr2_pk_1cmt_oral_linear <- function() {
  ini({
    tka     <- log(0.25)
    tCL     <- log(5) 
    tV      <- log(50)
    eta_CL   ~ 0.3
    eta_V    ~ 0.3
    eta_ka   ~ 0.3
    prop_sd <- 0.15
  })
  model({
    ka <- exp(tka + eta_ka)
    CL <- exp(tCL + eta_CL)
    V  <- exp(tV  + eta_V)
    # d/dt(A1) = -ka*A1
    # d/dt(A2) = -(CL/V)*A2 + ka*A1
    # Cp = A2 / V
    # Cp ~ prop(prop_sd)
    linCmt() ~ prop(prop_sd)
  })
}

#' nlmixr2 model: 1-cmt linear model (oral)
#' Linear absorption with lagtime
#' 
#' @export
nlmixr2_pk_1cmt_oral_linear_lag <- function() {
  ini({
    tka     <- log(0.25)
    tCL     <- log(5) 
    tV      <- log(50)
    tlag    <- log(0.5)
    eta_CL   ~ 0.3
    eta_V    ~ 0.3
    eta_ka   ~ 0.3
    prop_sd <- 0.15
  })
  model({
    ka <- exp(tka + eta_ka)
    CL <- exp(tCL + eta_CL)
    V  <- exp(tV  + eta_V)
    d/dt(A1) = -ka*A1
    d/dt(A2) = -(CL/V)*A2 + ka*A1
    lag(A1) = exp(tlag)
    Cp = A2 / V
    Cp ~ prop(prop_sd)
  })
}

#' nlmixr2 model: 1-cmt linear model (oral)
#' Transit compartment absorption model
#' 
#' @export
nlmixr2_pk_1cmt_oral_linear_transit <- function() {
  ini({
    tka     <- log(0.25)
    tCL     <- log(5) 
    tV      <- log(50)
    tMTT    <- log(0.2)
    tN      <- log(3) 
    eta_CL   ~ 0.3
    eta_V    ~ 0.3
    eta_ka   ~ 0.3
    prop_sd <- 0.15
  })
  model({
    ka <- exp(tka + eta_ka)
    CL <- exp(tCL + eta_CL)
    V  <- exp(tV  + eta_V)
    MTT <- exp(tMTT)
    N <- exp(tN)
    ktr = (N+1)/MTT
    d/dt(A1) = transit(N, MTT, 1.0) - ka*A1
    d/dt(A2) = -(CL/V)*A2 + ka*A1
    Cp = A2 / V
    Cp ~ prop(prop_sd)
  })
}

#' nlmixr2 model: 2-cmt linear model (iv)
#' 
#' @export
nlmixr2_pk_1cmt_iv_linear <- function() {
  ini({
    tCL     <- log(5) 
    tV      <- log(50)
    eta_CL   ~ 0.3
    eta_V    ~ 0.3
    prop_sd <- 0.15
  })
  model({
    CL <- exp(tCL + eta_CL)
    V  <- exp(tV  + eta_V)
    # d/dt(A1) = -Ka*A1
    # d/dt(A2) = -(CL/V)*A2 + Ka*A1
    # Cp = A2 / V
    # Cp ~ prop(prop_sd) + add(add_sd)
    linCmt() ~ prop(prop_sd)
  })
}

#########################################################
## 2-cmt models
#########################################################

#' nlmixr2 model: 2-cmt linear model (oral)
#' 
#' @export
nlmixr2_pk_2cmt_oral_linear <- function() {
  ini({
    tka     <- log(0.5)
    tCL     <- log(5) 
    tV      <- log(50)
    tQ      <- log(10) 
    tV2     <- log(100)
    eta_CL   ~ 0.3
    eta_V    ~ 0.3
    eta_Q    ~ 0.3
    eta_V2   ~ 0.3
    eta_ka   ~ 0.3
    prop_sd <- 0.15
  })
  model({
    ka <- exp(tka + eta_ka)
    CL <- exp(tCL + eta_CL)
    V  <- exp(tV  + eta_V)
    Q  <- exp(tQ  + eta_Q)
    V2 <- exp(tV2 + eta_V2)
    # d/dt(A1) = -Ka*A1
    # d/dt(A2) = -(CL/V)*A2 + Ka*A1
    # Cp = A2 / V
    # Cp ~ prop(prop_sd) + add(add_sd)
    linCmt() ~ prop(prop_sd)
  })
}

#' nlmixr2 model: 2-cmt linear model (oral)
#' Linear absorption with lagtime
#' 
#' @export
nlmixr2_pk_2cmt_oral_linear_lag <- function() {
  ini({
    tka     <- log(0.5)
    tCL     <- log(5) 
    tV      <- log(50)
    tQ      <- log(10) 
    tV2     <- log(100)
    tlag    <- log(0.5)
    eta_CL   ~ 0.3
    eta_V    ~ 0.3
    eta_Q    ~ 0.3
    eta_V2   ~ 0.3
    eta_ka   ~ 0.3
    prop_sd <- 0.15
  })
  model({
    ka <- exp(tka + eta_ka)
    CL <- exp(tCL + eta_CL)
    V  <- exp(tV  + eta_V)
    Q  <- exp(tQ  + eta_Q)
    V2 <- exp(tV2 + eta_V2)
    d/dt(A1) = -ka*A1
    d/dt(A2) =  ka*A1 -(CL/V)*A2 - (Q/V)*A2 + (Q/V2)*A3
    d/dt(A3) =                     (Q/V)*A2 - (Q/V2)*A3
    lag(A1) = exp(tlag)
    Cp = A2 / V
    Cp ~ prop(prop_sd)
  })
}

#' nlmixr2 model: 2-cmt linear model (oral)
#' Transit compartment absorption model
#' 
#' @export
nlmixr2_pk_2cmt_oral_linear_transit <- function() {
  ini({
    tka     <- log(0.5)
    tCL     <- log(5) 
    tV      <- log(50)
    tQ      <- log(10) 
    tV2     <- log(100)
    tMTT    <- log(0.2)
    tN      <- log(3)
    eta_CL   ~ 0.3
    eta_V    ~ 0.3
    eta_Q    ~ 0.3
    eta_V2   ~ 0.3
    eta_ka   ~ 0.3
    prop_sd <- 0.15
  })
  model({
    ka <- exp(tka + eta_ka)
    CL <- exp(tCL + eta_CL)
    V  <- exp(tV  + eta_V)
    Q  <- exp(tQ  + eta_Q)
    V2 <- exp(tV2 + eta_V2)
    MTT <- exp(tMTT)
    N   <- exp(tN)
    ktr = (N+1)/MTT
    d/dt(A1) =  transit(N, MTT, 1.0) - ka*A1
    d/dt(A2) =  ka*A1 -(CL/V)*A2 - (Q/V)*A2 + (Q/V2)*A3
    d/dt(A3) =                     (Q/V)*A2 - (Q/V2)*A3
    Cp = A2 / V
    Cp ~ prop(prop_sd)
  })
}

#' nlmixr2 model: 2-cmt linear model (iv)
#' 
#' @export
nlmixr2_pk_2cmt_iv_linear <- function() {
  ini({
    tCL     <- log(5) 
    tV      <- log(50)
    tQ      <- log(10) 
    tV2     <- log(100)
    eta_CL   ~ 0.3
    eta_V    ~ 0.3
    eta_Q    ~ 0.3
    eta_V2   ~ 0.3
    prop_sd <- 0.15
  })
  model({
    CL <- exp(tCL + eta_CL)
    V  <- exp(tV  + eta_V)
    Q  <- exp(tQ  + eta_Q)
    V2 <- exp(tV2 + eta_V2)
    # d/dt(A1) = -Ka*A1
    # d/dt(A2) = -(CL/V)*A1 + Ka*A1
    # Cp = A2 / V
    # Cp ~ prop(prop_sd) + add(add_sd)
    linCmt() ~ prop(prop_sd)
  })
}

# R CMD check gets confused about the rxode2 syntax, this is to suppress R CMD
# check NOTEs about "no visible binding for global variable"
utils::globalVariables(
  c(
    "eta_CL",
    "eta_V",
    "eta_V2",
    "eta_ka",
    "eta_Q",
    "dt",
    "A1",
    "A2",
    "A3",
    "/<-",
    "lag<-",
    "transit",
    "." # silence note from functions with dplyr chains that use .
  )
)
