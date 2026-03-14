library(pharmr)
devtools::load_all()

# ── Mock dataset: 3 patients, both DV (concentration) and LNDV (log-scale) ────

dataset <- data.frame(
  ID   = rep(1:3, each = 4),
  TIME = rep(c(0, 1, 4, 8), times = 3),
  AMT  = rep(c(100, 0, 0, 0), times = 3),
  EVID = rep(c(1, 0, 0, 0), times = 3),
  MDV  = rep(c(1, 0, 0, 0), times = 3),
  DV   = c(0, 8.2, 4.1, 1.9,
           0, 9.5, 4.8, 2.2,
           0, 7.8, 3.9, 1.8),
  LNDV = c(0, log(8.2), log(4.1), log(1.9),
           0, log(9.5), log(4.8), log(2.2),
           0, log(7.8), log(3.9), log(1.8))
)

print(dataset)

# ── Build model with both DV and LNDV in $INPUT ───────────────────────────────

data_file <- tempfile(fileext = ".csv")
write.csv(dataset, data_file, row.names = FALSE, quote = FALSE)

model_code <- paste0(
  "$PROBLEM Example with DV and LNDV\n",
  "$INPUT ID TIME AMT EVID MDV DV LNDV\n",
  "$DATA ", data_file, " IGNORE=@\n",
  "$SUBROUTINES ADVAN1 TRANS2\n",
  "$PK\n",
  "CL = THETA(1)\n",
  "V  = THETA(2)\n",
  "S1 = V\n",
  "$ERROR\n",
  "Y = F + EPS(1)\n",
  "$THETA (0, 5)  ; CL\n",
  "$THETA (0, 20) ; V\n",
  "$SIGMA 0.1\n",
  "$EST METHOD=1\n"
)

model <- pharmr::read_model_from_string(model_code)

# ── Inspect datainfo before changing DV ───────────────────────────────────────

cat("\n--- Before set_dv() ---\n")
cat("DV column  :", model$datainfo$dv_column$name, "\n")
cat("DV type    :", model$datainfo[["DV"]]$type, "\n")
cat("LNDV type  :", model$datainfo[["LNDV"]]$type, "\n")

# ── Switch to LNDV as the dependent variable ──────────────────────────────────

model_lndv <- set_dv(model, "LNDV")

cat("\n--- After set_dv(model, \"LNDV\") ---\n")
cat("DV column  :", model_lndv$datainfo$dv_column$name, "\n")
cat("DV type    :", model_lndv$datainfo[["DV"]]$type, "\n")   # demoted to 'unknown'
cat("LNDV type  :", model_lndv$datainfo[["LNDV"]]$type, "\n") # promoted to 'dv'

# ── Switch back to DV ─────────────────────────────────────────────────────────

model_dv <- set_dv(model_lndv, "DV")

cat("\n--- After set_dv(model_lndv, \"DV\") ---\n")
cat("DV column  :", model_dv$datainfo$dv_column$name, "\n")
cat("DV type    :", model_dv$datainfo[["DV"]]$type, "\n")     # back to 'dv'
cat("LNDV type  :", model_dv$datainfo[["LNDV"]]$type, "\n")   # back to 'unknown'
