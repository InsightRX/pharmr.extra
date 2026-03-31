# $TABLE: https://pkpd-info.com/NONMEM/NM_guides/$table.htm
nonmem_reserved_variables <- c(
  # NONMEM system/input variables (always available in $TABLE)
  "EVID", "MDV", "CMT", "AMT", "RATE", "SS", "II", "ADDL",
  # Residual/output variables
  "PRED", "RES", "WRES",
  "NPRED", "NRES", "NWRES",
  "PREDI", "RESI", "WRESI",
  "CPRED", "CRES", "CWRES",
  "CPREDI", "CRESI", "CWRESI",
  "CIPRED", "CIRES", "CIWRES",
  "CIPREDI", "CIRESI", "CIWRESI",
  "NIPRED", "NIRES", "NIWRES",
  "IPREDI", "IRESI", "IWRESI",
  "IPRD", "IRS", "IWRS",
  "EPRED", "ERES", "EWRES",
  "ECWRES",
  "EIPRED", "EIRES", "EIWRES",
  "NPDE",
  "NPD",
  "OBJI"
)
