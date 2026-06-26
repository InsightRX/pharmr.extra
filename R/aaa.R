# $TABLE: https://pkpd-info.com/NONMEM/NM_guides/$table.htm
nonmem_reserved_variables <- c(
  # The independent variable is always available in $TABLE, regardless of
  # whether a literal `TIME` column appears in $INPUT (e.g. `$INPUT TAFD=TIME`
  # renames the data column but `TIME` remains a valid $TABLE label).
  "TIME",
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

# Map of pharmpy `datainfo` column types to their canonical NONMEM reserved
# $INPUT label. NONMEM lets a renamed data column be referenced in $TABLE by
# this reserved synonym (e.g. `$INPUT TAFD=TIME` is stored by pharmpy as a
# column named `TAFD` of type `idv`, but `TIME` is still a valid $TABLE label).
# Used by `check_nm_table_variables()` to resolve such synonyms.
nonmem_type_synonyms <- c(
  "id" = "ID",
  "idv" = "TIME",
  "dv" = "DV",
  "dose" = "AMT",
  "rate" = "RATE",
  "mdv" = "MDV",
  "event" = "EVID",
  "compartment" = "CMT"
)
