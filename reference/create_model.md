# Create model

This is essentially a wrapper around the model-creation and
-modification functionality in pharmr/Pharmpy.

## Usage

``` r
create_model(
  route = c("auto", "oral", "sc", "im", "extravascular", "iv"),
  lag_time = FALSE,
  n_transit_compartments = 0,
  bioavailability = FALSE,
  n_cmt = 1,
  elimination = c("linear", "michaelis-menten"),
  iiv = "all",
  iiv_type = "exp",
  tmdd_type = NULL,
  tmdd_dv_types = list(drug = 1, target = 2),
  metabolite = FALSE,
  ruv = c("additive", "proportional", "combined", "ltbs"),
  covariates = NULL,
  scale_observations = NULL,
  data = NULL,
  name = NULL,
  estimation_method = "foce",
  estimation_options = list(),
  uncertainty_method = c("sandwich", "smat", "rmat", "efim", "none"),
  sir_options = list(niter = -1),
  blq_method = NULL,
  lloq = NULL,
  tool = c("nonmem", "nlmixr", "nlmixr2"),
  tables = c("fit"),
  full_tables = FALSE,
  auto_init = TRUE,
  auto_stack_encounters = TRUE,
  mu_reference = "auto",
  settings = list(),
  verbose = FALSE
)
```

## Arguments

- route:

  route of administration, either `oral` or `iv`

- lag_time:

  add a lag time, default is `FALSE`

- n_transit_compartments:

  number of transit-compartments for absorption model. Default is `0`.

- bioavailability:

  Add a bioavailability parameter? Default is `FALSE`. Will add using a
  logit function.

- n_cmt:

  number of elimination and distribution compartments. Default is 1,
  i.e. no peripheral distributions.

- elimination:

  elimination type, either `linear` or `michaelis-menten`.

- iiv:

  either `character` or a `list` object. If `character`, should be
  either "basic" (only CL and V parameters) or "all" (IIV on all
  parameters). If specified as a list object, it should contain the IIV
  magnitude (on SD scale) for parameters and potential correlations
  specified using a tilde, e.g.
  `list("CL" = 0.2, "V" = 0.3, "CL~V" = 0.1)`.

- iiv_type:

  either `character` or `list`. If character, one of
  `c("exp", "add", "prop", "log", "re_log")`. If `list`, should specify
  for each parameter the effect type, e.g.
  `list(CL = "add", V = "exp")`. Default is `"exp"` for all.

- tmdd_type:

  if a TMDD model structure is desired, can choose one of `full`, `ib`,
  `crib`, `cr`, `qss`, or `mmapp`. See Pharmpy/pharmr documentation for
  details.

- metabolite:

  either `TRUE`/`FALSE` or a `list`. If logical, determines if a
  metabolite compartment be added (with input from central using linear
  kinetics). In this case, DVID in the dataset should be set to 1 for
  the parent compound and 2 for the metabolite. If argument is a `list`
  object, it will require list elements `dvid_drug` (integer) and
  `presystemic` (logical) to be specified. `dvid_drug` specified the
  index number for parent drug in the `DVID` column in the dataset.
  `presystemic` determines whether the metabolite is formed before
  absorption (`TRUE`), or after systemic absorption (`FALSE`).

- ruv:

  one of `proportional`, `additive`, or `combined`.

- covariates:

  list of parameter-covariate effects, e.g.
  `list(CL = list(WT = "pow", CRCL = "lin"), V = list(WT = "pow")`
  Values in list need to match one of the effects allowed by pharmpy.

- scale_observations:

  scale observations by factor, e.g. due to unit differences between
  dose and concentration. E.g. `scale_observations = 1000` will add
  `S1 = V/1000` (for a 1-compartment model) to NONMEM code.

- data:

  data.frame as input to NONMEM / nlmixr.

- name:

  name of model

- estimation_method:

  character vector of one or more estimation methods. A single method
  (e.g. `"foce"`) sets one estimation step; a vector (e.g.
  `c("saem", "imp")`) creates sequential estimation steps. Available
  methods: `"fo"`, `"foce"`, `"its"`, `"impmap"`, `"imp"`, `"saem"`.

- estimation_options:

  options for estimation method(s). For a single estimation method,
  specify a flat list, e.g. `list(NITER = 50)`. For multiple estimation
  methods, specify a named list of lists where names match the method
  names, e.g. `list(SAEM = list(NBURN = 500), IMP = list(NITER = 10))`.
  Methods not listed will use their default options. If a flat list is
  provided with multiple estimation methods, it applies to the first
  step only.

- uncertainty_method:

  Compute uncertainty for parameter estimations. One of `sandwich`
  (default), `smat`, `fmat`, `efim`, or `none`.

- sir_options:

  options for running SIR in covariance step. A list with options
  `niter` and `samples`. If `niter <= 0` (default), SIR will not be run.

- blq_method:

  method for handling data below the limit of quantification. Available
  options are `m1`, `m3`, `m4`, `m5`, `m6`, `m7`, as described by Beal
  et al. Default is no handling of BLQ data (`NULL`).

- lloq:

  (optional) a numeric value specifying the limit of quantification for
  observations. Will be disregarded if an `LLOQ` column is in the
  dataset.

- tool:

  output model type, either `nonmem` or `nlmixr`

- tables:

  which pre-specified tables to add, defaults to `parameters` and `fit`
  tables.

- full_tables:

  For the default tables, should all input columns from be included in
  the output tables? Default `FALSE`.

- auto_init:

  automatically update initial estimates to reasonable values based on a
  crude assessment of the PK data. Default is `TRUE`.

- auto_stack_encounters:

  detects if TIME within an individual is decreasing from one record to
  another, which NONMEM cannot handle. If this happens, it will add a
  reset event (EVID=3) at that time, and increase the TIME for
  subsequent events so that NONMEM does not throw an error. It will
  increase the time for the next encounter to the maximum encounter
  length across all subjects in the dataset (rounded up to 100). If no
  decreasing TIME is detected, nothing will be done (most common case).
  This feature is useful e.g. for crossover trials when data on the same
  individual ispresent but is included in the dataset as time-after-dose
  and not actual time since first overall dose.

- mu_reference:

  Control mu-referencing of the model. `"auto"` (default) applies
  mu-referencing automatically when `estimation_method = "saem"`. `TRUE`
  always applies mu-referencing. `FALSE` never applies it.

- settings:

  additional settings for model creation and model estimation. TBD

- verbose:

  verbose output?

## Value

TODO
