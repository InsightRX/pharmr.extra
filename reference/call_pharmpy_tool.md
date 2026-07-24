# Generic function for running a pharmpy tool, like bootstrap, or modelsearch. A separate function is available for `fit()`

Generic function for running a pharmpy tool, like bootstrap, or
modelsearch. A separate function is available for `fit()`

## Usage

``` r
call_pharmpy_tool(
  id,
  model = NULL,
  results = NULL,
  tool = NULL,
  folder = NULL,
  clean = TRUE,
  verbose = TRUE,
  force = FALSE,
  options = list(),
  remove_tables = TRUE,
  uppercase_mfl = TRUE
)
```

## Arguments

- id:

  model id. Optional. If not specified, will generate random modelfit
  id. The `id` will be used to create the run folder.

- model:

  Pharmpy model object, preferably created using
  [`create_model()`](https://insightrx.github.io/pharmr.extra/reference/create_model.md).

- results:

  TODO

- tool:

  TODO

- folder:

  TODO

- clean:

  if one or more run folders exists for the tool, do we want to remove
  them first?

- verbose:

  verbose output?

- force:

  TODO

- options:

  list of arguments pass on to `tool` as argument. Documentation for
  available arguments for each Pharmpy tool can be found here:
  https://pharmpy.github.io/latest/mfl.html. For `tool = "structsearch"`
  with `type = "tmdd"`, the pharmr.extra-specific `kd` element (target
  dissociation constant seed, in concentration units) seeds the QSS
  candidates' target parameters via
  [`seed_tmdd_results()`](https://insightrx.github.io/pharmr.extra/reference/seed_tmdd_results.md)
  and is *not* forwarded to Pharmpy.

- remove_tables:

  if `TRUE` (default), removes all `$TABLE` records from the model
  before passing it to the Pharmpy tool.

- uppercase_mfl:

  if `TRUE` (default), uppercases the model's `$INPUT` / datainfo /
  dataset column names and the `options$search_space` string before
  calling the Pharmpy tool. Works around Pharmpy's MFL parser, which
  unconditionally uppercases every identifier in a search_space and then
  fails the case-sensitive lookup against datainfo (see
  <https://github.com/pharmpy/pharmpy/issues/4576>). Set to `FALSE` to
  disable.

## Value

fit object

## Examples

``` r
if (FALSE) { # \dontrun{
# Run 200 bootstrap samples on a fitted model
bs <- call_pharmpy_tool(
  id      = "run1",
  model   = model,
  results = results,
  tool    = "bootstrap",
  options = list(samples = 200)
)

# Inspect parameter estimates (one row per sample)
head(as.data.frame(bs$parameter_estimates))

# Plot distributions and overlay original estimates
orig <- setNames(results$parameter_estimates$estimates,
                 results$parameter_estimates$parameter)
plot_bootstrap(bs, original_estimates = orig)
} # }
```
