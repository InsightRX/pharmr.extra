# Write a NONMEM MPI parafile

Writes a NONMEM MPI parafile for within-model parallelization. The
parafile defines `[nodes]` workers; the actual node count can be
overridden on the nmfe command line via `[nodes]=N`.

## Usage

``` r
create_mpi_parafile(path, threads, filename = "parafile.pnm")
```

## Arguments

- path:

  directory in which to write the parafile.

- threads:

  number of worker nodes (default `[nodes]` value).

- filename:

  file name for the parafile. Default `parafile.pnm`.

## Value

the absolute path to the written parafile.

## Details

Layout matches the standard `mpilinux8.pnm` shipped with NONMEM:
`TRANSFER_TYPE=1` (MPI), `PARSE_TYPE=4`, and `mpirun` invoking
`./nonmem` on the master node with worker nodes inheriting the MPI
launch context. Requires MPI (e.g. OpenMPI) to be installed and `mpirun`
on the PATH at run time. FPI mode is not currently supported.
