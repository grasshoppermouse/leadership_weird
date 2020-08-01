source("R/packages.R")
source("R/utilities.R")
source("R/functions.R")
source("R/graph_functions.R")
source("R/plan.R")

set.seed(456789876)

make(
  plan,
  verbose = 2,
  parallelism = 'future',
  jobs = 3
)