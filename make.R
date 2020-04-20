source("R/packages.R")
source("R/utilities.R")
source("R/functions.R")
source("R/plan.R")

make(
  plan,
  verbose = 2,
  parallelism = 'future',
  jobs = 3
)