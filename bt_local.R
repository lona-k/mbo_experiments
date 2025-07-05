library(batchtools)
library(data.table)
library(bbotk)
library(mlr3)
library(mlr3mbo)
library(mlr3batchmark)
library(mlr3learners)

reg = makeExperimentRegistry(
  file.dir = "./experiments",
  packages = c("mlr3verse", "mlr3mbo"),
  seed = 1
)

# reg = loadRegistry("./experiments")

branin = function(xdt) {
  y = bbotk::branin(xdt[["x1"]], xdt[["x2"]])
  data.table(y = y)
}

objective_b = ObjectiveRFunDt$new(
  fun = branin,
  domain = ps(x1 = p_dbl(-5, 10), x2 = p_dbl(0, 15)),
  codomain = ps(y = p_dbl(tags = "minimize"))
)

rastrigin = function(xdt) {
  D = ncol(xdt)
  y = 10 * D + rowSums(xdt^2 - (10 * cos(2 * pi * xdt)))
  data.table(y = y)
}

objective_r = ObjectiveRFunDt$new(
  fun = rastrigin,
  domain = ps(x1 = p_dbl(lower = -5.12, upper = 5.12),
              x2 = p_dbl(lower = -5.12, upper = 5.12)),
  codomain = ps(y = p_dbl(tags = "minimize"))
)

## Problems: branin and rastrigin

# branin
addProblem(
  name = "branin",
  data = list(obj = objective_b)
)

# rastrigin
addProblem(
  name = "rastrigin",
  data = list(obj = objective_r)
)


## Algorithms: mbo with mean and ei

mbo_algo = function(job, data, instance, acqf.id) {
  inst = oi(
    objective = data$obj,
    terminator = trm("evals", n_evals = 11)
  )

  opt = opt(
    "mbo",
    loop_function  = bayesopt_ego,
    surrogate      = srlrn(lrn("regr.km", control = list(trace = FALSE))),
    acq_function   = acqf(acqf.id),
    acq_optimizer  = acqo(
      opt("local_search", n_initial_points = 10),
      terminator = trm("evals")
    )
  )

  opt$optimize(inst)
  inst$archive$best()[, 1:3] # x1, x2, y
}


addAlgorithm(
  name = "mbo",
  fun = function(data, job, instance, acq_fun, ...) {
    mbo_algo(job, data, instance, acq_fun)
  }
)


# problem design
pdes = list(
  branin    = data.table(),
  rastrigin = data.table()
)

# algorithm design
ades = list(mbo = data.table(acq_fun = c("mean", "ei")))

addExperiments(pdes, ades, repls = 2)
summarizeExperiments(by = c("problem", "algorithm", "acq_fun"))


# test one job
id1 = head(findExperiments(prob.name = "branin", algo.name = "mbo"), 1)
print(id1)
testJob(id = id1)


# submit
submitJobs(reg = reg)
waitForJobs()

# look at results
rr = rbindlist(reduceResultsList(reg = reg), idcol = "job.id")
jobs = unwrap(getJobPars())
all = merge(jobs, rr)
print(all)

