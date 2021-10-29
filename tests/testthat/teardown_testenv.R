
# remove test data and restore previous global env
message("Restoring environment")
rm(list=ls(envir = globalenv()), envir=globalenv())
envDataFn <- here::here('tests/preTestEnv.RData')
if (file.exists(envDataFn)) {
  # if file exists reload starting data, and remove the file.
  load(envDataFn, envir = globalenv())
  file.remove(envDataFn)
}
