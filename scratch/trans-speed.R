nodeBody <- quote({
  if (action != "pause")
    base::stop("pump asked to continue, but last action was ",
               action)
  pumpCont(...)
  while (action == "continue") {
    if (verbose)
      trace("pump: continue\n")
    action <<- "pause"
    cont <- list(pumpCont, pumpCont <<- NULL)[[1]]
    cont()
  }
})
nms <- c(windings = "stop_|windings", action = "stop_|action",
         pumpCont = "stop_|pumpCont",  verbose = "stop_|verbose",
         nonce = "stop_|nonce", value = "stop_|value",
         err = "stop_|err", stop = "stop_|stop",
         trace = "stop_|trace",  pumpCont = "stop_|pumpCont")
cnms <- c(stop = "stop_|stop", trace = "stop_|trace",
          pumpCont = "stop_|pumpCont" )
tenv <- translationEnv(cnms, nms)
nmse <- as.environment(lapply(nms, as.name))
cnmse <- as.environment(lapply(cnms, as.name))
senv <- as.environment(lapply(c(cnms, nms), as.name))

microbenchmark::microbenchmark(
  a={
    translatedBodyA1 <- trans(nodeBody, cnms, nms)
  },
  a2={
    translatedBodyA2 <- trans2(nodeBody, cnmse, nmse)
  },
  b = {
    translatedBodyB <- translateWithEnv(nodeBody, tenv)
  },
  c = {
    translatedBodyC <- substituteDirect(nodeBody, senv)
  }
)
