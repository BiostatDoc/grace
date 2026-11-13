scripts <- sort(list.files("R", pattern = "[.]R$", full.names = TRUE))
for (script in scripts) {
  source(script, echo = TRUE, max.deparse.length = Inf)
}
