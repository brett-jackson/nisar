# Useful functions for development and readability

# Verbose print: print if verbose, else do nothing. Cuts if statement sprawl.
vprint <- function(verbose, ...) {
  if (verbose) {
    printables <- list(...)
    for (printable in printables) {
      print(printable)
    }
  }
}
