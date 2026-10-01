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

# Find the remaining seconds until an S3 token expires
s3_ttl <- function() {
  aws_expiry <- as.POSIXct(Sys.getenv("AWS_EXPIRY"),tryFormats=c("%Y-%m-%d %H:%M:%S"),tz="UTC")
  as.numeric(difftime(aws_expiry, Sys.time(), units = "secs"))
}