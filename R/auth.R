# Functions for handling authorization to access to NISAR data

# Login to earthdata
earthdata_auth <- function(user = "", pass = "", verbose = FALSE) {
  # Store relevant environment values
  env_username <- Sys.getenv("EARTHDATA_USERNAME")
  env_password <- Sys.getenv("EARTHDATA_PASSWORD")

  # Use environment variables for login credentials if needed
  if (user == "" && env_username != "") {
    vprint(verbose, "Using EARTHDATA_USERNAME environment variable")
    user <- env_username
  }
  if (pass == "" && env_password != "") {
    vprint(verbose, "Using EARTHDATA_PASSWORD environment variable")
    pass <- env_password
  }

  # Use earthdatalogin for handling login logic
  vprint(verbose, "Logging on to earthdata")
  earthdatalogin::edl_netrc(username = user, password = pass)
}

#' Request Amazon S3 credentials
#'
#' Gets a AWS key and token for NISAR S3 bucket.
#'
#' @param edl_token EDL token value as string. Optional if already store in env.
#' @param user Username string, alternative to token when used with password.
#' @return pass Password string, alternative to token when used with username.
#' @return verbose Boolean to flag output verbosity.
#' @export
s3_auth <- function(edl_token = "", user = "", pass = "", verbose = FALSE) {
  s3_auth_req <- httr2::request(NISAR_S3_ENDPOINT)
  vprint(verbose, "Created S3 credential request")

  # Use EDL token if provided as a parameter
  if (edl_token != "") {
    # Add EDL token to request header
    s3_auth_req <- httr2::req_auth_bearer_token(s3_auth_req, edl_token)
    vprint(verbose, "Added EDL token from s3_auth parameter", s3_auth_req)
  } else if ((env_edl <- Sys.getenv("EDL_TOKEN")) != "") {
    # Use EDL token if available in environment
    s3_auth_req <- httr2::req_auth_bearer_token(s3_auth_req, env_edl)
    vprint(verbose, "Added EDL_TOKEN from environment variable", s3_auth_req)
  } else {
    # Use user/pass if EDL token is unavailable
    ed_auth <- earthdata_auth(user = user, pass = pass, verbose = verbose)
    vprint(verbose, "Performed user/pass auth from earthdatalogin", ed_auth)
  }

  # Send request for S3 credentials
  vprint(verbose, "Sending S3 credential request")
  s3_auth_resp <- httr2::req_perform(s3_auth_req)
  vprint(verbose, "Got response", s3_auth_resp)

  # Validate response, allow errors to propagate
  httr2::resp_check_status(s3_auth_resp,
    info = "S3 auth response indicates an error"
  )
  vprint(verbose, "Validated response")

  # Extract credentials from response
  httr2::resp_body_json(s3_auth_resp) |> s3_store_cred(verbose)
}

# Store S3 AWS credentials in session environment
s3_store_cred <- function(creds, verbose = FALSE) {
  key <- ""
  # Check that the S3 credentials are formatted as expected
  if ((key <- AWS_KEY_ID_IDX) %in% names(creds) &&
    (key <- AWS_KEY_VAL_IDX) %in% names(creds) &&
    (key <- AWS_TOKEN_IDX) %in% names(creds)) {
    # Store AWS credentials to session environment
    Sys.setenv("AWS_KEY_ID" = creds[[AWS_KEY_ID_IDX]])
    Sys.setenv("AWS_KEY_VAL" = creds[[AWS_KEY_VAL_IDX]])
    Sys.setenv("AWS_TOKEN" = creds[[AWS_TOKEN_IDX]])
    vprint(verbose, "Stored AWS credentials to session environment")
    if (AWS_EXPIRY_IDX %in% names(creds)) {
      Sys.setenv("AWS_EXPIRY" = creds[[AWS_EXPIRY_IDX]])
    } else {
      vprint(verbose, "Missing ", AWS_EXPIRY_IDX, " in credential response.")
    }
  } else {
    # Throw error if essential keys are missing
    vprint(verbose, key, "missing from S3 credential response")
    stop(paste(key, "missing from S3 credential response"))
  }
}
