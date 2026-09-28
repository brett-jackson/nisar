# Functions for handling authorization to access to NISAR data

# Login to earthdata
earthdata_auth <- function(user = "", pass = "") {
  # Store relevant .Renviron values
  env_username <- Sys.getenv("EARTHDATA_USERNAME")
  env_password <- Sys.getenv("EARTHDATA_PASSWORD")

  # Use environment variables for login credentials if needed
  if (user == "" && env_username != "") {
    user <- env_username
  }
  if (pass == "" && env_password != "") {
    pass <- env_password
  }

  # Use earthdatalogin for handling login logic
  earthdatalogin::edl_netrc(username = user, password = pass)
}

# Request Amazon S3 credentials
s3_auth <- function(edl_token = "", user = "", pass = "", verbose = FALSE) {
  vprint(verbose, "Creating S3 credential request")
  s3_auth_req <- httr2::request(NISAR_S3_ENDPOINT)

  # Use EDL token if provided as a parameter
  if (edl_token != "") {
    # Add EDL token to request header
    s3_auth_req <- s3_auth_req |>
      httr2::req_auth_bearer_token(edl_token)
    vprint(verbose, "Using EDL_TOKEN parameter", s3_auth_req)
  } else if ((env_edl <- Sys.getenv("EDL_TOKEN")) != "") {
    # Use EDL token if available in environment
    s3_auth_req <- s3_auth_req |>
      httr2::req_auth_bearer_token(env_edl)
    vprint(verbose, "Using EDL_TOKEN environment variable", s3_auth_req)
  } else {
    # Use user/pass if EDL token is unavailable
    ed_auth <- earthdata_auth(user = user, pass = pass)
    vprint(verbose, "Using user/pass", ed_auth)
  }

  # Send request for S3 credentials
  vprint(verbose, "Sending S3 credential request")
  s3_auth_req |> httr2::req_perform()
}
