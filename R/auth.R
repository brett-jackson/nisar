# Functions for handling authorization to access to NISAR data

# Login to earthdata
earthdata_auth <- function( user="", pass="" )
{
    # Store relevant .Renviron values
    env_username <- Sys.getenv( "EARTHDATA_USERNAME" )
    env_password <- Sys.getenv( "EARTHDATA_PASSWORD" )

    # Use environment variables for login credentials if needed
    if( user == "" & env_username != "" )
    {
        user <- env_username
    }
    if( pass == "" & env_password != "" )
    {
        pass <- env_password
    }

    # Use earthdatalogin for handling login logic
    earthdatalogin::edl_netrc( username=user, password=pass )
}

# Request Amazon S3 credentials
s3_auth <- function( edl_token="", user="", pass="" )
{
    s3_auth_req <- httr2::request( NISAR_S3_ENDPOINT )

    # Use EDL token if provided as a parameter
    if( edl_token != "" )
    {
        # Add EDL token to request header
        s3_auth_req <- s3_auth_req |>
            httr2::req_headers( Authorization = edl_token )
    }
    # Use EDL token if available in environment
    else if( (env_edl <- Sys.getenv("EDL_TOKEN")) != "" )
    {
        s3_auth_req <- s3_auth_req |>
            httr2::req_headers( Authorization = env_edl )
    }
    # Use user/pass if EDL token is unavailable
    else
    {
        earthdata_auth( user=user, pass=pass )
    }
        
    # Send request for S3 credentials
    s3_auth_resp <- s3_auth_req |> 
        httr2::req_perform()
}

