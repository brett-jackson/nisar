# Functions for implementing API for NISAR data

# Login to earthdata
nisar_auth <- function(user=NULL, pass=NULL)
{
    # Store relevant .Renviron values
    env_username <- Sys.getenv("EARTHDATA_USERNAME")
    env_password <- Sys.getenv("EARTHDATA_PASSWORD")

    # Use environment variables for login credentials if needed
    if( is.null(user) & !is.null(env_username) )
    {
        user <- env_username
    }
    if( is.null(pass) & !is.null(env_password) )
    {
        pass <- env_password
    }

    # Use earthdatalogin for handling login logic
    earthdatalogin::edl_netrc(username=user,password=pass)
}
