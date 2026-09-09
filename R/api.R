# Functions for implementing API for NISAR data

# Get Amazon s3 credentials
get_s3_credentials <- function(user, pass)
{
    http_get <- httr2::request(NISAR_S3_ENDPOINT)
    response <- httr2::req_perform(http_get)
    return(response)
}
