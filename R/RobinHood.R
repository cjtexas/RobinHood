#' RobinHood Account Authentication
#'
#' This function returns an object of S3 class RobinHood and establishes a connection to a RobinHood account.
#' It is a required input for every other function in the package.
#'
#' @param username (string) account email address
#' @param password (string) password
#' @param mfa_code (string) mfa_code provided by your authentication app (required if mfa is enabled)
#' @param totp_code (string) the Code Robinhood provides to set in the authenticator app
#' @param account_type (string) for Retirement accounts, use 'ira_roth' or 'ira_traditional'
#' @import httr magrittr
#' @export
#' @examples
#' \dontrun{
#' 
#' ## Logging into the primary account using an mfa code from authenticator
#' RH <- RobinHood("username", "password", mfa_code = "123456")
#' 
#' ## Alternatively, this allows the mfa code to be generated automatically using the code
#' ## Robinhood provides when you set up two factor authentication. This also logs into an IRA account
#' RH <- RobinHood("username", "password", totp_code = "JBSWY3DPEHPK3PXP", account_type = "ira_roth")
#' 
#'}
RobinHood <- function(username, password, mfa_code ="NA", totp_code = "NA", account_type = 'individual'){
  
  if(mfa_code == "NA" & totp_code =="NA") {
    stop("Both mfa_code and totp_code cannot be 'NA'. Please pass a valid code in one of the variables")
  }
  
  if(totp_code != "NA") {
    p <- otp::TOTP$new(totp_code)
    mfa_code <- p$now()
    Sys.sleep(0.25)
    print(p$verify(mfa_code))
  }
  
  if(!(account_type %in% c('individual','ira_roth','ira_traditional'))) {
    
    warning("Account type not: 'individual','ira_roth','ira_traditional'. Defaulting to 'individual'")
    account_type = 'individual'
  }
  
  RH <- RobinHood::api_login(username,password,mfa_code)
  
  ### Change most of this
  url = 'https://api.robinhood.com/accounts/?default_to_all_accounts=true'
  token <- paste("Bearer", RH$api_response.access_token)
  dta <- httr::GET(url, httr::add_headers(Accept = "application/json", 
                                          `Content-Type` = "application/json", Authorization = token))
  httr::stop_for_status(dta)
  dta <- RobinHood::mod_json(dta, "fromJSON")
  url_account_id = dta$results$url[dta$results$brokerage_account_type==account_type]
  act_number = dta$results$account_number[dta$results$brokerage_account_type==account_type]
  act_type = dta$results$brokerage_account_type[dta$results$brokerage_account_type==account_type]
  RH <- c(RH, url = list(account_id = url_account_id, account_number = act_number,
                         act_typ = act_type))
  if (is.null(RH$api_response.access_token)) {
    cat("Login not successful, check username and password.")
  }
  class(RH) <- "RobinHood"
  return(RH)
}
