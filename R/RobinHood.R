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
#' RH <- RobinHoodUpd("username", "password", mfa_code = "123456")
#' 
#' ## Alternatively, this allows the mfa code to be generated automatically using the code
#' ## Robinhood provides when you set up two factor authentication. This also logs into an IRA account
#' RH <- RobinHoodUpd("username", "password", totp_code = "JBSWY3DPEHPK3PXP", account_type = "ira_roth")
#' 
#'}
RobinHoodUpd <- function(username, password, mfa_code ="NA", totp_code = "NA", account_type = 'individual'){
  
  dvctkn = uuid::UUIDgenerate()
  mfa_cd = otp::TOTP$new(totp_code)$now()
  lst = list('device_token' = dvctkn,
             "client_id"="c82SH0WZOsabOXGP2sxqcj34FxkvfnWRZBKlBjFS",
             "create_read_only_secondary_token"='true',
             "expires_in"='86400',
             "grant_type"="password",
             "scope"="internal",
             "token_request_path"="/login",
             "username"=username,
             "password"=password,
             "try_passkeys"='false',
             'mfa_code'=mfa_cd,
             "request_id"=uuid::UUIDgenerate())
  
  
  rhurl = 'https://api.robinhood.com/oauth2/token/'
  headers <- c(Accept = "*/*", 
               'Content-Type' = "application/json"#,
               # 'User-Agent'= 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.1.1 Safari/605.1.15'
  )
  dta <- httr::POST(rhurl, body = toJSON(lst,auto_unbox = TRUE),
                    add_headers(headers))
  
  if(dta$status_code != 200){
    wrkflw = content(dta)$verification_workflow$id
    wrklst = list('device_token' = dvctkn,
                  'flow'= 'suv',
                  'input'=list('workflow_id'= wrkflw))
    
    newurl = "https://api.robinhood.com/pathfinder/user_machine/"
    wrkflw_pull <- httr::POST(newurl, body = toJSON(wrklst,auto_unbox = TRUE),add_headers(headers))
    wrkid = content(wrkflw_pull)$id
    
    inq_url = paste0("https://api.robinhood.com/pathfinder/inquiries/",wrkid,"/user_view/")
    inq_pull = GET(inq_url)
    chlng_id = content(inq_pull)$type_context$context$sheriff_challenge$id
    
    challenge_url = paste0("https://api.robinhood.com/challenge/",chlng_id,"/respond/")
    challenge_list = list('response'=mfa_cd)
    challenge_pull <- httr::POST(challenge_url, body = toJSON(challenge_list,auto_unbox = TRUE),add_headers(headers))
    content(challenge_pull)
    
    inquiries_payload = list("sequence"=0,"user_input"=list("status"="continue"))
    inq_final <- httr::POST(inq_url, body = toJSON(inquiries_payload,auto_unbox = TRUE),add_headers(headers))
    content(inq_final)
    
    dta <- httr::POST(rhurl, body = toJSON(lst,auto_unbox = TRUE),
                      add_headers(headers))
    
  }
  
  dta = content(dta)
  RH <- c(api_request = list(grant_type = "password", client_id = "c82SH0WZOsabOXGP2sxqcj34FxkvfnWRZBKlBjFS", 
                             device_token = uuid::UUIDgenerate(), scope = "internal", 
                             al_pk = "7F867EDC-C71B-467F-B0A1-8DCBA5D4D2E3"), 
          api_response = list(access_token = "000", refresh_token = "000", expires_in = 0, 
                              token_type = "000", scope = "000", mfa_code = "000", backup_code = "000"))
  
  RH$api_response.access_token <- dta$access_token
  RH$api_response.refresh_token <- dta$refresh_token
  RH$api_response.expires_in <- dta$expires_in
  RH$api_response.token_type <- dta$token_type
  RH$api_response.scope <- dta$scope
  RH$api_response.mfa_code <- dta$mfa_code
  RH$api_response.backup_code <- dta$backup_code
  
  url = 'https://api.robinhood.com/accounts/?default_to_all_accounts=true'
  token <- paste("Bearer", RH$api_response.access_token)
  new_data <- httr::GET(url, httr::add_headers(Accept = "application/json", 
                                               `Content-Type` = "application/json", Authorization = token))
  
  new_data <- RobinHood::mod_json(new_data, "fromJSON")
  url_account_id = new_data$results$url[new_data$results$brokerage_account_type==account_type]
  act_number = new_data$results$account_number[new_data$results$brokerage_account_type==account_type]
  act_type = new_data$results$brokerage_account_type[new_data$results$brokerage_account_type==account_type]
  RH <- c(RH, url = list(account_id = url_account_id, account_number = act_number,
                         act_typ = act_type))
  class(RH) <- "RobinHood"
  
  return(RH)
}
