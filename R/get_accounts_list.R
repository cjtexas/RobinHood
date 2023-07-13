#' Get data related to your RobinHood account in list format
#'
#' @param RH object of class RobinHood
#' @import httr magrittr
#' @export
#' @examples
#' \dontrun{
#' # Login in to your RobinHood account
#' RH <- RobinHood("username", "password", mfa_code = '123456')
#'
#' get_accounts_list(RH)
#'}
get_accounts_list = function (RH) 
{
  ## Change this line
  url <- paste0('https://bonfire.robinhood.com/accounts/',RH$url.account_number,'/unified/')
  url <- paste0(RobinHood::api_endpoints("accounts"),RH$url.account_number,'/')
  token <- paste("Bearer", RH$api_response.access_token)
  dta <- GET(url, add_headers(Accept = "application/json", 
                              `Content-Type` = "application/json", Authorization = token))
  httr::stop_for_status(dta)
  dta <- RobinHood::mod_json(dta, "fromJSON")
  for (i in seq_along(dta)) {
    dta[[i]][is.null(dta[[i]])] <- "NA"
  }
  for (i in seq_along(dta$margin_balances)) {
    dta$margin_balances[[i]][is.null(dta$margin_balances[[i]])] <- "NA"
  }
  for (i in seq_along(dta$instant_eligibility)) {
    dta$instant_eligibility[[i]][is.null(dta$instant_eligibility[[i]])] <- "NA"
  }
  return(dta)
}
