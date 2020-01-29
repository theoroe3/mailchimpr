#' @title Set your mailchimp api key as an environment variable.
#'
#' @description A function to set your mailchimp api key as an environment variable in your .Renviron
#'
#' @param api Character. Your private api key.
#' @examples
#' api_set(api = "Xhui89090sjioH77aknovfdn")
#'
#' @export
api_set = function (api){
  if (!is.null(api) & nchar(api) != 0){
    api = Sys.setenv(x = c(mailchimp_api = api))
    message("Mailchimp api key successfully added to environment variables.")
  }
    return(TRUE)
  }

#' @title Grab your mailchimp api key.
#'
#' @description A function to grab the mailchimp api key from your .Renviron.
#'
#' @param api Character. Your private api key. If api is `NULL`, the environment variable `Sys.getenv("mailchimp_api")` is used.
#' @examples
#' api_get()
#'
#' @export
api_get = function (api = NULL){
  if (is.null(api)){
    api = Sys.getenv("mailchimp_api")
  }
  if (nchar(api) != 0){
    return(api)
  }
  stop("Invalid api key.", call. = FALSE)
}

#' @title Get the mailchimp api URL
#'
#' @description Get the mailchimp api URL
#'
#' @examples
#' url_get()
#'
#' @export
url_get = function (){
  return("https://us4.api.mailchimp.com/3.0")
}
