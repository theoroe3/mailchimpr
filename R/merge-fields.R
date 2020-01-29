#' @title Grab the merge fields of a list
#'
#' @description Grab the merge fields of a list.
#'
#' @param api Character. Your private api key. If api is `NULL`, the environment variable `Sys.getenv("mailchimp_api")` is used.
#' @param list_id Character. The ID of a list. See `get_lists()`.
#'
#' @export
merge_field_get = function(api = NULL, list_id){
  mailchimp_api = api_get(api = api)
  mailchimp_api_url  = url_get()
  result = httr::GET(
    paste0(mailchimp_api_url, '/lists/', list_id, "/merge-fields"),
    httr::authenticate("anystring", mailchimp_api)
  ) %>%
    httr::content() %>%
    magrittr::extract2("merge_fields") %>%
    purrr::map_df(magrittr::extract, c("merge_id", "name", "tag", "type", "required"))

  return(result)
}

#' @title Create a merge field for a list
#'
#' @description Create a merge field for a list
#'
#' @param api Character. Your private api key. If api is `NULL`, the environment variable `Sys.getenv("mailchimp_api")` is used.
#' @param list_id Character. The ID of a list. See `list_get()`.
#' @param name Character. The name of the merge_field. See `merge_field_get()`.
#' @param type Character. The type of the merge field. Can be one of "text", "number", "address",
#' "phone", "date", "url", "image", "url", "radio", "dropdown", "birthday" or "zip".
#' @param required Boolean. Is this merge field required?
#'
#' @export
merge_field_create = function(api = NULL,
                              list_id,
                              name,
                              type = c(
                                "text",
                                "number",
                                "address",
                                "phone",
                                "date",
                                "url",
                                "image",
                                "url",
                                "radio",
                                "dropdown",
                                "birthday",
                                "zip"
                              ),
                              required = FALSE) {
  mailchimp_api = api_get(api = api)
  mailchimp_api_url  = url_get()

  tag = name %>%
    stringr::str_remove_all(" ") %>%
    stringr::str_to_upper()
  message("Setting merge field with name ", name, " to ", tag)

  if(length(type) > 1){
    type = type[1]
  }

  new_merge_field_json = list(
    name = name,
    tag = tag,
    type = type,
    required = required
  ) %>%
    purrr::map(jsonlite::unbox) %>%
    jsonlite::toJSON()

  result = httr::POST(
    paste0(mailchimp_api_url, '/lists/', list_id, "/merge-fields"),
    httr::authenticate("anystring", mailchimp_api),
    body = new_merge_field_json
  )

  invisible(result)
}

#' @title Delete a merge field from a list
#'
#' @description Delete a merge field form a list
#'
#' @param api Character. Your private api key. If api is `NULL`, the environment variable `Sys.getenv("mailchimp_api")` is used.
#' @param list_id Character. The ID of a list. See `list_get()`.
#' @param merge_id Character. The ID of a merge_field See `merge_field_get()`.
#'
#' @export
merge_field_delete = function(api = NULL, list_id, merge_id){
  mailchimp_api_url = url_get()
  mailchimp_api = api_get(api = api)

  result = httr::DELETE(
    paste0(mailchimp_api_url, '/lists/', list_id, "/merge-fields/", merge_id),
    httr::authenticate("anystring", mailchimp_api)
  )

  if (!result$status_code %in% c(200, 204)) {
    cat(crayon::red("Merge field delete DELETE request unsuccessful."))
  } else{
    cat(crayon::green("Merge field delete DELETE request successful!"))
  }

  invisible(result)
}
