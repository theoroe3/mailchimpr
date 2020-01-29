#' @title Grab a tibble of your mailchimp lists.
#'
#' @description Grab a tibble of your mailchimp lists.
#'
#' @param api Character. Your private api key. If api is `NULL`, the environment variable `Sys.getenv("mailchimp_api")` is used.
#'
#' @export
list_get = function(api = NULL) {
  mailchimp_api = api_get(api)
  mailchimp_api_url = url_get()
  result = httr::GET(
    paste0(mailchimp_api_url, '/lists'),
    httr::authenticate("anystring", mailchimp_api)
  ) %>%
    httr::content()
  lists = purrr::map_df(result$lists, ~ c(.x["id"], .x["name"]))
  return(lists)
}

#' @title Returns information about the members of a particular list.
#'
#' @description Returns information about the members of a particular list, such as EMAIL, merge fields and locations.
#'
#' @param api Character. Your private api key. If api is `NULL`, the environment variable `Sys.getenv("mailchimp_api")` is used.
#' @param list_id Character. The ID of a list. See `get_lists()`.
#' @param count Numeric. The number of members you want information about. Max 100 due to API rules.
#'
#' @export
list_get_members = function(api = NULL, list_id, count = 10) {
  mailchimp_api = api_get(api)
  mailchimp_api_url = url_get()
  string = paste0(
    mailchimp_api_url,
    '/lists/',
    list_id,
    "/members?count=",
    count
  )
result = httr::GET(string,
                   httr::authenticate("anystring", mailchimp_api)) %>%
  httr::content()
members = result$members
members = cbind(EMAIL = purrr::map_chr(members, ~.x$email_address),
                purrr::map_df(members, ~.x$merge_fields),
                purrr::map_df(members, "location") %>%
                  dplyr::select(.data$latitude, .data$longitude, .data$country_code),
                id = purrr::map_chr(members, "id"),
                timestamp_opt = purrr::map_chr(members, "timestamp_opt"),
                timestamp_signup = purrr::map_chr(members, "timestamp_signup"),
                member_rating = purrr::map_chr(members, "member_rating"),
                purrr::map_df(members, ~.x$stats[1:2]),
                purrr::map_df(members, ~.x$stats$ecommerce_data))
return(members)
}

#' @title Returns information about a particular list.
#'
#' @description Returns information about a particular list
#'
#' @param api Character. Your private api key. If api is `NULL`, the environment variable `Sys.getenv("mailchimp_api")` is used.
#' @param list_id Character. The ID of a list. See `get_lists()`.
#'
#' @export
list_get_info = function(api = NULL, list_id) {
  mailchimp_api = api_get(api)
  mailchimp_api_url = url_get()
  string = paste0(
    mailchimp_api_url,
    '/lists/',
    list_id
  )
  result = httr::GET(string,
                     httr::authenticate("anystring", mailchimp_api)) %>%
    httr::content()
  info = purrr::map(result$stats, as.character) %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = .data$value) %>%
    tidyr::pivot_wider(names_from = .data$name, values_from = .data$value) %>%
    cbind(name = result$name) %>%
    tibble::as_tibble()
  return(info)
}


#' @title Create list for one new list subscriber
#'
#' @description Create list for one new list subscriber.
#' This is in prep for creating the JSON for multiple new subscribers in `list_batch_sub()`
#'
#' @param api Character. Your private api key. If api is `NULL`, the environment variable `Sys.getenv("mailchimp_api")` is used.
#' @param list_id Character. The ID of a list. See `get_lists()`.
#' @param new_sub Data Frame (or tibble). Email column must be named `email_address`.
#' Merge fields must match merge fields in `list_get_merge_fields()`
#'
#' @return A list
#'
list_create_new_sub_list = function(api = NULL, list_id, new_sub) {
  mailchimp_api = api_get(api = api)
  mailchimp_api_url = url_get()

  if(!"email_address" %in% colnames(new_sub)){
    stop("Email column must be named email_address, not my choosing!")
  }

  user_merge_fields = colnames(new_sub)[-(colnames(new_sub) == "email_address")]
  list_merge_fields = merge_field_get(list_id = list_id) %>%
    dplyr::pull(.data$tag)

  if (!is.null(list_merge_fields)){
    if (!all(user_merge_fields %in% list_merge_fields)) {
      stop("You have submitted a merge field that does not exist in your list. \n
           You must add the merge field before you can submit the information. \n
           See merge_field_create() and merge_field_get() for more info.")
    }
  } else{
    if(!all(user_merge_fields %in% c("FNAME", "LNAME", "ADDRESS", "PHONE"))){
      stop("Your list has 0 members and therefore you can only add information\n
           relating to email, first name, last name, address, phone number.\n
           See merge_field_create() and merge_field_get() for more info.")
    }
  }

  new_merge_fields = new_sub %>%
    dplyr::select(-.data$email_address) %>%
    purrr::map(jsonlite::unbox)
  new_sub = new_sub %>%
    dplyr::select(.data$email_address) %>%
    dplyr::mutate(status = "subscribed") %>%
    purrr::map(jsonlite::unbox)

  new_sub = c(new_sub,
              merge_fields = list(new_merge_fields))

  return(new_sub)
}

#' @title Batch subscribe members to a list
#'
#' @description Batch subscribe members to a list.
#'
#' @param api Character. Your private api key. If api is `NULL`, the environment variable `Sys.getenv("mailchimp_api")` is used.
#' @param list_id Character. The ID of a list. See `get_lists()`.
#' @param new_subs Data Frame (or tibble). Email column must be named `email_address`.
#' Merge fields must match merge fields in `list_get_merge_fields()`
#'
#'
#' @export
list_batch_sub = function(api = NULL, list_id, new_subs) {
  mailchimp_api = api_get(api = api)
  mailchimp_api_url  = url_get()

  new_subs_list = new_subs %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidyr::nest(data = c(-.data$id)) %>%
    dplyr::select(-.data$id) %>%
    dplyr::mutate(new_sub_list = purrr::map(
      .data$data,
      ~ list_create_new_sub_list(list_id = list_id,
                                 new_sub = .x)
    )) %>%
    dplyr::pull(.data$new_sub_list)

  new_mem_json = jsonlite::toJSON(list(
    members = new_subs_list,
    update_existing = jsonlite::unbox(TRUE)
  ))

  result = httr::POST(
    paste0(mailchimp_api_url, '/lists/', list_id),
    httr::authenticate("anystring", mailchimp_api),
    body = new_mem_json
  )

  if (result$status_code != 200) {
    cat(crayon::red("Batch sub post request unsuccessful."))
  } else{
    cat(crayon::green("Batch sub post request successful!"))
  }

  invisible(result)
}

#' @title Create a list/audience
#'
#' @description Create a list/audience.
#'
#' @param api Character. Your private api key. If api is `NULL`, the environment variable `Sys.getenv("mailchimp_api")` is used.
#' @param name Name of the list.
#' @param company Character. Company name associated with the list.
#' @param address1 Character. First line of the address of the associated company.
#' @param address2 Character. Second line of the address of the associated company.
#' @param city Character. City of the associated company.
#' @param state Character. State of the associated company.
#' @param zip Character. Zip/post code of the associated company.
#' @param country Character Country of the associated company.
#' @param phone Character. Phone number of the associated company.
#' @param permission_reminder Character. Permission reminder for subscribers of the list.
#' @param from_name Character. Default from-name for emails to subscribers of the list.
#' @param from_email Character. Default from-email for emails to subscribers of the list.
#' @param subject Character. Default from-subject for emails to subscribers of the list.
#' @param language Character. Default language for emails to subscribers of the list.
#' @param email_type_option Boolean. See mailchimp api reference.
#' @export
list_create = function(api = NULL,
                       name,
                       company,
                       address1,
                       address2 = "",
                       city,
                       state,
                       zip,
                       country,
                       phone = "",
                       permission_reminder,
                       from_name,
                       from_email,
                       subject = "",
                       language = "en",
                       email_type_option = FALSE) {

  mailchimp_api_url = url_get()
  mailchimp_api = api_get(api = api)

  args = as.list(environment())
  args = purrr::map(args, jsonlite::unbox)

  contact = args[c("company",
                   "address1",
                   "address2",
                   "city",
                   "state",
                   "zip",
                   "country")]

  campaign_defaults = args[c("from_name",
                             "from_email",
                             "subject",
                             "language")]

  new_list_info = list(
    name = args$name,
    contact = contact,
    permission_reminder = args$permission_reminder,
    campaign_defaults = campaign_defaults,
    email_type_option = args$email_type_option
  )

  new_list_json = jsonlite::toJSON(new_list_info)

  result = httr::POST(
    paste0(mailchimp_api_url, '/lists'),
    httr::authenticate("anystring", mailchimp_api),
    body = new_list_json
  )

  if (result$status_code != 200) {
    cat(crayon::red("List create post request unsuccessful."))
  } else{
    cat(crayon::green("List create post request successful!"))
  }

  invisible(result)
}

#' @title Delete a list/audience
#'
#' @description Delete a list/audience.
#'
#' @param api Character. Your private api key. If api is `NULL`, the environment variable `Sys.getenv("mailchimp_api")` is used.
#' @param list_id Character. The ID of a list. See `get_lists()`.
#'
#' @export
list_delete = function(api = NULL, list_id){
  mailchimp_api_url = url_get()
  mailchimp_api = api_get(api = api)

  result = httr::DELETE(
    paste0(mailchimp_api_url, '/lists/', list_id),
    httr::authenticate("anystring", mailchimp_api)
  )

  if (!result$status_code %in% c(200, 204)) {
    cat(crayon::red("List delete DELETE request unsuccessful."))
  } else{
    cat(crayon::green("List delete DELETE request successful!"))
  }

  invisible(result)
}

#' @title Get the segments for a list
#'
#' @description Get the segments for a list
#'
#' @param api Character. Your private api key. If api is `NULL`, the environment variable `Sys.getenv("mailchimp_api")` is used.
#' @param list_id Character. The ID of a list. See `get_lists()`.
#'
#' @export
list_get_segments = function(api = NULL, list_id) {
  mailchimp_api = api_get()
  mailchimp_api_url = url_get()
  result = httr::GET(
    paste0(mailchimp_api_url, '/lists/', list_id, "/segments"),
    httr::authenticate("anystring", mailchimp_api)
  ) %>%
    httr::content()

  segments = result$segments %>%
    purrr::map_df(~ c(.x["id"], .x["name"],
                      .x["member_count"], .x["created_at"],
                      .x["updated_at"]))

  return(segments)
}

