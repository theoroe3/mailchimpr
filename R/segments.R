library("mailchimpr")
#### I think possibly rename to segments_get etc
### get initial segment info
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

#### get segment conditions
list_get_segment_conditions = function(api = NULL, list_id) {
  mailchimp_api = api_get()
  mailchimp_api_url = url_get()
  result = httr::GET(
    paste0(mailchimp_api_url, '/lists/', list_id, "/segments"),
    httr::authenticate("anystring", mailchimp_api)
  ) %>%
    httr::content()
  conditions = result$segments
  names = conditions %>%
    purrr::map_chr("name")

  conditions = result$segments %>%
    purrr::map( ~ c(.x[["options"]], .x[["name"]])) %>%
    purrr::map("conditions") %>%
    purrr::map(unlist) %>%
    purrr::map(tibble::enframe) %>%
    purrr::map2(.y = names, ~ dplyr::mutate(.x, segment = .y))

  more_than_one_con = conditions %>%
    purrr::map_lgl( ~ nrow(.x) > 1)

  conditions = conditions[more_than_one_con] %>%
    purrr::map( ~ cbind(.x, condition_no = rep(1:(nrow(
      .x
    ) / 4), each = 4))) %>%
    dplyr::bind_rows() %>%
    tidyr::pivot_wider(names_from = "name")

  return(conditions)
}

# new_seg_list = list(name = u("test"),
#      options = list(
#        match = u("any"),
#        conditions = list(
#          list(condition_type = u("TextMerge"),
#               field = u("FNAME"),
#               op = u("is"),
#               value = u("Roe")),
#          list(condition_type = u("TextMerge"),
#               field = u("LNAME"),
#               op = u("is"),
#               value = u("hey")))
#      ))
#
# conditions = tibble::tibble(
#   condition_type = c("TextMerge", "TextMerge"),
#   field = c("FNAME", "LNAME"),
#   op = c("is", "is"),
#   value = c("Theo", "Hey")
# )

#### create segment ------------------------------------------------------------
list_create_segment = function(api = NULL, list_id = list_id, conditions) {
  mailchimp_api = api_get()
  mailchimp_api_url = url_get()

  ### take each row i.e. each condition,
  ### unbox it and turn it into a list using transpose
  conditions = conditions %>%
    purrr::transpose() %>%
    rapply(jsonlite::unbox, how = "list")

  new_seg_json = list(
    name = jsonlite::unbox("test"),
    options = list(match = jsonlite::unbox("any"),
                   conditions = conditions)
  ) %>%
    jsonlite::toJSON()

  result = httr::POST(
    url = paste0(mailchimp_api_url, '/lists/', list_id, "/segments"),
    httr::authenticate("anystring", mailchimp_api),
    body = new_seg_json
  )
  invisible(result)
}


##### delete a list - works ------------------------------------------
list_delete_segment = function(api = NULL, list_id, segment_id) {
  mailchimp_api = api_get(api = api)
  mailchimp_api_url = url_get()
  result = httr::DELETE(
    url = paste0(
      mailchimp_api_url,
      '/lists/',
      list_id,
      "/segments/",
      segment_id
    ),
    httr::authenticate("anystring", mailchimp_api)
  )
  result
}

