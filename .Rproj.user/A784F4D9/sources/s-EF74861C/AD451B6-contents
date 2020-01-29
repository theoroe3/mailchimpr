library("mailchimpr")
library("tibble")
library("dplyr")
## lists --------------------------------------------------------------------

mailchimp_api = api_get()
mailchimp_api_url = url_get()
list_create(
  name = "Mailchimpr test",
  company = "Me",
  address1 = "8 South View",
  address2 = "Chester",
  city = "Newcastle",
  state = "County Durham",
  zip = "DH2 1PY",
  country = "UK",
  permission_reminder = "You are recieving this message because you signed up to our newsletter",
  from_name = "Theo Roe",
  from_email = "theo@jumpingrivers.com"
)

lists = list_get()
list_id = lists$id[1]

new_subs = tibble(email_address = c("Theo@theo.theo", "hi@hi.hi"),
                  FNAME = c("Theo", "hi"),
                  LNAME = c("Hey", "hey"))

list_batch_sub_result = list_batch_sub(list_id = list_id, new_subs = new_subs)


list_info = list_get_info(list_id = list_id)
list_members = list_get_members(list_id = list_id)

merge_field_create(list_id = list_id, name = "SOME NEW INFO", type = "text")
merge_fields = merge_field_get(list_id = list_id)
merge_field_id = merge_fields %>%
  filter(name == "SOME NEW INFO") %>%
  pull(merge_id)

merge_field_delete(list_id = list_id, merge_id = merge_field_id)
