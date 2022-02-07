# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan <- drake_plan(
  adsl = read_excel(file_in("input/data.xlsx"), sheet = "ADSL") %>% setDT(),
  adlb = read_excel(file_in("input/data.xlsx"), sheet = "ADLB")%>% setDT(),

  data_check = check_data(adsl, adlb),
table_1 = make_table_1(adsl, adlb)
)
