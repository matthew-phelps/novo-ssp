# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan <- drake_plan(
  adsl = read_excel(file_in("input/data.xlsx"), sheet = "ADSL") %>% setDT(),
  adlb = read_excel(file_in("input/data.xlsx"), sheet = "ADLB")%>% setDT(),

table_1 = make_table_1(adsl, adlb),
fig_1 = make_fig_1(adlb),

report = rmarkdown::render(
  knitr_in("reports/slides.rmd"),
  output_file = file_out("reports/slides.html"),
  output_dir = "reports"
)
)
