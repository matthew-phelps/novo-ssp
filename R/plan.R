# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan <- drake_plan(
  adsl = read_excel(file_in("input/data.xlsx"), sheet = "ADSL") %>% setDT(),
  adlb = read_excel(file_in("input/data.xlsx"), sheet = "ADLB") %>% setDT(),

  data_review = review_data(adsl, adlb),

  table_1 = make_table_1(data_review),
  fig_1 = make_fig_1(adlb = data_review$adlb),

  report = rmarkdown::render(
    knitr_in("reports/slides.rmd"),
    output_file = file_out("reports/slides.html"),
    output_dir = "reports"
  )
)
