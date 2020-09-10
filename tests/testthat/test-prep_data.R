context("Prepare Data")

### Support -----

test_that("`prep = 'support'` works", {
  expect_equivalent(prep_data(Beijing_cleaned, "support"),
                    sort(unique(Beijing_cleaned$MSRP)))
  expect_equivalent(prep_data(Tianjin_cleaned, "support",
                              upperdate = "2011-01-01"),
                    sort(unique(Tianjin_cleaned$MSRP[Tianjin_cleaned$year < 2011])))
})


test_that("unnecessary arguments are ignored", {
  expect_message(prep_data(Beijing_cleaned, "support"),
                 "`count` will be ignored.")
  expect_message(prep_data(Beijing_cleaned, "support", count = NA),
                 "`count` will be ignored.")
})



