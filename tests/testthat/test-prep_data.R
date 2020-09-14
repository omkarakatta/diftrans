context("Prepare Data")

### Support -----

test_that("`prep = 'support'` works", {
  expect_equivalent(prep_data(Beijing_sample, "support"),
                    sort(unique(Beijing_sample$MSRP)))
  expect_equivalent(prep_data(Tianjin_sample, "support",
                              upperdate = "2011-01-01"),
                    sort(unique(Tianjin_sample$MSRP[Tianjin_sample$year < 2011])))
})


test_that("unnecessary arguments are ignored", {
  expect_message(prep_data(Beijing_sample, "support"),
                 "`count` will be ignored.")
  expect_message(prep_data(Beijing_sample, "support", count = NA),
                 "`count` will be ignored.")
})



