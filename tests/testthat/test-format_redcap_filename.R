split_list_r <- list(preamble="PartnershipTracking",
                     type = "R",
                     date = "2021-11-08",
                     time = "1628",
                     ext="r"
)
split_list_data <-list(preamble="PartnershipTracking",
                       type="DATA",
                       date="2021-11-08",
                       time = "1628",
                       ext="csv"
)

test_that("format_redcap_filename works", {
  expect_equal(format_redcap_filename(split_list_r), "PartnershipTrackingP_R_2021-11-08_1628.r")
  expect_equal(format_redcap_filename(split_list_data), "PartnershipTrackingP_DATA_2021-11-08_1628.csv")
})
