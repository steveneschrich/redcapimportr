

split_list_r <- list(preamble="PartnershipTrackingP",
                     type = "R",
                   date = "2021-11-08",
                   time = "1628",
                   ext="r"
)
split_list_data <-list(preamble="PartnershipTrackingP",
                       type="DATA",
                       date="2021-11-08",
                       time = "1628",
                       ext="csv"
                       )

test_that("split_redcap_filename on malformed string.", {
          expect_equal(split_redcap_filename("PartnershipTrackingP_DATA_2021-11-08_1628.csv"),
                       split_list_data)
          expect_equal(split_redcap_filename("PartnershipTrackingP_R_2021-11-08_1628.r"),
                       split_list_r)
          expect_error(out<-split_redcap_filename("test"))
})
