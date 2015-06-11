# setup
# benchmark time are all 2015-06-01
library(testthat)
library(dplyr)
library(r17BI)
# test for single user
sdata = data.frame(id=1, time = as.POSIXct(strptime('2015-06-01','%F')))
adata_1 = data.frame(id=1, time = as.POSIXct(strptime('2015-06-06','%F')), num=1)
adata_2 = data.frame(id=rep(1,2),
                     time = c(as.POSIXct(strptime('2015-06-06','%F')),
                              as.POSIXct(strptime('2015-06-09','%F'))),
                     num=c(1,3))

track_length = 15
end_time = as.POSIXct(strptime('2015-6-18','%F'))

test1 = track_relative_time(sdata, adata_1, track_length, end_time)
expect_that(test1$track[6:15], equals(rep(1,10)) )

test2 = track_relative_time(sdata, adata_2, track_length, end_time)
expect_that(test2$track[6:15], equals(c(rep(1,3),rep(4,7)) ) )

# test for multiple users
sdata_1 = data.frame(id=c(1,2), time = rep(as.POSIXct(strptime('2015-06-01','%F')),2))
sdata_2 = data.frame(id=c(1,3), time = rep(as.POSIXct(strptime('2015-06-01','%F')),2))
sdata_3 = data.frame(id=c(1,2), time = c(as.POSIXct(strptime('2015-06-01','%F')),
                                         as.POSIXct(strptime('2015-06-5','%F')))
                     )

adata = data.frame(id=c(1,2),
                     time = c(as.POSIXct(strptime('2015-06-06','%F')),
                              as.POSIXct(strptime('2015-06-09','%F'))),
                     num=c(1,3))



test1 = track_relative_time(sdata_1, adata, track_length, end_time)
expect_that(test1$track[1,6:15], equals(rep(1,10)) )
expect_that(test1$track[2,9:15], equals(rep(3,7)) )

test2 = track_relative_time(sdata_2, adata, track_length, end_time)
expect_that(test2$track[1,6:15], equals(rep(1,10)) )
expect_that(test2$track[2,], equals(rep(0,15)) )

test3 = track_relative_time(sdata_3, adata, track_length, end_time)
expect_that(test3$track[1,6:15], equals(rep(1,10)) )
expect_that(test3$track[2,5:14], equals(rep(3,10)) )
expect_that(is.na(test3$track[2,15]), equals(T))
