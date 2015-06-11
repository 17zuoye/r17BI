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

test1 = track_relative_time(sdata, adata_1, track_length)
expect_that(test1[6:16], equals(rep(1,11)) )

test2 = track_relative_time(sdata, adata_2, track_length)
expect_that(test2[6:16], equals(c(rep(1,3),rep(4,8)) ) )

# test for multiple users
sdata_1 = data.frame(id=c(1,2), time = rep(as.POSIXct(strptime('2015-06-01','%F')),2))

adata = data.frame(id=c(1,2),
                     time = c(as.POSIXct(strptime('2015-06-06','%F')),
                              as.POSIXct(strptime('2015-06-09','%F'))),
                     num=c(1,3))

test1 = track_relative_time(sdata_1, adata, track_length)
expect_that(test1[1,6:16], equals(rep(1,11)) )
expect_that(test1[2,9:16], equals(rep(3,8)) )


sdata_2 = data.frame(id=c(1,3), time = rep(as.POSIXct(strptime('2015-06-01','%F')),2))

test2 = track_relative_time(sdata_2, adata, track_length)
expect_that(test2[1,6:16], equals(rep(1,11)) )
expect_that(sum(is.na(test2[2,])), equals(16) )
