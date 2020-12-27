---
title: "test out bin packing"
author: "Lynette Lau"
output: html_notebook
---
#Uses gbp package.generate photos for pitching, show packing analysis that tries to maximise volume with price.

library(gbp)

#**R-level class, solver and viewer**
#list of items

it <- data.table::data.table(
  oid = c(1249, 1249, 1249, 1249, 1249, 1250, 1250, 1250, 1250, 1251),
  sku = c("SC-460554", "SC-385177", "SC-347335", "SC-645285", "SC-108384", "SC-850619", "SC-688675", "SC-232507", "SC-489973", "SC-954684"),
  l   = c(2.14, 7.24, 7.24, 2.14, 7.24, 7.24, 6, 4, 9.8, 9.9),
  d   = c(3.58, 7.24, 7.24, 3.58, 7.24, 7.24, 6, 4, 6.6, 7.8),
  h   = c(4.76, 2.58, 2.58, 4.76, 2.58, 2.58, 6, 4, 12, 17.5),
  w   = c(3.00, 10.5, 2.4, 243, 13.13, 24.2, 2.3, 3.8, 14.38, 8.2)
)

knitr::kable(it)

#bins - boxes available for use.

bn <- data.table::data.table(
  id = c("B1","B2","B3"),
  l  = c(42,42,42),
  d  = c(29,29,29),
  h  = c(25.5,25.5,25.5),
  w  = c(300,300,300)
)


knitr::kable(bn)

#solving the problem: The function gbp::bpp_solver(it, bn) will undergo 2 steps: (1) aim to pack each order into the smallest number of bins; (2) find the smallest bins to achieve highest utlization rate, subject to the three dimensional none overlap contraints and weight limit constraint.

packsolution <- gbp::bpp_solver(it = it, bn = bn)
packsolution$it
packsolution$bn
gbp::bpp_viewer(packsolution)


#single box solution

ldhw <- t(as.matrix(it[oid == 1249, .(l, d, h, w)])) #items 4 to 8
ldhw
m <- t(as.matrix(bn[ , .(l, d, h, w)])) # multple bin
m
p <- gbp4d_solver_dpp_prep_create_p(ldhw, m[, 2L]) # single bin
p

sn4d <- gbp4d_solver_dpp(p, ldhw, m[, 2L])
sn4d$it
sn4d$o
sn4d$ok
gbp4d_checkr(sn4d) #- check: no overlap in 3d volume and no over weight limit

gbp4d_viewer(sn4d) #viewer
