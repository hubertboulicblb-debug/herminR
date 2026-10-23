# Pastry-dough experiment (Gilmour & Trinca 2000) as distributed in daewr::pastry
# (Lawson 2015). Only the single published response is available.
load("daewr/data/pastry.rda")  # from https://github.com/cran/daewr
pastrydough <- data.frame(block = pastry$Block, x1 = pastry$x1, x2 = pastry$x2,
                          x3 = pastry$x3, y = pastry$y)
save(pastrydough, file = "data/pastrydough.rda", compress = "xz")
