context("Test logit and expit")

x = seq(from = 0.001, to = 0.999, length.out = 100)

expect_equal(x, expit(logit(x)))

expect_equal(x, logit(expit(x)))
