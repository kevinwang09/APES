context("vi plot")
set.seed(10)
n = 100
p = 10
k = 1:10
beta = c(1, -1, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)
y = rpois(n = n, lambda = exp(x %*% beta))
mu = glm.fit(x = x, y = y, family = poisson(link = "log"))$fitted.values

listResult = boot_apes_poisson(x = x, y = y, mu = mu, k = k, estimator = "leaps", nBoot = 20)
viPlotResult = plot_vi(listResult)
vdiffr::expect_doppelganger("Variable inclusion plot", viPlotResult$viPlot)

###############
# imgurl <- "~/Desktop/monkey.jpg"
# hexSticker::sticker(imgurl, package = "APES",
#         s_x=1, s_y=.8,
#         s_width = 0.4,
#         p_size=8, p_color = "#3E3E3B",
#         h_fill="white", h_color = "#2A93D5",
#         filename="inst/APES_logo.png")
