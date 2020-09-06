context("vi plot")
set.seed(10)
n = 100
p = 10
k = 1:10
beta = c(1, -1, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)
y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
data = data.frame(y, x)
model = glm(y ~ ., data = data, family = "binomial")

list_result = apes(model = model, n_boot = 20)

vi_tile_result = plot_vi_boot_apes(list_result = list_result)
vdiffr::expect_doppelganger("Variable inclusion plot", vi_tile_result$vi_plot)

###############
# imgurl <- "~/Desktop/monkey.jpg"
# hexSticker::sticker(imgurl, package = "APES",
#         s_x=1, s_y=.8,
#         s_width = 0.4,
#         p_size=8, p_color = "#3E3E3B",
#         h_fill="white", h_color = "#2A93D5",
#         filename="inst/APES_logo.png")
