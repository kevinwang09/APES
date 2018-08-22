#
# data("diabetes", package = "lars")
# x = diabetes$x
# x2 = diabetes$x2
# y = ifelse(diabetes$y > median(diabetes$y), 1L, 0L)
# dim(x)
# length(y)
# 
# fullModel = glm(y ~ x, family = "binomial")
# fullModel2 = glm(y ~ x2, family = "binomial")
# 
# apes_leaps_logit(x = x, 
#                  y = y, 
#                  Pi = fullModel$fitted.values,
#                  maxK = ncol(x))
# 
# 
# apes_mio_logit(x = x, 
#                y = y, 
#                Pi = fullModel$fitted.values,
#                krange = 1:ncol(x))
# 
# 
# # apes_leaps_logit(x = x2, 
# #                  y = y, 
# #                  Pi = fullModel2$fitted.values,
# #                  maxK = 5)