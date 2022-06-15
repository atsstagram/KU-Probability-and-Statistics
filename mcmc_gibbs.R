###MCMC法 ###Gibbs法
###指数分布
mu = c(1, 0.5) #平均
sigma = c(1, 0.5) #標準偏差
rho = 0.8
Sigma = matrix(c(sigma[1] ^ 2, sigma[1] * sigma[2] * rho, sigma[1] * sigma[2] * rho, sigma[2] ^ 2),
               2,
               2)
Sigma
Sinv = solve(Sigma)
myf = function(x)
  exp(-0.5 * (x - mu) %*% Sinv %*% (x - mu))

h1 = function(x2)
  rexp(1,
       (mu[1] + (sigma[1] + sigma[2]) * rho * (x2 - mu[2])))
h2 = function(x1)
  rexp(1,
       (mu[2] + (sigma[2] + sigma[1]) * rho * (x1 - mu[1])))

xx = matrix(0, 2, N)
ff = rep(0, N)
#cnt = 0
x = c(0, 0)
for (t in 1:N) {
  i = t %% 2 + 1
  if (i == 1) {
    x[1] = h1(x[-1])
  } else if (i == 2) {
    x[2] = h2(x[-2])
  }
  xx[, t] = x
  ff[t] = myf(x)
}
apply(xx, 1, mean)
var(t(xx))
hist(xx[1, ], breaks = 100, freq = FALSE) #縦軸を確率に
curve(exp(-x), add = TRUE, col = "red") #確率密度関数の理論曲線
plot(t(xx), pch = ".", col = rgb(0, 0, 0, alpha = 0.1))