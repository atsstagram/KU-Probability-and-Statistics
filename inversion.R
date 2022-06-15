###逆関数法
###指数分布
set.seed(11) #生成する乱数を固定
N = 1000000 #生成する乱数の数
u = runif(N) #一様分布から乱数を生成
mu = 1.0 #平均を1とした
x = -mu * log(1 - u) #逆関数
rbind(u, x)[, 1:5] #最初の5個
#hist(x)
hist(x, breaks = 100, freq = FALSE) #縦軸を確率に
curve(dexp(x, 1 / mu), add = TRUE, col = "red") #確率密度関数の理論曲線
plot(x, pch = ".", col = rgb(0, 0, 0, alpha = 0.1))