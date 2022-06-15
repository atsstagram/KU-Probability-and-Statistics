###棄却法
###ガンマ分布（k=1,θ=1.0） -> 指数分布:exp(-x)
###2^(-x)でカバー
set.seed(21) #生成する乱数を固定
N = 1000000 #生成する乱数の数
u =  runif(N) #一様分布から乱数を生成
set.seed(22) #生成する乱数を別に固定
v = -log(runif(N)) / log(2) #2^(-x)の逆関数
y = (2 / exp(1)) ^ v #閾値の計算
bool = u <= y #条件を満たすかどうか
rbind(u, v, y, bool)[, 1:5] #最初の5個
x = v[bool] #条件を満たすvだけ取り出す
length(x) / N * 100 #使われた乱数の割合
#hist(x)
hist(x, breaks = 100, freq = FALSE) #縦軸を確率に
curve(exp(-x), add = TRUE, col = "red") #確率密度関数の理論曲線
plot(x, pch = ".", col = rgb(0, 0, 0, alpha = 0.1))