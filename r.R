# 数据输入
df = read.csv("租赁业务监控安装与并网情况表5.9.csv",encoding = 'UTF-8',stringsAsFactors = F)
df = df[1:42,1:3]
names(df)=c("dist","install","abnormal")
df$ab.rate = df$abnormal / df$install    # abnormal rate

# 合并重复区域
dist.new = unique(df$dist)
abnormal.new = sapply(dist.new,function(x) sum(df$abnormal[df$dist==x]))
install.new = sapply(dist.new,function(x) sum(df$install[df$dist==x]))
df = data.frame(dist=dist.new, abnormal=abnormal.new, install=install.new)
df$dist=as.character(df$dist)

# 去掉安徽
df=df[df$dist!="安徽",]

# 找经纬度
library(devtools)
#install_github("lijian13/RbaiduLBS") #An interface to Baidu LBS API - Jian Li
library(RbaiduLBS)
baidu.ak = 'xLPyzk9xAIvPvvetSpV7BIvZoohSZIsB' # 申请的百度地图access key
df$longitude = NA
df$latitude = NA
for(i in 1:nrow(df)){
  geo.info = getGeocoding(address = df$dist[i], ak = baidu.ak)
  df$longitude[i] = geo.info$lng
  df$latitude[i] = geo.info$lat
}
#检查下位置对不对
plot(df$longitude,df$latitude)
text(df$longitude,df$latitude,labels = df$dist)

# 距离矩阵
distance = matrix(nrow=35,ncol=35)
for(i in 1:35){
  for(j in 1:35){
    distance[i,j] = sqrt((df$longitude[i]-df$longitude[j])^2 + (df$latitude[i]-df$latitude[j])^2)
  }
}

# 成本矩阵
# cost[i,j]: 假设运维人员在i，去j维修所有异常电站需要的总路程 (=距离*异常电站数)
cost = matrix(nrow=35,ncol=35)
for(i in 1:35){
  for(j in 1:35){
    cost[i,j] = distance[i,j] * (df$abnormal[j]+0.1) #+0.1以避免0产生
  }
}
rownames(cost) = 1:35

# 目标：使总成本最小
# 总成本：假设驻扎点是i_1,...,i_N, 对任意j，选cost[i_1,j],...,cost[i_N,j]中最小的，再求和
cost.compute = function(ivec){  # ivec: vector of i_1,i_2,...,i_N
  if(length(ivec)==1){return(sum(cost[ivec,]))} # avoid error
  mincost = apply(cost[ivec,],2,min)
  return(sum(mincost))
}

# 贪心算法：每次都选择所剩区域中成本最小的点来作为新加入的驻扎点，但已加入的驻扎点不变
# 优点：快速
# 缺点：可能不是全局最优解。例如N=1时可能应该选择的是比较中央的驻扎点，但是N=2时显然就不应该选择中央的点，而是分成两边各选一个点来就近负责各自的大区域
greedy = function(N){
  selection = c()
  for(i in 1:N){
    if(length(selection)==0){candidate = (1:35)}else{
      candidate = (1:35)[-selection] #候选点
    }
    cost.all = sapply(candidate, function(x) cost.compute(c(selection,x))) #计算加入这个点以后cost的大小
    selection = c(selection, candidate[which.min(cost.all)]) #选使cost最小的点，加入selection
  }
  selection = sort(selection)
  return(list(selection=selection, cities=df$dist[selection],
              cost=cost.compute(selection)))
}
greedy(5)
## $selection
## [1] 4  8 16 23 28
## $cities
## [1] "宁海" "德清" "诸暨" "桐庐" "龙游"
## $cost
## [1] 280.3128
# （与下面的全局最优解比较来看，差别不是很大）

# 枚举法
# 优点：一定是最优解；缺点：很慢，需要N较小才能计算
enumerate = function(N){
  subset = t(combn(1:35,N))
  cost.subset = apply(subset,1,cost.compute)
  mincost = which.min(cost.subset)
  selection = subset[mincost,]
  return(list(selection=selection, cities=df$dist[selection],
              cost=cost.subset[mincost]))
}
enumerate(5)
## $selection
## [1]  4  8 12 23 28
## $cities
## [1] "宁海" "德清" "嵊州" "桐庐" "龙游"
## $cost
## [1] 257.1481
# （全局最优解）


# 随机终点法：随机选N个中心，给他们分配终点，再在终点组成的区中选新的最优中心，然后为这些中心重新分配终点 - 反复迭代，直到达到稳定
# 比枚举法快，近似最优解
random = function(N, maxiter=99999){
  selection = sample(1:35,N)
  subcost = cost[selection, ]
  classn = apply(subcost,2,which.min)
  selection0 = selection*0 #初始化，用来存旧结果
  classn0 = classn*0 #初始化，用来存旧结果
  iter = 1 #循环次数
  while(sum(selection!=selection0)>0 | sum(classn!=classn0)>0){ #只要没有达到稳态就循环
    selection0 = selection
    classn0 = classn
    for(i in 1:N){ #选新的最优中心
      if(sum(classn0==i)==0){selection = sample(1:35,N);break} 
      if(sum(classn0==i)==1){selection[i]=which(classn0==i);next} # 小区域只有一个点时直接是这个点
      subcost2 = cost[classn0==i,classn0==i] #第i个小区域的cost matrix
      selection[i] = as.numeric(rownames(subcost2)[which.min(rowSums(subcost2))])
    }
    subcost = cost[selection, ]
    classn = apply(subcost,2,which.min) #重新分类
    iter = iter+1
    if(iter>maxiter){print("超出最大次数！");break}
  }
  selection=sort(selection)
  return(list(iteration=iter, selection=selection, cities=df$dist[selection],
              cost=cost.compute(selection)))
}
random(5)
# 迭代次数少，速度快，但是结果时好时坏，有时甚至比greedy差
# 因此，进一步考虑：通过大量反复进行“随机终点法”结果选最优，以更大概率逼近全局最优
repeatrandom = function(N, times=500){
  result = list()
  cost.all = double(times)
  for(i in 1:times){
    result[[i]] = random(N)
    cost.all[i] = result[[i]]$cost
  }
  return(result[[which.min(cost.all)]])
}
repeatrandom(5)
# N=5时，几乎每次都能得到和枚举法一样的全局最优解，但速度比枚举法快了很多，可以用来计算N较大的情况


# 贪心算法 和 随机终点法 的对比 (N=2,3,4,...,25)
set.seed(0)
g.result = rep(NA,25)
rr.result = rep(NA,25)
for(i in 2:25){
  g.result[i] = greedy(i)$cost
  rr.result[i] = repeatrandom(i)$cost
}
plot(g.result,type="l")
lines(rr.result,col=2)
# 当N<=10，随机终点法 更接近最优解