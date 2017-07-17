result = enumerate(5)

# 每个数目除以1.4
df[,c(2,3)]=df[,c(2,3)]/1.4

result
selection = result$selection #驻扎点
classn = apply(cost[selection,],2,which.min) #每个城市分类

plot(df$longitude,df$latitude,col=classn+1, xlab="经度",ylab="纬度")
text(df$longitude,df$latitude,labels = df$dist,col=classn+1)
points(df$longitude[selection],df$latitude[selection],pch=19,col=1)

class.selection = selection[classn]

#计算每个驻扎点需要监控的电站数目
N.monitor = rep(0,5)
for(i in 1:5){
  N.monitor[i] = round(sum(df$install[classn==i]))
}
text(df$longitude[selection]+0.2,df$latitude[selection]+0.1,labels = N.monitor,col=1)

#计算每个驻扎点需要修理的电站数目
N.repair = rep(0,5)
for(i in 1:5){
  N.repair[i] = round(sum(df$abnormal[classn==i]))
}
text(df$longitude[selection]+0.2,df$latitude[selection]-0.1,labels = N.repair,col=1)

#人手数目
# 每人每天看4个电站，每年工作250天，每年所有电站看1次
compare = function(x,y){mat = cbind(x,y);apply(mat,1,max)} #用来找到两个vector中较大的
N.people = round(N.monitor/4/250+0.5)
names(N.people) = result$cities
N.people
text(df$longitude[selection]-0.1,df$latitude[selection],labels = N.people,col=1)

# fanwei
for(i in 1:5){
  print(paste(df$dist[classn==i][1:sum(classn==i)],collapse = "，"))
}


# 每人每天看4个，每3天修1个电站，每年工作250天，每年所有电站看2次
# N.people2 = compare(round(N.monitor*2/4/250+0.5),round(N.repair*2*3/250+0.5))
# names(N.people2) = result$cities
# N.people2

