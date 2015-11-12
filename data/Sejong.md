Population and Households in Chosun Dynasty
========================================================
author: Kee-Won Lee
date: 10/03/15

Data
========================================================

- 조선시대 호수와 인구의 변화 


```
   year.chosun households population
1         1404     153403     322746
2         1406     180246     370365
3         1432     201853     692475
4         1648     441321    1531365
5         1657     658771    2290083
6         1669    1313453    5018644
7         1672    1178144    4701359
8         1678    1342428    5246972
9         1717    1560561    6846568
10        1724    1572086    6865286
11        1726    1576598    7032425
12        1777    1715371    7238546
```

Plot
=========================================================

![plot of chunk unnamed-chunk-2](Sejong-figure/unnamed-chunk-2-1.png) 

Code
========================================================

```
plot(population/10000~year.chosun,data=chosun.pop.data, type="b",pch=17, col="red",ylim=c(0,800), xlab="연도", ylab="호수와 인구수(단위 만)")
lines(households/10000~year.chosun, data=chosun.pop.data, type="b", pch=17)
legend("topleft", legend=c("인구","호수"), lty=1, col=c("red","black"))
text(x= 1700,y=500, labels="인구", col="red")
text(x= 1700,y=100, labels="호수")
title(main="조선시대 호수와 인구수의 변화")
```

