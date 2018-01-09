x <- c(2,4,6,8)
y <- c(1,3,5,7)

x[-1]
y[y>2]
which(y>2)
(1:length(y))[y>2]

x[y>2]
sum(y>5)
sum(y > max(y)-2)


y[c(T,F,F)]
which(x==4)

z <- c(4,1,6,7,2,9,3)
sort(z)
order(z)

x1 <- c(-3, 3, 1, -1)
order(x1)


mean(1:10)


outer(x,y,"+")
-1%%9

-11%%2
11%%2


k = 1 + 2i 

print (seq (12,30))

alist <- list ("Red", "Blue", c(42,36,01), FALSE, 73.91, 128.6)
print (alist)

ls <- list(
  
  first = 2,
  
  second = 4,
  
  third = list(
    
    fourth = 3.2,
    
    fifth = 6.4
  )
)
ls [1:2]
ls[-3]
ls [c ("first", "second")]


names (alist) <- c ("Ray", "Karl", "Steve")

# Now you will add element at the end of the list

alist[4] <- "Mark"
print (alist[4])
alist[4] <- NULL



# creating 2 vectors of dissimilar lengths
vec1 <- c (3, 4, 2)
vec2 <- c (11, 12, 13, 14, 15, 16)
# taking these vectors as input for this array
res1 <- array (c (vec1, vec2), dim=c (3,3,2))
print (res1)



# Creating 2 vectors having different lengths.
vec1 <- c (2, 4, 6)
vec2 <- c (11, 12, 13, 14, 15, 16)
column.names <- c ("COLA","COLB","COLC")
row.names <- c ("ROWA","ROWB","ROWC")
matrix.names <- c ("MatA", "MatB")

res1 <- array (c (vec1,vec2), dim=c (3,3,2), dimnames=list (column.names, row.names, matrix.names))
print(res1)



f <-function(){
  cat("Hello, World!\n")
      }
f()

f<-function(num){ 
  for(i in seq_len(num)) 
  #cat("Hello, world!\n")
  cat("Hello, world!","")
} 

f(3)

f<-function(num){ 
  hello <-"Hello, world!\n" 
  for(i in seq_len(num)) { cat(hello) } 
  chars <- nchar(hello) * num + chars 
}
meaningoflife <- f(3) # nothing in this
print(meaningoflife)


x <- 1:4
x >2
0 %in% x

x <-matrix(1:4, 2, 2,byrow = TRUE) 
y <-matrix(rep(10, 4), 2, 2) 
x * y

vec <- c(1,"hello",TRUE)

x <-0:6 
as.logical(x)
# as.logical will print TRUE for 1 and FALSE for 0 since x is a vector of numbers from 0 to 6. 
# Hence, the output shown for option a will be displayed.


v <-2*x + y +1


s <- c(5,1,2)
sort(s)
order(s)
sort.list(s)


s1 <- c(3,1,11)
order(s1)

x <-list(foo =1:4, bar =0.6)
print(x[[1]] )

x <-list(foo =1:4, bar =0.6, baz ="hello") 
name <-"foo" 
x$name

pts <- list(x = cars[,1], y = cars[,2])
plot(pts)

x <-list(foo =1:4, bar =0.6, baz ="hello") 
name <-"foo" 
x[[name]]
# One thing that differentiates the [[ operator from the $ is that 
# the [[ operator can be used with computed indices. 
# Here, x[[name]]" is the computed index."

x <-list(a =list(10, 12, 14), b =c(3.14, 2.81)) 
x[[c(1, 3)]]

# [[]]与$类似，都是list选取单个元素
# 唯一的区别是[[通过参数“exact”激活模糊匹配
x <-list(aardvark =1:5) 
x[["a", exact = FALSE]] # 模糊匹配

#   定义list对象
li <- list(name='yiifaa', age=35)
#   从list提取元素一定要用[[]]
#   模糊匹配到name
li[['nam', exact=FALSE]]
li[['a', exact=FALSE]]

#   如果R语言支持对象比较的话，那么li[1] == li
li[2][[1]] == li[[2]]
#   继续玩转它们
li[[1]][1] == li[[1]]


# 标准分布
x <- seq(-10, 10, by = .1)
# Choose the mean as 2.5 and standard deviation as 0.5.
y <- dnorm(x, mean = 2.5, sd = 0.5) # dnorm() 的返回值是正态分布概率密度函数
y <- pnorm(x, mean = 2.5, sd = 2) # pnorm()返回值是正态分布的分布函数
# Give the chart file a name.
png(file = "dnorm.png")
plot(x,y)
# Save the file.
dev.off()



flag <- 3<2
month <- 3-2 + 12 * flag
year <- 1995 - flag



xVec <- rep(NA, 10)
xVec[1] <- 1
xVec[2] <- 2
for( j in 3:(10-1) )
  xVec[j] <- xVec[j-1] + 2/xVec[j-1]


























