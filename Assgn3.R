checkprime<-function(num)
{  
   #num = as.integer(readline(prompt="Enter a number: "))
  
   flag = 0
  # prime numbers are greater than 1
  if(num > 1) {
    # check for factors
    flag = 1
    for(i in 2:(num-1)) {
      if ((num %% i) == 0) {
        flag = 0
        break
      }
    }
  }
   if(num == 2)    flag = 1
   if(flag == 1) {
     print(paste(num,"is a prime number"))
   } else {
     print(paste(num,"is not a prime number"))
   }
   
   return(flag)
}
checkprime(103)
countprime<-function(x)
{
  primecount<-0
  for(i in 1:length(x))
  {
    flag<-checkprime(x[i])
    cat(flag)
    if(flag == 1) {
      print(paste(num,"is a prime number"))
      primecount<-primecount+1
    }
    
  }
  cat(primecount)
  return(primecount)
}

cat(primecount)

myFirstFun<-function(x)
{
  y<-c()
  for(i in x)
  {
      cat(i)
      h<-grepl("a",i)&&grepl("u",i)
      y<-c(y,h)
      
      
  }
  return(y)
}


cat("Enter the weight in kg")
cat("Enter the height in meters")
bmiinfo<-function(weight,height)
{
  category<-""
  bmi=weight/height^2
  cat(bmi)
 

}




k<-c("above","aunder","Unit")
# call the function with that value
m<-myFirstFun(k)
cat(m)


sum<-function(n)
{
  y=(n*(n+1)/2)^2
  return(y)
  
}
sum(10)  
cat(sum)
   
mode1<-function(d)
{
  return(mode(d))
}

cat(mode1)



s<-c(2,3,3,4,4,5,6,7,9,10)
v<-sort(s)
uniq<-unique(v)
i<-1
numnow<-s[i]
s[i+1]
numnow
  
store<-vector(length=10,mode="numeric")
k<-1
curcount<-1
repeat{
  if(numnow==s[i+1])
   { curcount<-curcount+1
  }
  else
  {
    store[k]<-curcount
    k<-k+1
    curcount<-1
    numnow<-s[i+1]
    
  }
  if(i==length(store)-1)
  {
    store[length(store)]=1
    break
  }
    
  i<-i+1

  
    
}
cat("the mode is")
store



#Q8
  #a)LOAD TWO FILES AND DISPLAY
  library(readr)
  File1 <- read_csv("C:/Users/Acer/Desktop/Acadgild/ReadData/File1.csv")

  library(readr)
  File2 <- read_csv("C:/Users/Acer/Desktop/Acadgild/ReadData/File2.csv")
 
  f1<-data.frame(File1)
  f2<-data.frame(File2)

  #b)EQUI JOIN
   merge(f1,f2,by="empid")
   
  #c) LEFT OUTER JOIN
   merge(f1,f2,by="empid",all.x = TRUE)
   
  #d)RIGHT OUTER JOIN
   merge(f1,f2,by="empid",all.y = TRUE)
   
   #e)FULL OUTER JOIN
   merge(f1,f2,by="empid", all=TRUE) 
   
   
#f)
   f2 %>% filter(sal == 0)
   
   f1%>%group_by(deptid,mgrid)
   
#g)
   f2[,1:2]<-sapply(f2[,1:2],as.numeric) 
   f2$empid-f2$sal
   
   sqldf("select avg(sal) as average from f2")
   sqldf("select sum(sal) as count from f2")
   
#h)
   f2$empname<-paste(f2$firstname,"",f2$lastname)

   grouprec<-function(x,y)
   {
     q<-paste("select count(distinct ",x," ) from prod group by ",y)
     q
     
     return(sqldf(q))
   }
#Q9
   uniqrecord <- function(){
     library(dplyr)
     color <- infert
     color
     distinct(color,age)
   }
   uniqrecord()   
#Q10   
   
 #find null values in dataset  
   #sum(is.na(student$aggregate)) 
   
   checkNull<-function(d)
   {
     data1<-d
     data1
     
     if(is.null(data1)||complete.cases(data1))
     {
       print(TRUE)
     }
     else
     {
       print(FALSE)
     }
     
    
        
       
     
  
   }
   
   
   checkNull(df2)   
#Q 11
   
#a)   
   
removedup<-function(x)
{
  return(unique(x))
}
a<-c(1,1,1,1,2,2,3)
b<-c("car","bus","car","top","bus","top","car")
removedup(a)
removedup(b)

#b)
poise <- function(x){
  viz <- x[duplicated(x)]
  print(viz)
  print("count of distinct elements is")
  return(length(viz))
}
x <- c(8,9,9,7,5,4,4,3,2,6,6,2,1)
poise(x)




  
uniqcount<-function(x)
{
  n<-length(unique(x))
  return(n)
}
uniqcount(a)
  
#c)

concat<-function(x,y)
{
  result<-paste(x,y)
  return(result)
}
  
concat("cat","bat") 

#d)

sumcol<-function(data)
{
  columnsum<-apply(mtcars,2,sum)
  return(columnsum)
}
#e)
  
filelist<-function(path1)
{
  
  list.files(path=path1,all.files = TRUE)
}
path<-"C:/Users/Acer/Desktop/MachLearning_Pranav/Machine Learning Company"
filelist(path)

#f)
filedel<-function()
{
  files.to.delete<-dir("C:/Users/Acer/Desktop/MachLearning_Pranav/Machine Learning Company",pattern = "Test.docx",full.names = TRUE)
  if(file.exists(files.to.delete))
  {
    print("file exists")
    file.remove(files.to.delete)
  }
  else
  {
    print("Sorry file you want to delete does not exist")
  }
  
  }
    
filedel()









data1<-mtcars

#Q12
#a)
loadfile<-function(path)
{
  fstore<-read.csv(path)
  attach(fstore)
  print(fstore)
}
loadfile("C:/Users/Acer/Desktop/Acadgild/book7.csv")


#b)
renamcol<-function(data1,oldname,newname)
{
  rename(data1, c(data1$oldname=data1$newname))
  return(data1)  
}
renamcol(mtcars,"mpg","mms")

#c
dropcol<-function(data1,colname)
{
  data1$colname<-NULL
  return(data1)
}

dropcol()


#e)

Missdiff<-function()
{
  x<-c(88,NA,79,NA,56)
  print(mean(x))
  sqrt(-50) #result produces a NaN ,which means "Not a Number"
  x<-c(88,NULL,79,NULL,56)
  print(mean(x))
  
}
Missdiff()




