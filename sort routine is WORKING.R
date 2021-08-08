temp <- 0
for (i in 1:100){
  # test sort routine
  newtemp <- runif(1,1,100)
  if(newtemp>temp){
    temp = newtemp
  }
  print(newtemp)  
}

print(temp)