### practice_1_answer
### BMI Caculator

# Craete variable called my.height.cm with your actual height in cm 
my.height.cm <- 170

# Craete variable called my.weight.cm with your actual weight in kg
my.weight.kg <- 65

# Create my.height.m transfered by my.height.cm  
my.height.m <- 1.70

# Create my.bmi with BMI(Body Mass Index) formula
my.bmi <- my.weight.kg/(my.height.m)^2

# Use if-else to print matched information
# Reference: http://www.tpech.gov.taipei/ct.asp?xItem=1794336&CtNode=30678&mp=109171
if (my.bmi >= 35) {
  print(paste("Your bmi: ", my.bmi))
  print("重度肥胖!")
} else if (my.bmi >= 30) {
  print(paste("Your bmi: ", my.bmi))
  print("中度肥胖!")
} else if (my.bmi >= 27) {
  print(paste("Your bmi: ", my.bmi))
  print("輕度肥胖!")
} else if (my.bmi >= 24) {
  print(paste("Your bmi: ", my.bmi))
  print("過重")
} else if (my.bmi >= 18.5) {
  print(paste("Your bmi: ", my.bmi))
  print("正常")
  
} else{
  print(paste("Your bmi: ", my.bmi))
  print("過輕")}
  
