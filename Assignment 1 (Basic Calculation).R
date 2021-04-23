#Factorial of 10
faktorial = function(n) {
  if(n <= 1) {
    return(1)
  } else {
    return(n * faktorial(n-1))
  }
}
faktorial(10)

#Grade of student
grade = function(x)
  if (x > 80) {
    print('A')
  } else if (x > 60 & x <= 80) {
    print('B')
  } else if (x > 40 & x <= 60) {
    print ('C')
  } else {
    print('D')
  } 
grade(75)

#List of numbers multiple by 7 with max 100
i = 0
while (i <= 100) {
  print(i)
  i = i + 7
}