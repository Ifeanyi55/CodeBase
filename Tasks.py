nums = [8,2,1,1,3]
dums = [2,3,4,9,7]
dums.
nums

count_nums = nums.count(8)
count_nums
range(len(nums))

sum(i < j for i in nums for j in nums )


def Freq_counter():
  count = 0
  output = []
  for i in nums:
    for j in nums:
      if j < i:
        count = count+1
        
  return output.append(count)

Freq_counter()

dums.count(dums) > 6
nums[0] < nums[1]

################# Customer wealth #################
accounts = [[1,2,3],[3,2,1]]
accounts[0]
len(accounts)
range(len(accounts))

def maximumWealth(accounts):
        
        
        for i in range(len(accounts)):
            result = sum(accounts[i])
            
        print(result)
maximumWealth(accounts=accounts)

########## Richest customer ##########
customer_accounts = [[1,5],[7,3],[3,5]]
customer_accounts[2]
range(len(customer_accounts))
len(customer_accounts)
sum(customer_accounts[])

def richest(accounts):
  highest = []
  counter = 0
  
  while counter < 3:
    result = sum(accounts[counter])
    highest = highest.append(input(result))
    counter = counter+1

    
  return max(highest)
  
richest(accounts=customer_accounts)  

## Good pairs
num = [1,2,3,1,1,3]
def goodpairs(nums):
  count = 0
  
  for i in range(len(num)):
    for j in range(len(num)):
      if num[1] == num[j] & i < j:
        count = count+1
        
  return count
  
goodpairs(nums=num)

## List comprehension for finding possible pairs
nums = [1,2,3,1,1,3]
result = [(a,b) for index, a in enumerate(nums) for b in nums[index + 1:]]

print(str(result))

nums = [2,5,1,3,4,7]
outcome = [[2,3],[5,4],[1,7]]
outcome[2][0]
result = []
output = []
n = 3
range(n)


def shuffle(nums):

    for i in range(n):
            result.append([nums[i],nums[i+n]])
            
    for j in range(len(nums)):
            output.append(result[i][j])
      
              
    return output
  
shuffle(nums=nums)

## Climbing stairs
n = 2
r = range(0,(n+1) - 1)

r

fl = [1,2,3]
fl[0:2] = 1

fl

def climb(n):

    coppy = []
    
    for i in range(0,(n+1) - 1):
            con = n - i
      
            coppy.append(con)
      
    # print(coppy)
    
    for j in range(0,n - 1):
            coppy[j] = 1
    
    
    return sum(coppy)


climb(n = n)

## Split a word into list of letters
letters = "word"
letters_list = list(letters)
letters_list
letters_list[0]

s = "anagram"
t = "nagaram"

s = list(s)

t = list(t)

b = "car"
d = "rat"

t[1] in s
len(t)

## Anagram
import numpy as np
import pandas as pd
import math as mt
from numpy import arange
from math import factorial
from pandas import DataFrame

df = DataFrame(np.shape(5),["A","B","C","D","E"],["W","X","Y","Z"])

df

def anagram(word,anagram):
  
  word = list(word)
  anagram = list(anagram)
  
  check1 = anagram in word
  check2 = np.unique(anagram) in word
  check_all = all(check1 == True)
  
  if len(anagram) == len(word):
    if check_all == True:
          print("true")
    else: to_binary = if check2 == True: 1 else: 0
         
  if sum(to_binary) == len(word):
    
      print("true")
    
  else: print("false")
  
  else: print("false")
  
  
anagram(word=b,anagram=d)

n = 5 
range(len(n))
seq = arange(1,n)
seq
seq[2]
range(5+1)
list(range(5+1))
list(range(1,5+1))
range(1,5)
range(0,4)
factorial(10)

#Remove trailing zeros in formula below by using two division marks
factorial(int(5))//factorial(int(5-1))//factorial(int(1)) 


## Pascal's triangle
def pascalsTriangle(n):
  
      store = []
  
      for j in range(1,n+1):
          for i in range(0,j):
              result = factorial(j)//factorial(j-i)//factorial(i)
              store.append(result)
      
      return store

pascalsTriangle(n = 3)

b = 15
if b < 20:
  print("Yes")
else:
  print("No")

c = [True,True,False,False,True]
if c >= 10:
  "Good"
else:
  "Bad"

if c == True:
  1
elif c == False:
  0
  

## Good string
words = ["cat","bt","hat","tree"]
char = "atach"
cs = []
words[1][1] in char
len(words)

for i in range(len(words)):
    
  tsl = list(words[i])
        
for j in range(len(tsl)-1):
  
  ts = tsl[j] in char
        
  if all(ts):
      
      
    cs.append(ts)
                
          
          
print(sum(len(cs)))
  
  
  
  
  
range(len("cat")-1)

bt = ["b","o","x"] 

bts = bt in "box"

btst = [True,True,False]

all(btst)  
  
words
wdl = list(words[0])
wdl[0] in "atach"
range(len(wdl)-1)

tol = ["bank"]
tol = list(tol[0])
tol
len(tol)

lot = "blank"
tol[3] in lot


s = "This is good"
print(all(s))

# 0 is False
# '0' is True
s = '000'
print(all(s))

s = ''
print(all(s))

p = [True,True,True]
all(p)


## Binary search
nums = [-1,0,3,5,9,12]
target = 4

nums.index(9) # Returns the index position of number in a list
target in nums

def binary_search(nums,target):
  
  check = target in nums
  
  if check == True:
    
    return nums.index(target)
    
  else: return -1
  
  
binary_search(nums=nums,target=target)

  
## Check words summation equals word
import string as st
from string import ascii_lowercase

letters = ascii_lowercase
letters[0]

letters.index(firstword[0])

firstword = "acb"
secondword = "cba"  
targetword = "cdb"  

firstword[2]  

len(firstword)-1

check1 = []
for i in range(len(firstword)):
  res1 = letters.index(firstword[i])
  check1.append(res1)
  print(check1)

check1
sum(check1)

## Joining the numbers in a list
str_check1 = [str(int) for int in check1]

str_of_check1 = "".join(str_check1)

print(str_of_check1)


def check_words(first,second,target):
  
  check1 = []
  check2 = []
  check3 = []
  
for i in range(len(first)-1):
  res1 = letters.index(first[i])
  check1.append(res1)
  
  
## Height checker
heights = [1,1,4,2,1,3]
expected = [1,1,1,2,3,4]

heights  
expected  

heights[2] == expected[2]

store1 = []
store2 = []

store = []

for i in range(len(heights)):
  
  result1 = heights[i]
  store1.append(result1)
  
for j in range(len(expected)):
    result2 = expected[j]
    store2.append(result2)
    
    
print(store1)    
    
    
store1[0] == store2[0]
range(6)

    for p in range(5):
      
        outcome = heights[p] == expected[p]
        
        store.append(outcome)
    
print(store)    
    
for k in range(len(heights)):
  for p in range(len(expected)):
    result = heights[k] == expected[p]
    store.append(result)
    
print(store)
    
    
b = "balloon"    
b.count("o")    

text = "loonbalxballpoon"    
text.count(text[1])    


## Shuffle indices
s = "codeleet"
indices = [4,5,6,7,0,2,1,3] 
s.index("d")

p = "aiohn"
pind = [3,1,4,2,0]

t = "aaiougrt"
tind = [4,0,2,6,7,3,1,5]


def shuffle_string(string,indices):
  
      take = []
        
      string = list(string)
      
      sdf = DataFrame(string,indices)   
      
      for i in range(len(string)):
        result = sdf[0][i]
        take.append(result)
        
      return " ".join(take)

shuffle_string(string=s,indices=indices)

  
dic = {"s":list("codeleet"),"indices":[4,5,6,7,0,2,1,3]}    
dic["s"]    
dic["indices"]    
    
dic["s"][0]    
dic["indices"][0]


## Jewels and stones

jewels = "aA"
stones = "aAAbbbb"

len(stones)

jewels = list(jewels)
stones = list(stones)

jewels
stones

stones[3] in jewels

twt = "z"
stn = "ZZ"

stn[1] in twt


def jewels_and_stones(jewels,stones):
  
    cool = []
    
    for i in range(len(stones)):
      
          result = stones[i] in jewels
          cool.append(result)
    
          
            
    if cool == True: 
        1
              
    elif cool == False: 
        0
              
        
    return sum(cool)
    

jewels_and_stones(jewels=twt,stones=stn)


## Coordinates
coords = [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7]]
coords[0][1]

coordsB = [[1,1],[2,2],[3,4],[4,5],[5,6],[7,7]]

bestA = []
bestB = []

odd = [0,2,4]
even = [1,3,5]

p = [1,2,3]
d = [1,2,3]

p == d

def coordinates(coordinates):
  
      bestA = []
      bestB = []
    
      odd = [0,2,4]
      even = [1,3,5]
      
      for i in odd:
              resultA = coordinates[i][1]
              bestA.append(resultA)
        
      for j in even:
              resultB = coordinates[j][0]
              bestB.append(resultB)
          
      if bestA == bestB:
          
          return True
        
      elif bestA != bestB:
          
          return False
          
   
coordinates(coordinates=coordsB)   
      

## Reverse string
s = ["h","e","l","l","o"]
s.reverse()
s

## Find the difference
s = "abcd"
t = "abcde"

s = list(s)
t = list(t)

b = "aaa"
c = "aa"

h = "ee"
y = "eeb"

o = " "
j = "y"

p = [True,True,True,True,False]
p

y[2] in h
t[4] in s
all(t)
all(p)


def difference(s,t):
      call = []
      hall = []
      
      s = list(s)
      t = list(t)
      
      for i in range(len(t)):
            call.append(t[i] in s)
            
      if all(call) == True:
            n = len(t) - len(s)
            return t[n+len(s)-1]
      else:
          for j in range(len(t)):
                k = t[j] in s
                hall.append(k)
          for p in range(len(hall)):
                if hall[p] == False:
                  return t[hall.index(False)]

difference(s = o,t = j)


## Pattern
pattern = "abba"
phrase = "dog cat cat dog"

pattern = list(pattern)
phrase = "dog cat cat dog".split(" ")
      
pattern1 = pattern[0] == pattern[len(pattern)-1]
pattern2 = pattern[0+1] == pattern[len(pattern)-(1+1)]

phrase1 = phrase[0] == phrase[len(phrase)-1]
phrase2 = phrase[0+1] == phrase[len(phrase)-(1+1)]

check1 = phrase1 == pattern1
check2 = phrase2 == pattern2

check_both = check1 ==check2

if check_both == True:
              
      return True
            
else: 
  
      return False
            




def pattern(pattern,phrase):

      pattern = list(pattern)
      phrase = "dog cat cat dog".split(" ")
      
      pattern1 = pattern[0] == pattern[len(pattern)-1]
      pattern2 = pattern[0+1] == pattern[len(pattern)-(1+1)]
      
      phrase1 = phrase[0] == phrase[len(phrase)-1]
      phrase2 = phrase[0+1] == phrase[len(phrase)-(1+1)]
      
      check1 = phrase1 == pattern1
      check2 = phrase2 == pattern2


      if len(phrase) == len(pattern):
            
              if check_both == True:
              
                      return True
            
              else: False
            
          
      else: False
          

pattern(pattern=pattern,phrase=phrase)


## Occurrence
s = "aabcbc"
s = list(s)
s.count(s[3])
tp = "aaabbb"
t.count("b")
len(t)
range(len(t))

st = list(set(s)) #Returns unique items in a list
st[0]
g=[2,3,4,2,2,2]
all(elem == 2 for elem in g) #Checks if all elements in list are the same
unique = list(set(s)) #Returns unique items in a list
store = []
      for i in range(len(unique)):
        result = s.count(unique[i])
        store.append(result)
print(store)

if(all(elem == store[0] for elem in store)):
  print (True)
else: print(False)
  

def occurrence(characters):
  
      unique = list(set(characters)) #Returns unique items in a list
      store = []
      for i in range(len(unique)):
        result = characters.count(unique[i]) #Counts frequency of each character
        store.append(result)
      
      if all(elem == store[0] for elem in store): #Checks if all elements in "store" are the same
        
        return True
      
      else: 
        
        return False

occurrence(characters=tp)


## To lower case
s = "LOVELY"
s.lower()

s = list(s)
s

def lowercase(text):
    text = list(text)
    lower = []
    for i in range(len(text)):
            result = text[i].lower()
            lower.append(result)
              
    return "".join(lower)

lowercase(text=s)


## Reverse string 2
p = "abcdefg"
p = list(p)
k = 2
p[0]

tsp = []
for i in range(k):
  result = p[i]
  tsp.append(result)

tsp_indx = []
tsp_rev = []
for j in range(len(tsp)):
  outcome = p.index(tsp[j])
  tsp_indx.append(outcome)

tsp_rev = tsp_indx.reverse()
print(tsp_rev)

store = []
for t in range(len(tsp_indx)):
  resultB = p[tsp_indx[t]]
  store.append(resultB)
  
print(store)

p[tsp_indx[0]] = store[0]
p[tsp_indx]


## Add strings
num1 = "456"
num2 = "77"

def addStrings(x,y):
      x = int(x)    #Converts string to integer
      y = int(y)
      
      total = x + y
      return str(total)    #Converts integer back to string


addStrings(x=num1,y=num2)

## Word equivalent
word1 = ["ab", "c"]
word2 = ["ac", "b"]

def equivalent(first,second):
  
      first = "".join(first)
      second = "".join(second)
    
      if(first == second):
        
        return True
      
      else:
        
        return False
          
equivalent(first=word1,second=word2)      
      
      
## Sum of digits after string
s = "iiii"
k = 1      
p = [9,9,9,9]
p = str(p)
p = int(p)
s = list(s)      
len(s)
s[0]

alphabets = "abcdefghijklmnopqrstuvwxyz"  
alphabets = list(alphabets)

location = []
for i in range(len(s)):
  location.append(alphabets.index(s[i]))
  
print(location) 

if(k == 1):
  location = str(location)
  location = location.replace("[","").replace("]","")
  location = location.replace(",","")
  location = "".join(location)
  location = int(location)



  
t = 8888 
g = str(t)
l = int(g)  
l 
 
## Uncommon words
s1 = "apple apple"
s2 = "this apple is sour"

s1 = s1.split(" ")
s2 = s2.split(" ")

s1.count("apple")



x = -121
x = list(str(x))
x
x = reversed(x)
x
x = int(x)

## Length of last word
s = "Hello World"
j = "   fly me   to   the moon  "

# strip() removes whitespaces at the beginning and end of a string
# lstrip() removes whitespace at the beginning of a string
# rstrip() removes whitespace at the end of a string

def lastword(s):
  
      s = s.strip()
      
      s = " ".join(s.split()) ## Removes duplicate whitespaces
      
      s = s.split(" ")
      
      # len(s)-1
      
      t = s[len(s)-1]
      
      return len(t)

lastword(s = j)


## Add digits
num = 94

# Use list comprehension to split a numeric vector

def add_digits(num):
  
      num = [int(a) for a in str(num)] 
      ans = sum(num)
      
      numB = ans
      numB = [int(a) for a in str(numB)]
      ansB = sum(numB)
      ansB = [int(a) for a in str(ansB)]
      
      
      # len(ansB)
      
      
      if len(ansB) == 1:
          
          # Remove square brackets
          return print(" ".join(map(str,ansB)))
      
      elif len(ansB) > 1:
          
          numC = ansB
          numC = [int(a) for a in str(numC)]
          ansC = sum(numC)
          
          # Remove square brackets
          return print(" ".join(map(str,ansC)))

add_digits(num=num)

## Reverse string
s =  "a good   example"

def reverse_string(s):

        s = s.strip()
              
        s = " ".join(s.split())
        
        s = s.split(" ")
        
        s.reverse()
        
        return " ".join(s)
     
reverse_string(s)        


## String without AAA or BBB
a = 1
b = 2

ar = "a" * a # Repeats an individual character
br = "b" * b

ar
br

cl = [ar,br]

"".join(cl)

def without(a,b):
    
      ar = "a" * a
      br = "b" * b
      cl = [ar,br]
      return "".join(cl)
  
without(a = a,b = b)  
  
## Multiply strings
bake = 3
lake = 2
