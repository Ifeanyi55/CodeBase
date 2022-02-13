print("It is a good way to start")
numbers = [10,11,7,17,2,13]
numbers


pip install spacy

import seaborn as sns
numbers6 = list(numbers)
numbers6

import array as arr
new = arr.array("i",[2,4,6,8])
print("First element:",new[0])
print("Second element:",new[1])
print("Third element:",new[2])

45+3 > 10
10 >= 10

a = "Hello guys"
b = ["Hello guys","let's go"]
b + [1,2,3,4]

# Creating a dictionary
c = {"favquote":"Hello world"}
c["favquote"]

# Creating a set, which returns repeated words once
d = {1,2,3,4,5,5,5,6,6,6}
d

# Conditional statements
if 1 == 1:
  print("That's correct")
x = 2
b = 3
if x < b:
  print("Obvious")
  
if 3 != 5:
  print("That is expected")
  
for item in b:
  print(item)
  
for item in range(0,10):
  print(item)
  
x = 10


while x > 0:
  print(x)
  x -= 1

# Create a function  
def func():
  print("Hello guys")
  
func();func()  


## Create while loop algorithm
age = (20,74,13,65,30,79)
address = (10,8,75,31,54,41)
income = (100,350,751,440,675,501)
age
df = pd.DataFrame(age,address,income,columns=RangeIndex(0,2)
df
counter = 0

def maxFun(data):
    while(counter < 3):
        counter = counter+1
        outcome = max(data[counter])
        print(outcome,"is the highest figure")

maxFun(data = )        

def func(x,y):
  z = x+y
  print(z)

func(x = 2,y = 3)

# Create an object
class Person:
  def __init__(self):
    print("New person")
    
p = Person()

v = [1,2,3]
for i in range(len(v)):
result = v[i] * 2
	print(result)

import math

print(math.pi)

class Num:
  def __init__(self):
    print(12,8,15,11,3,8,14)

Num(); Num()

num = [12,8,15,11,3,8,14]
num
for item in num:
  print(item > 10)

# Iterating through a string using list comprehension
h_letters = [letter for letter in "human"]

print(h_letters)


True
False
True or False
True and False

pip install spacy

## New project
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

df = pd.read_csv("bestsellers.csv")
df

## To return the first five rows
print(df.head())

## To get number of rows and columns

print(df.shape)

## To know column names

print(df.columns)

## To get the data type of each column

print(df.dtypes)

## To get more information about the data

print(df.info)

## To get a particular column name

dt = df["Genre"]
dt

## To see the first five rows of the column

print(dt.head())

## To see first five rows of more than one column

sub = df[["Name","Genre"]]
sub

## To see the first and hundredth row of my data

print(df.loc[0])
print(df.loc[99])

## To get the last row

print(df.tail(n = 1))

## To subset multiple rows

print(df.loc[[0,99,199]])

## To get details about the 3 row

print(df.iloc[2])

## To get the details about the last row

print(df.iloc[-1])

## To subset columns, use the colon : sign

subset = df.loc[:,["Author","Price"]]
print(subset.head())

## To subset using integers

subset = df.iloc[:,[2,4,-1]]
print(subset.head())

## Subsetting with range

small_range = list(range(5))
subset = df.iloc[:,small_range]
print(subset.head())

## Subsetting rows and columns at the same time

print(df.loc[100,"Year"])

print(df.iloc[100,1])


## To get the 1st, 100th, and 400th rows from the 1st, 4th, and 6th columns

print(df.iloc[[0,99,399],[0,3,5]])

## Use column name for the same as above

print(df.loc[[0,99,399],["id","User Rating","Price"]])

print(df.loc[10:20,["id","User Rating","Price"]])

## To see the first 10 rows in the data set

print(df.head(n = 10))

## To group data by Author and calculate the mean Price of their books

print(df.groupby("Author")["Price"].mean())

## Creating a multi-group or stacked table

multi_group = df.groupby(["Author","Name"])[["Price","User Rating"]].mean()
print(multi_group)

## To flatten the data frame

flat = multi_group.reset_index()
print(flat.head(5))

## Grouped frequency count

print(df.groupby("Author")["Name"].nunique())

## Basic plot of data

rating = df.groupby("Price")["User Rating"].mean()
print(rating)

rating.plot()

price = print(df[df["Price"] > 20])

price_rating_titles = df.loc[(df["Price"] > 10) & (df["User Rating"] > 3 )]

price_rating_titles

####Bio Timelines####

bio = pd.read_csv("BioTimeline.csv",encoding = "ISO-8859-1")

## Subsetting by tweets only

bio_tweets = bio[bio["is_retweet"] == "FALSE"]

bio_tweets


GM = pd.read_csv("GMTrends.csv")

print(GM.head())

s = "hello my name is Sam"
s.lower()

s.upper()

s.split()

## Loop through csv files
os.getcwd() # Check working directory

sp = pd.read_csv("SP.csv")
sp

all_files = ["SP.csv","SPA.csv","SPB.csv"]

li = []
for filename in all_files:
  df = pd.read_csv(filename,index_col = None,header = 0)
  li.append(df)
  
li[0]

df = pd.concat(li,axis = 0,ignore_index = True)
df

pip install seaborn
import seaborn as sns
tips = sns.load("tips")

## Collections
import collections as cl
from collections import deque
from collections import namedtuple
from collections import Counter

## Determing maximum number of items in list
a = deque(maxlen = 5)
a.append(1)
a.append(2)
a.append(3)
a.append(4)
a.append(5) ## Full capacity
a.append(6)
a

## Append lists at the beginning or end
b = deque(maxlen = 5)
b.append(1)
b.append(2)
b.appendleft(3) ## Appends list at the beginning
b.pop() ## Removes from the right side
b.popleft() ## Removes from the left side
b

## Named tuples
point = namedtuple("point",["x","y","z"])
p = point(3,4,5)
print(p.x,p.y,p.z)

## Create a new instance of a namedtuple
p1 = point(3,4,5)
p2 = point(x=3, y=4, z=5)
p3 = point._make([3,4,5])
p3

## Define the default value for some of the parameters
pointDef = namedtuple("pointDef","x,y,z",defaults = [0,0,0])
p = pointDef(x = 1)
p

## Counter
c = Counter("aaabbccdaaab") ## Returns a dictionary of counts
c

## Check the number of occurences for a specific value
print(c["a"])
print(c["c"])

## Check the most common values
print(c.most_common(3))

## Add or subtract occurences
d = Counter("abbc")
d.update("bccd")
d.subtract("bbc")
d

j = print("Hello world")

pip install spacy

