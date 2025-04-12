# learning hashmaps with Mick
# 11 Apr 2025
rm(list=ls())
library(r2r) # hashmap package; link: https://cran.r-project.org/web/packages/r2r/vignettes/r2r.html
m = hashmap()

# we can insert key value pairs in m in different ways
m[["key"]] <- "value"
m[c(1, 2, 3)] <- c("a", "b", "c") # Vectorized over keys and values
m[[c(4, 5, 6)]] <- c("d", "e", "f") # Not vectorized

# the brackets are specifically telling to iterate through numbers or look for the set of numbers as a key
# The following queries explain the differences between the [[ and [ operator mentioned in the comments above:

m[["key"]] # two brackets work; this is how it got input
m["key"] # single brackets also work

m[c(1, 2, 3)] # single brackets ok for vectorized sets
m[[c(1, 2, 3)]] # double brackets fails

m[c(4, 5, 6)] # single brackets fails for non vectorized sets
m[[c(4, 5, 6)]] # double brackets works

insert(m, "user", "mick") # Modifies `m` in place
query(m, "user")

s = hashset() # can't have duplicates of things. arguably hashmap where value is just key so its key to key
insert(s, 1)
s[[2]] = TRUE # equivalent to insert(s, 2)
s[c(1, 2, 3)] # asking: does the set have 1, 2, or 3? (iterating)
query(s, c(1,2,3)) # this is asking: does the set have a 1,2,3 vector? - if I wanted to use query to ask 1, then 2 then 3, I would need to iterate (for loop?)

# when I do View(m) or View(s); it looks just like the View(model) for a statistical model I do in research stuff. Mick says this is because its just a good way that lots of people store things. So the people who made the packages/functions used hashmaps and hashsets :)
m[[ lm(wt ~ mpg, mtcars) ]] = list("This is my fit!", 840)
m[[ lm(wt ~ mpg, mtcars) ]] # this is weird to me bc lm is math = but thats the point; you can put anything in here
lm(wt ~ mpg, mtcars) # doesn't change the actual lm() output :0
m[[ lm(cyl ~ mpg, mtcars) ]]

m[[ list("This is my fit!", "cool beans") ]] = lm(wt ~ mpg, mtcars)
m[[ list("This is my fit!", "cool beans") ]] # can do reverse, gives output the same as just running lm()

# Setting default values
m = hashmap(default = 0)
m[["asdf"]]

# can use a default when making a "counter"
# we are going to ask how many times each key shows up in the list
objects = list("apple", "apple", "orange", "TUNA CAT", "orange", "apple")

for (i in objects)
  m[[i]] = m[[i]] + 1

m[["1"]] # not in list should be 0 (the default)
m[["TUNA CAT"]] # in list 1 time
m[["apple"]] # in list 3 times
m[["orange"]] # in list 2 times

# alternatively you can have exception; aka return error message
m = hashmap(on_missing_key = "throw")
tryCatch(m[["missing key"]], error = function(cnd) "Womp womp :( no keys here")
m[["1"]] # this is stupid. it will fail still, but you won't get the specific error message unless you are using tryCatch to look for error message (Mick says Rust better)

## my problem -- keys will be encountered more than once, and need to keep adding values onto existing values (sort of) for a given key
# need to know: how to add an element to the end of a list
a = c("apple","bonana","kiwi")
a = append(a, "balls")
a = append(a, c("Mick","Dylan","Will"), after = length(1)); a

# different dates that kitties have been Rat
kitties = c("Chmall","Mees","FooCat","Chmall","Mees","Mees","Mees","FooCat","Chmall","Mees")
date = c("Monday","Monday","Monday","Tuesday","Tuesday","Wednesday","Thursday","Thursday","Friday","Friday")
type = c("scratch","scream","blind","scratch","booboo","sprint","scream","booboo","sprint","scream")
ratarchive = data.frame(kitties, date, type)
ratarchive = ratarchive %>% mutate(datetype = paste(date, type)); head(ratarchive,10)

nrow(ratarchive)

ratmap = hashmap(default = list())
# In English, what I want to do: I want to have Chmall Mees and FooCat as keys, to store each date and type they were rat
# This means I need to assign names as keys, then iterate through the dataframe to add each date,type as values ; making sure that each time I encounter a given key again, I am ADDING on to existing values, not overwriting.
for(i in 1:10) {
  cat = ratarchive[i,1]
  date = ratarchive[i,2]
  type = ratarchive[i,3]
  rlist::list.append(ratmap[cat], date) # not sure what R is doing with lists, it doesnt write the value
  ratmap[cat]
}

ratmap["Chmall"]

head(ratarchive, 10)
ratarchive[1,1]
ratarchive[1,"kitties"]
ratarchive[2,]
ratarchive[2,1]
ratarchive[1]=="Mees"
