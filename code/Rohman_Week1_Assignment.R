# Now it is time to create your own data frame using the tools we have learned this week.
# First, resave this script as: your last name_Week1_Assignment
  # e.g. mine would be Wilson_Week1_Assignemnt


# Create 3 numeric vectors and 2 character vectors that are each 15 values in length with the following structures:
  # One character vector with all unique values
a <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')
typeof(a)

  # One character vector with exactly 3 unique values
b <- c('x','x','x','x','x','x','x','x','x','x','c','c','c','v','v')
typeof(b)

  # One numeric vector with all unique values
c <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
typeof(c)

  # One numeric vector with some repeated values (number of your choosing)
d <- c(1,1,1,1,1,1,1,1,2,2,2,4,4,4,4)
typeof(d)

  # One numeric vector with some decimal values (of your choosing)
e <- c(1,1,1,1,1,1,1,1,1,1,2.2,3.2,4.57,7.67,4.356)
typeof(e)

# Bind the vectors into a single data frame, rename the columns, and make the character vector with unique values the row names.
data <- cbind(a,b,c,d,e)
data
df <- as.data.frame(data)
df
colnames(df) <- c('dog','cat','fish','mouse','bird')
df
row.names(df) <- df$dog
df
# Remove the character vector with unique values from the data frame.
df.a <- df[,-1]
df.a

# Add 1 row with unique numeric values to the data frame.
Horse <- data.frame(1,2,3,4)
Horse
colnames(Horse) <- colnames(df.a)    
rbind(df.a, Horse)
df.h <- rbind(df.a, Horse)
df.h
row.names(df.h) <- c(row.names(df.a[1:15,]),"Horse")
df.h

# Export the data frame as a .csv file 
write.csv(df.h,"C:/GitHub/R4Eco_2022/Week1/RohmanHM.CSV", row.names = TRUE)# So close! this saves it to the course repository instead of your personal repository.

# Generate summary statistics of your data frame and copy them as text into your script under a new section heading.
summary(df.h)#Summary should have numbers for the numeric vectors.

#cat                fish              mouse               bird          
#Length:16          Length:16          Length:16          Length:16         
#Class :character   Class :character   Class :character   Class :character  
#Mode  :character   Mode  :character   Mode  :character   Mode  :character  

# Push your script and your .csv file to GitHub in a new "Week1" folder.


