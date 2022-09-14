# With the data frame you created last week you will:

unique.char <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')
group.char <- c('x','x','x','x','x','x','x','x','x','x','c','c','c','v','v')
uniqu.num <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
rep.num <- c(1,1,1,1,1,1,1,1,2,2,2,4,4,4,4)
dec.num <- c(1,1,1,1,1,1,1,1,1,1,2.2,3.2,4.57,7.67,4.356)
df <- as.data.frame(cbind(unique.char,group.char,uniqu.num,rep.num,dec.num))
df$uniqu.num <- as.numeric(as.character(df$uniqu.num))
df$rep.num <- as.numeric(as.character(df$rep.num))
df$dec.num <- as.numeric(as.character(df$dec.num))
add.row <- data.frame("q","c",5,4,3.56)
colnames(add.row) <- colnames(df) 
df1 <- rbind(df, add.row)
row.names(df1) <- df1$unique.char
df1 <- df1[,-1]
df1

# Create a barplot for one numeric column, grouped by the character vector with 3 unique values
df.mean <- aggregate(df1$rep.num ~df1$group.char, FUN = "mean")
df.mean
colnames(df.mean) <- c("Factor","Mean")
df.mean
barplot(df.mean$Mean)
barplot(df.mean$Mean, names.arg = df.mean$Factor)
df.sd <- aggregate(df1$rep.num ~df1$group.char, FUN = "sd")
colnames(df.sd) <- c("Factor","StanDev")
df.sd
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, xlab = "Explanatory", ylab = "Response", main = "Baby's First Barplot")

  # Add error bars with mean and standard deviation to the plot
arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,5))
arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)#Need to put this line after 39 in order for someone to replicate your code.

# Change the x and y labels and add a title
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,5), xlab = "Explanatory", ylab = "Response", main = "Baby's First Barplot")#You did all of the right things here but never combined them into a single plot!
  # Export the plot as a PDF that is 4 inches wide and 7 inches tall.
#I couldn't recreate the plot without jumping around in your code - but you've got all the parts here!
# Create a scatter plot between two of your numeric columns.
plot(df1$dec.num ~ df1$rep.num)
plot(df1$dec.num ~ df1$rep.num, xlab = "Explanatory", ylab = "Response", main = "Scat plot (self-made)")

  # Change the point shape and color to something NOT used in the example.
plot(df1$dec.num ~ df1$rep.num, xlab = "Explanatory", ylab = "Response", main = "Scat plot (self-made)", pch=12)
plot(df1$dec.num ~ df1$rep.num, xlab = "Explanatory", ylab = "Response", main = "Scat plot (self-made)", pch=12, col = "saddlebrown")#forgot to change the axis labels.

  # Change the x and y labels and add a title
  # Export the plot as a JPEG by using the "Export" button in the plotting pane.

# Upload both plots with the script used to create them to GitHub.
  # Follow the same file naming format as last week for the script.
  # Name plots as Lastname_barplot or Lastname_scatterplot. Save them to your "plots" folder.
