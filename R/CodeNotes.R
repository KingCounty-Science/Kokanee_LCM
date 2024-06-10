#for loop code 
for (i in 1:5) {
  print(i)
}

#while loop code. Gives conditions under which to continue or stop a loop 
i <- 1
while (i <= 5) {
  print(i)
  i <- i + 1
}

# in base R, plotting multiple columns of data as lines
x <- 1:10
y1 <- x^2
y2 <- 2 * x

# Plot the first line
plot(x, y1, type = "l", col = "blue", xlab = "X", ylab = "Y", main = "Two Lines Plot")

# Add the second line
lines(x, y2, col = "red")

# Add a legend
legend("topleft", legend = c("Line 1", "Line 2"), col = c("blue", "red"), lty = 1)


#Kyle B code for 
#for assigning unique colors to lines # Add WTC series to dygraph with automatically selected colors
unique_colors <- rainbow(length(WTC_site_codes))  # Generating unique colors for each WTC site code
for (i in seq_along(WTC_site_codes)) {
  WTC_site_code <- WTC_site_codes[i]
  col_name_raw_WTC <- paste("Raw WTC", WTC_site_code)
  col_name_corrected_WTC <- paste("Corrected WTC", WTC_site_code)
  WTC_color <- unique_colors[i]