setwd("/users/sgriffin/Documents//GitHub/MultivariateStats/Class 5/")
# Now, read the file data into R
df <- read.csv("nycdoe.csv")
#Now, the data is stored

#View data
View(df)
#Look at variable names
head(df)

# column_names = colnames(df)
# column_names = c(column_names)
df = na.omit(df)

attach(df)

for (columns in df[1:69]){
  print(columns)
  # plot(columns, collegeenroll_percent)
  # regression = lm(collegeenroll_percent~columns, data=df)
  # summary(regression)
}

plot(score_environment,collegeenroll_percent)

nyc_regression<-lm(collegeenroll_percent~score_environment+score_progress+safety_mean_nonviolent+safety_mean_property+safety_mean_violent, data=df)
summary(nyc_regression)