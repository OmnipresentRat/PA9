# MPG Model
# Nicholas Vollmer
# 11/8/26
# Linear regression of mtcars fuel efficiency (MPG) on vehicle weight


# Load the built-in mtcars dataset
data(mtcars)


# Scatter plot of MPG (y-axis) versus weight in 1,000 lbs (x-axis)
plot(mtcars$wt, mtcars$mpg,
     main = "Fuel Efficiency vs. Vehicle Weight",
     xlab = "Vehicle Weight (1,000 lbs)",
     ylab = "Miles per Gallon (MPG)",
     pch = 19, col = "steelblue")


# Linear regression using formula notation (response ~ predictor)
mpgModel <- lm(mpg ~ wt, data = mtcars)


# Add the fitted regression line to the scatter plot
abline(mpgModel, col = "firebrick", lwd = 2)


# Slope estimate, p-value, and 95% confidence interval
modelSummary <- summary(mpgModel)
slopeEstimate <- coef(modelSummary)["wt", "Estimate"]
slopePValue <- coef(modelSummary)["wt", "Pr(>|t|)"]
slopeConfInt <- confint(mpgModel, "wt", level = 0.95)

cat("Slope Estimate: ", slopeEstimate, "\n")
cat("p-value: ", slopePValue, "\n")
cat("95% Confidence Interval for Slope: ", slopeConfInt, "\n")


# Each additional 1,000 lbs is associated with about a 5.34 MPG decrease
# The slope is significant when the p-value is below the 0.05 threshold
if (slopePValue < 0.05) {
  cat("The slope is statistically significant at the 0.05 level,",
      "indicating\na significant relationship between weight and fuel",
      "efficiency.\n\n")
} else {
  cat("The slope is not statistically significant at the 0.05 level.\n\n")
}


# Predict MPG for a 6,000-pound vehicle (6 in thousands of lbs)
newCar <- data.frame(wt = 6000 / 1000)
mpgPrediction <- predict(mpgModel, newdata = newCar,
                         interval = "prediction", level = 0.95)


# Print the point estimate and 95% prediction interval
cat("Predicted MPG for a 6,000-pound vehicle:\n")
print(mpgPrediction)


# Check whether the new weight falls within the range of the dataset
weightRange <- range(mtcars$wt)
if (newCar$wt < weightRange[1] || newCar$wt > weightRange[2]) {
  cat("\nNote: The weight value is outside the range of the dataset,",
      "which\nlimits the accuracy of the prediction. Use caution when",
      "interpreting\nthis result.\n")
} else {
  cat("\nNote: The weight value is within the range of the dataset.\n")
}


# My concerns about the predictions are that the heaviest car in mtcars 
# is about 5,424 lbs, so 6,000 lbs is beyond the observed data. The lower
# prediction bound is negative, which feels like it is physically impossible,
# and the true relationship may not stay linear at more extreme weights. 
# So personally I don't think this prediction should be trusted.

