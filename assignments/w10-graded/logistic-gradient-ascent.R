require("mlbench")
data("PimaIndiansDiabetes")

df <- PimaIndiansDiabetes
y <- as.numeric(df$diabetes == "pos")
X <- df[, colnames(df)[colnames(df) != "diabetes"]]
X <- cbind(intercept=rep(1, nrow(X)), X)