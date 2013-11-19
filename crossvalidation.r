#Split data, a data frame in to k folds. If nrow(data)%k > 0, the first nrow(data)%k folds will be 1 row longer than the other folds.
createFolds <- function(data, k){
	n <- nrow(data)

	#Remainder that can't be assigned neatly
	r <- n%%k
	
	#Size of every fold, without remainder
	foldSize <- (n - r)/k

	print(paste("n:", n, "k:", k, "r:", r, "foldSize:", foldSize))
#	folds <- as.factor(sample(c(rep(c(1:foldSize), times=k),1:r), n))
	if(r>0){
		#Add remainder (1:r) to first r folds
		folds <- as.factor(sample(c(rep(c(1:k), times=foldSize),1:r), n))
	}
	else{
		folds <- as.factor(sample(rep(c(1:k), times=foldSize), n))
	}
	return(split(data, folds))
}

require(plyr)
require(randomForest)
#Simple example use for folds with random forest classifier.
test.classify <- function(x, dataFolds, classCol){
	#Glue folds together, excluding the first column: gets added by ldply.
	train <- na.omit(ldply(dataFolds[-x], data.frame)[,-1])
	test  <- na.omit(dataFolds[[x]])
	
	rf <- randomForest(y=train[,classCol], x=train[,-classCol])
	p <- predict(rf, test[,-classCol])
	return(data.frame(pred=p, truth=test[,classCol]))
}


