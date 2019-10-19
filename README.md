# ML

This dataset contains daily weather observations from numerous Australian weather stations.
The target variable RainTomorrow means: Did it rain the next day? Yes or No.
Note: You should exclude the variable Risk-MM when training a binary classification model. Not excluding it will leak the answers to your model and reduce its predictability.


RISKMM is the amount of rainfall in millimeters for the next day. It includes all forms of precipitation that reach the ground, such as rain, drizzle, hail and snow. And it was the column that was used to actually determine whether or not it rained to create the binary target. For example, if RISKMM was greater than 0, then the RainTomorrow target variable is equal to Yes.
Since it contains information about the future, and since it contains information directly about the target variable, including it would leak the future information to your model. Using it as a predictor to build a model and then testing on this dataset would give the false appearance of a high accuracy.
It is included in the dataset so that if you wanted to create your own binary target and decide that a really small amount of rain like 0.1 mm shouldn't be counted as a rainstorm, you could try predicting only more significant amounts of rain.
It's also included in the dataset so that, if you wanted to, you could use the dataset to build a regression machine learning model -- instead of classification. In other words, you can use RISK_MM as your target and drop RainTomorrow if you want to treat this as a regression problem instead of a classification problem.

