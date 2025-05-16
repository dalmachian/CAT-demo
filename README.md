# CAT-demo
Various R-based Computerized Adaptive Testing (CAT) simulations using the catR package, for comparing traditional and ML-enhanced methods of item selection. This work is for a Computer Science bachelors thesis project.

## Scripts
- Implements a standard CAT using the TCALS item bank (85 items, 4PL model)
- Simulates 100 students with normally distributed abilities
- Uses the catR package to simulate test taking behavior and item selection (EAP)
- Trains ML models (logistic regression and random forset) to predict student responses
- Compares CAT vs MLCAT using estimation accuracy (RMSE)

## Dataset used: TCALS
A matrix with 85 rows and five columns, respectively holding the discrimination, difficulty, pseudo-guessing and inattention parameters as calibrated on the results of the 1998 application of the TCALS questionnaire. The fifth column holds the name of the subgroups of items:
- Audio1: listening comprehension of sentences (items 1 to 12)
- Audio2: listening comprehension of dialogs and short texts (items 13 to 33)
- Written1: written vocabulary exercises (items 34 to 46)
- Written2: written grammar exercises (items 47 to 63)
- Written3: written exercises of other types: reading and mistake detection (items 64 to 85)

## Archive
- Creates a 3PL item bank with 50 items
- Simulates a test-taker with a known ability
- Selects items adaptively based on the Maximum Fisher Information (MFI) method

## Dependencies
```r
install.packages(c("catR", "caret", "randomForest", "rpart", "ggplot2"))
