LitteraLector
=============

A repo for Malone-Balogne and I to work on the kaggle character recognition challenge.

Stage 1: Get kaggle dataset?
Stage 2: Build/apply Support Vector Machine to the kaggle dataset
Stage 3: ???
Stage 4: Profit!


I'm picturing LitteraLector.R as a general purpose macro that calls various techniques, i.e., SVM, RandomForest, or NN
ultimately perhaps comparing results

utilityBelt.R holds various helper functions, right now mostly for reducing the resolution of the character images (i.e. 28x28 to 14x14)

This was done to reduce the number of features my crummy laptop has to deal with

rageAgainsTheSupportVectorMachine.R is the file for SVM functions

randomForestForTheTrees.R is the file for Random Forest functions

(clearly a NN file will have a Skynet or Terminator pun in it ... or maybe Neuromancer)