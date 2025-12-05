# Text-Prediction

## Overview
This is an NLP project for predicting the word coming after 1,2, or 3 words typed. Our training set is obtained from [HC Corpora](https://corpora.epizy.com/index.html). We used the blogs, news and twitter texts written in English (US). These files can be found in the repository as well. As there are so many lines in these files we randomly picked one hundred thousand of those. I also created a shiny app of this model which can be found in this [link](https://ovunceker.shinyapps.io/Text_Prediction/).

## Algorithm
The way our model work is that it takes input of 1,2, or 3 words, ***A B C*** for instance. Then it checks if ***A B C*** appears as a trigram in our training set. If it does, then among the whole 4-grams, it checks all of the ones with starting ***A B C*** and then provides the words which are the most probable to follow ***A B C***. If ***A B C*** does not belong to trigrams, then backs off and checks if ***B C*** belongs to bigrams. Then the algorithmn works similarly. 

## Probabilities 
Now in this project we didn't applied simple probabilities to the words following n-grams. For instance, suppose the words ***Francisco*** and ***eat*** appear both 10 times in the training set. Then for the simple unigram probability, they are equally likely to occur in a text. However, Kneser-Ney smoothing realizes that the word ***Fransico*** appears only right after the word ***San***, whereas the word ***eat*** has many more precedents. Thus, Kneser-Ney method realizes that the word ***eat*** is way more versatile than the word ***Francisco*** and that's why it should have bigger probability. Implementation of Kneser-Ney smoothing cost us to lose a little computation time but our model became more accurate.

## Math Behind Kneser-Ney Smoothing
For this method, the probability of word $w_i$ occuring after $w_{i-1}$ is given by 

$$P_{KN}(w_i|w_{i-1})=\frac{\max(c(w_{i-1},w_i)-\delta,0)}{\sum_{w'}c(w_{i-1},w')}+\lambda_{w_{i-1}}P_{KN}(w_i),$$

where $c(w,w')$ is the number of occurrences of the word $w$ followed by the word $w'$, $\delta$ is a constant called discount value, and $\lambda_{w_{i-1}}$ is a normalizing constant which is there so that the sum of probabilities is 1. Finally, 

```math
P_{KN}(w_i)=\frac{|\{w': 0 < c(w',w_i)\}|}{|\{(w',w'') : 0 < c(w',w'')\}|}
```
