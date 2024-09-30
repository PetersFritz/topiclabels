# topiclabels
[![CRAN](https://www.r-pkg.org/badges/version/topiclabels)](https://cran.r-project.org/package=topiclabels)
[![R build status](https://github.com/PetersFritz/topiclabels/workflows/R-CMD-check/badge.svg)](https://github.com/PetersFritz/topiclabels/actions)

## Automated Topic Labeling with Language Models

topiclabels leverages (large) language models for automatic topic labeling. The main function converts a list of top terms into a label for each topic. Hence, it is complementary to any topic modeling package that produces a list of top terms for each topic. While human judgement is indispensable for topic validation (i.e., inspecting top terms and most representative documents), automatic topic labeling can be a valuable tool for researchers in various scenarios.

## References

*Related work*:
* [Grootendorst (2023). Topic Modeling with Llama 2. Create easily interpretable topics with Large Language Models](https://newsletter.maartengrootendorst.com/p/topic-modeling-with-llama-2?trk=feed_main-feed-card_feed-article-content)
* [Li et al. (2023). Can Large Language Models (LLM) label topics from a topic model?](https://osf.io/preprints/socarxiv/23x4m)
* [Wanna et al. (2024). TopicTag: Automatic Annotation of NMF Topic Models Using Chain of Thought and Prompt Tuning with LLMs](https://arxiv.org/abs/2407.19616)

*Topic models (selection)*:
* [BERTopic](https://github.com/MaartenGr/BERTopic)
* [Structural Topic Model](https://www.structuraltopicmodel.com/)
* [topicmodels](https://cran.r-project.org/package=topicmodels)
* [RollingLDA](https://github.com/JonasRieger/rollinglda)
* [ldaPrototype](https://github.com/JonasRieger/ldaPrototype)

## Contribution
This R package is licensed under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html).
For bug reports (lack of documentation, misleading or wrong documentation, unexpected behaviour, ...) and feature requests please use the [issue tracker](https://github.com/PetersFritz/topiclabels/issues).
Pull requests are welcome and will be included at the discretion of the author.

## Installation
You can install the recent CRAN version using 
``` r
install.packages("topiclabels")
```

For installation of the development version use [devtools](https://cran.r-project.org/package=devtools):

``` r
devtools::install_github("PetersFritz/topiclabels")
```

## (Quick Start) Example
``` r
library("topiclabels")
```
First of all, you should store your Huggingface token in the variable ``token``. If you do not have a token, create a Huggingface account and generate a token based on this [guideline](https://huggingface.co/docs/transformers.js/guides/private).
``` r
token = "" # set your hf token here
```
We would now like to label two topics, one with the three top terms *zidane, figo, kroos* and the other with the three top terms *gas, power, wind*.
``` r
topwords_matrix = matrix(c("zidane", "figo", "kroos", "gas", "power", "wind"), ncol = 2)
topwords_list = list(c("zidane", "figo", "kroos"), c("gas", "power", "wind"))
```
A common way to represent top terms is a matrix structure.
```
topwords_matrix
     [,1]     [,2]   
[1,] "zidane" "gas"  
[2,] "figo"   "power"
[3,] "kroos"  "wind" 
```
For our package, it is not necessary that all topics are characterized by the same number of top terms. For this case, the input must be given via the following list format:
```
topwords_list
[[1]]
[1] "zidane" "figo"   "kroos" 

[[2]]
[1] "gas"   "power" "wind" 
```
Using one of the following two calls
``` r
label_topics(topwords_matrix, token = token)
label_topics(topwords_list, token = token)
```
the labels for the two topics can then be generated, which yields
```
lm_topic_labels object generated using mistralai/Mixtral-8x7B-Instruct-v0.1
 1: Real Madrid Midfielders [zidane, figo, kroos]
 2: Renewable Energy [gas, power, wind]
```
Beyond this, it is also possible to display the actual generated outputs of the language models, which might be helpful if our default postprocessing function did not generate proper labels for single topics.
``` r
obj = label_topics(topwords_matrix, token = token)
names(obj)
# [1] "terms"       
# [2] "prompts"     
# [3] "model"       
# [4] "params"      
# [5] "with_token"  
# [6] "time"        
# [7] "model_output"
# [8] "labels"      
obj$model_output
# [1] "\n\n{\n\"label\": \"Real Madrid Midfielders\"\n}"
# [2] "\n\n{\n\"label\": \"Renewable Energy\"\n}"
obj$labels
# [1] "Real Madrid Midfielders"
# [2] "Renewable Energy"
```

Feel free to also check the following examples and check our [Vignette](https://htmlpreview.github.io/?https://github.com/PetersFritz/topiclabels/blob/main/performance/Compare_LLM_and_human_labels.html) of the package for further reading.
``` r
label_topics(list(c("zidane", "figo", "ronaldo"), c("gas", "power", "wind")), token = token)
label_topics(list("wind", "greta", "hambach"), token = token)
label_topics(list("wind", "fire", "air"), token = token)
label_topics(list("wind", "feuer", "luft"), token = token)
label_topics(list("wind", "feuer", "luft"), context = "Elements of the Earth", token = token)
```
