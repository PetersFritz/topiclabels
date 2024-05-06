# topiclabels

## Automated Topic Labeling with Language Models

topiclabels leverages (large) language models for automatic topic labeling. The main function converts a list of top terms into a label for each topic. Hence, it is complementary to any topic modeling package that produces a list of top terms for each topic. While human judgement is indispensable for topic validation (i.e., inspecting top terms and most representative documents), automatic topic labeling can be a valuable tool for researchers in various scenarios.

## References

*Related work*:
* [Li et al. (2023). Can Large Language Models (LLM) label topics from a topic model?](https://osf.io/preprints/socarxiv/23x4m)
* [Grootendorst (2023). Topic Modeling with Llama 2. Create easily interpretable topics with Large Language Models](https://maartengrootendorst.substack.com/p/topic-modeling-with-llama-2?trk=feed_main-feed-card_feed-article-content)

*Topic models (selection)*:
* [BERTopic](https://github.com/MaartenGr/BERTopic)
* [Structural Topic Model](https://www.structuraltopicmodel.com/)
* [RollingLDA](https://github.com/JonasRieger/rollinglda)
* [ldaPrototype](https://github.com/JonasRieger/ldaPrototype)

## Contribution
This R package is licensed under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html).
For bug reports (lack of documentation, misleading or wrong documentation, unexpected behaviour, ...) and feature requests please use the [issue tracker](https://github.com/PetersFritz/topiclabels/issues).
Pull requests are welcome and will be included at the discretion of the author.

## Installation

For installation of the development version use [devtools](https://cran.r-project.org/package=devtools):

``` r
devtools::install_github("PetersFritz/topiclabels")
```

## (Quick Start) Example

TODO + Link to HTML Vignette

[Vignette](https://htmlpreview.github.io/?https://github.com/PetersFritz/topiclabels/blob/main/performance/Compare_LLM_and_human_labels.html)
