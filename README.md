# 📘 Replication Package for *"Am I Finished Yet?"*

This repository contains the replication materials for the master's thesis:

**"Am I Finished Yet? A discovery of burnout and ragequits within open-source projects"**  
Authored by **Wouter Mulder**, For a Master Thesis in Data Science at the Jheronimus Acadamy of Data Science, 2025.

---

## 🧠 Thesis Summary

Open-source software (OSS) plays a critical role in the digital infrastructure, yet the human aspects behind these collaborative communities—specifically burnout and ragequitting—remain underexplored. This thesis investigates whether such phenomena can be predicted by analyzing:

- Community structure ("community smells")
- Patterns of developer collaboration
- Natural language used in developer communication

A mixed-methods approach was applied to data from six Apache projects, using tools such as **Kaiaulu**, **GitHub**, **JIRA**, and **Apache Pony Mail**, combined with machine learning techniques for text and network analysis.

---

## 📁 Repository Structure

This GitHub repository contains all the scripts, models, and datasets necessary to reproduce the main experiments and findings in the thesis. Next to the source code, the following files are included:

```
.
├── data/                     # Processed datasets used in modeling
├── SO-vectors_200.bin        # Word2Vec model reutilised from Efstathiou et al. (2018)
├── custom-bert-tokenizer/    # The custom bert tokeniser which is created based on the data of Zhang et al. (2025)
├── dev-sentiment/            # Code and resources from BERT-CP model as created by Zhang et al. (2025)
├── SentiCR/                  # Sentiment analysis model (SentiCR) implementation
├── results/                  # Evaluation metrics, plots, and analysis outputs
└── README.md                 # This file
```

---

## 🔍 Utilised sources

### 🔹 `SentiCR/`

This directory contains the source code for the **SentiCR** module, a sentiment analysis tool for code review comments. While not the primary method used in the thesis, parts of the preprocessing associated with this module were used, among which the negation handling and emoji removal, for which the connections to the base dictionary is necessary.

> Source: Ahmed, A., SentiCR: Sentiment Analysis for Code Review, 2017.

---

### 🔹 `dev-sentiment/`

This folder provides an implementation of the **BERT-CP** model as introduced by:

> Haotian Zhang, Carlos Paradis, Rick Kazman, and Damian Andrew Tamburri.  
> *Making Cross-Platform Developer Sentiment Classification a Reality*, 2025.

This model was used for robust sentiment scoring of developer messages across GitHub, JIRA, and mailing lists, providing the foundation for ragequit prediction based on communication tone.

---

### 🔹 `SO-vectors_200.bin`

This folder provides an implementation of the **Pretained Word2Vec model** as introduced by:

> Vasiliki Efstathiou, Christos Chatzilenas and Diomidis Spinellis
> *Word Embeddings for the Software Engineering Domain*, 2018

This model was used for robust modelling of textual data to a vector space using a pretrained model which reflects the correlations of textual data in the Software Engineering field.

---

## ⚙️ Replicating the Study

For the replication of this study, there are 2 parts which can be used for the replication:

1. The data collection is performed through the  `data_retrieval.R`, `Kaiaulu_edits.R` and `standardised_data_retrieval.R`, these files, which will need to be ran on a linux system with a downloaded OSLOM and Perceval module, the data which can be seen in the data folder was retrieved from the appropriate sources

2. The notebooks in the main directory can be used to rediscover the insights obtained during the modelling of this data. It can be the case that the correct source paths need updating, but I have tried to keep it as accurate as possible


