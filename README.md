# IMDb Data Analysis & Statistical Modeling

This project is a study on the factors that influence IMDb ratings. It covers the entire process from data collection to statistical analysis using R.

## 📊 Process Overview

### 1. Data Collection
I built a custom web scraper using `rvest` and `httr` to collect data from IMDb. 
* **Method**: The crawler started from the series *Dexter* and navigated through "More like this" links to gather over 4,000 titles.
* **Stability**: To prevent IP bans, I implemented User-Agent spoofing and randomized delays.
* **Reliability**: The script saves progress every 200 entries, allowing it to resume if the connection drops.

### 2. Data Preparation & EDA
Using `tidyverse`, I cleaned the raw data and created new variables to better understand the dataset:
* **Feature Engineering**: Grouped titles into **Eras** (e.g., Classic, Retro, Millennium, Digital, Streaming) and **Runtime Categories** (Short, Standard, Epic, Marathon).
* **Observation**: The data showed a noticeable decline in median ratings during the "Streaming" era compared to previous decades.
* **Format**: Movies and series were nearly balanced, with a slight majority of movies in the final dataset.

### 3. Statistical Modeling
The goal was to model how different variables (genres, duration, engagement) affect the final rating. I compared four different approaches:
* **GLM**: Established that TV Series have a +1.14 rating advantage over movies.
* **Elastic Net**: Used for feature selection, identifying that certain genres acted more as "noise" than significant predictors.
* **MARS**: Found non-linear tipping points in user engagement through hinge functions.
* **Random Forest**: Achieved the best fit with an **$R^2$ of 47.6%**.

## 📈 Key Findings
* **Engagement vs. Genre**: Behavioral metrics (how much users interact with a title) proved to be more significant predictors of a rating than the genre itself.
* **Duration**: Longer formats (Epic/Marathon) tend to correlate with higher ratings.
* **Format**: TV series consistently hold a statistical advantage in ratings within this dataset.

## 🛠 Tools Used
* **Language**: R
* **Key Packages**: `tidyverse`, `rvest`, `caret`, `glmnet`, `earth`, `ranger`.
