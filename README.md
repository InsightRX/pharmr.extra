<!-- badges: start -->
[![R-CMD-check](https://github.com/InsightRX/pharma_ai_r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/InsightRX/pharma_ai_r/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# pharmaair

pharmaAIR provides functions to facilitate automated analysis of clinical trial datasets,
including reading in .xpt files, wrangling data, plotting, and analysis.

Manual installation:

```r
remotes::install_github("InsightRX/pharma_ai_r")
```


# Docker

For use as a container in the AI architecture, build the docker container e.g. using:

```
docker build -t pharma_ai_r .
```

# Pushing image to ECR

```
## Build image: 
docker build -t apolloai:pharmaai .

## Tag:
docker tag apolloai/pharmaair 678372015519.dkr.ecr.us-west-2.amazonaws.com/pharma_ai_r:update-nm-functions

## Log in to ECR:
aws ecr get-login-password --region us-west-2 | docker login --username AWS --password-stdin 678372015519.dkr.ecr.us-west-2.amazonaws.com

## Push to ECR:
docker push 678372015519.dkr.ecr.us-west-2.amazonaws.com/pharma_ai_r:update-nm-functions

## To remove tag locally:
docker rmi 678372015519.dkr.ecr.us-west-2.amazonaws.com/pharma_ai_r:update-nm-functions
```