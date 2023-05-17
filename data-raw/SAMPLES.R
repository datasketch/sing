library(googlesheets4)
library(hdtype)
library(hdtable)
library(jsonlite)
data <- read_sheet("https://docs.google.com/spreadsheets/d/1kZdsjCh_smoWizxvg2abljb5b61P3fPoF-38aEcepHA/edit#gid=1655853030")
input_recommender(data = data)


json_inputs <- read_json("data-raw/data/format/inputs.json")
