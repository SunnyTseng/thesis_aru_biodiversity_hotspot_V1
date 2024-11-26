
# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(gt)



# load data ---------------------------------------------------------------




# bird parameters table ---------------------------------------------------

birdnet_argument <- c("i", "o", "lat", "lon", "week", "slist", "sensitivity",
                      "min_conf", "overlap", "rtype", "threads", "batchsize",
                      "locale", "sf_thresh", "classifier", "fmin", "fmax",
                      "output_file", "skip_existing_results")

default_value <- c("None", "None", "-1", "-1", "-1", "None", "1.0", "0.1",
                   "0", "table", "1", "1", "en", "0.03", "None", "0",
                   "15000", "None", "FALSE")

my_value <- c("--", "--", " ", " ", " ", " ", " ",
              " ", " ", "r", "4", "4",
              " ", " ", " ", " ", " ", "--", "TRUE")


birdnet_table <- data.frame(Argument = birdnet_argument,
                            Default = default_value,
                            MyValue = my_value,
                            stringsAsFactors = FALSE) %>% 
  gt() %>%
  cols_label(Argument = "Argument",
             Default = "Default value",
             MyValue = "Used value") %>%
  cols_align(align = "center") %>%
  tab_options(table.font.size = 12,
              heading.title.font.size = 16,
              heading.subtitle.font.size = 12)

gtsave(data = birdnet_table, filename = here("docs", "tables", "birdnet_parameters.rtf"))
