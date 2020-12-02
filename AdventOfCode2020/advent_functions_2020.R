# For question details, please see:
# https://adventofcode.com/2020

## Packages -----------------------

library(dplyr)
library(stringr)
library(tidyr)


## Day 1 --------------------------

get2020sumx <- 
  function(
    list_n,
    num_used = 2,
    sum_val  = 2020
  ) {
    # Purpose: find the number of entries of interest 
    # that sum to value of interest and then 
    # multiply those numbers together
    
    # Arg: 
    #   list_n: a list of numbers of interest
    #   num_used: how many numbrs to use to find
    #             meeting sum of interest. Defaults
    #             to using 2 numbers. 
    #   num_val: the sum of interest. Defaults
    #            to 2020. 
    
    # Output: numeric value of two numbers
    # multiplied together 
    require(dplyr)
    
    list_n_df <- 
      data.frame(
        matrix(
          rep(list_n, num_used),
          nrow = length(list_n),
          byrow = F
        ),
        stringsAsFactors = F
      )
    
    expand_list_n <- 
      expand.grid(list_n_df) %>% 
      as.data.frame() %>% 
      mutate(sum_val_eq = rowSums(.) == sum_val)
    
    sum_val_x <- 
      expand_list_n %>% 
      filter(sum_val_eq == T) %>% 
      .[1,1:num_used] %>% 
      unlist() %>% 
      prod()
    
    return(sum_val_x)
  }

### test test test ................ 

test_list_n <- c(1721, 979, 366, 299, 675, 1456)
get2020sumx(test_list_n, 2) # 514579
get2020sumx(test_list_n, 3) # 241861950

data_day_1 <- unlist(
  read.table(
    "~/anala_lytics/AdventOfCode2020/advent_inputs_2020/data_day_1", 
    quote="\"", 
    comment.char="", 
    stringsAsFactors=F
  )
)
get2020sumx(data_day_1, 2) # 996996
get2020sumx(data_day_1, 3) # 9210402


## Day 2 --------------------------

numValidPw <- 
  function(
    format_file_pw,
    version = c('old', 'toboggan')
  ) {
    # Purpose: Counts how many passwords are valid according 
    # to various interpretation of the policies
    
    # Arg: 
    #   format_file_pw: a data frame formatted for input
    #                   into the function
    #   version: whether old or toboggan versions of the
    #            policy is used 
    
    # Output: numeric value of number of valid passwords 
    require(dplyr)
    
    if (!all(c('min_rep', 'max_rep', 'letter_rep', 'password') %in% 
             colnames(format_file_pw))) {
      stop('Please reformat password with rules file.')
    }
    
    if (version == 'old') {
      how_many_valid_passwords <- 
        format_file_pw %>% 
        rowwise() %>% 
        mutate(
          n_rep = length(unlist(str_extract_all(password, letter_rep))),
          valid_password = n_rep >= min_rep & n_rep <= max_rep
        ) %>% 
        ungroup() %>% 
        summarise(sum(valid_password))
      
    } else if (version == 'toboggan') {
      how_many_valid_passwords <-
        format_file_pw %>% 
        mutate(
          min_letter = substr(password, min_rep, min_rep),
          max_letter = substr(password, max_rep, max_rep),
          valid_password = (
            min_letter == letter_rep | 
              max_letter == letter_rep
            ) & 
            min_letter != max_letter
        ) %>% 
        summarise(sum(valid_password))
    }
    
    return(as.numeric(how_many_valid_passwords))
  }

### test test test ................ 

data_day_2 <- 
  read.table(
    "~/anala_lytics/AdventOfCode2020/advent_inputs_2020/data_day_2", 
    quote="\"", 
    comment.char="", 
    stringsAsFactors=F
  ) %>% 
  separate(V1, c('min_rep', 'max_rep'), sep = "-") %>% 
  transmute(
    min_rep = as.numeric(min_rep),
    max_rep = as.numeric(max_rep),
    letter_rep = gsub(":", "", V2),
    password = V3
  )

numValidPw(data_day_2, version = 'old') # 538
numValidPw(data_day_2, version = 'toboggan') # 489


# Session Info ==================== 

# R version 3.4.1 (2017-06-30)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS  10.16
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets 
# [6] methods   base     
# 
# other attached packages:
#   [1] tidyr_0.6.0   stringr_1.2.0 dplyr_0.8.0.1
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.0       crayon_1.3.4     assertthat_0.2.0
# [4] R6_2.4.0         magrittr_1.5     pillar_1.3.1    
# [7] rlang_0.3.1      stringi_1.2.4    rstudioapi_0.9.0
# [10] tools_3.4.1      glue_1.3.1       purrr_0.2.3     
# [13] compiler_3.4.1   pkgconfig_2.0.2  tidyselect_0.2.5
# [16] tibble_2.0.1    
