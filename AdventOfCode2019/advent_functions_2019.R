# For question details, please see:
# https://adventofcode.com/2019

library(dplyr)
library(purrr)

# Day 1.1 ----------------------------------------

moduleFuel <- 
  function(mass) {
    # Summary: Fuel required to launch a given module 
    # is based on its mass. 
    
    # Input:
    #   mass: numeric mass of module
    
    # Output: Fuel required, calculated by taking mass, 
    # dividing by three, rounding down, and subtracting 2.
    # Negative fuel is treated as zero fuel. 
    
    # Examples:
    # moduleFuel(0) # 0
    # moduleFuel(12) # 2
    # moduleFuel(14) # 2
    # moduleFuel(1969) # 654
    # moduleFuel(100756) # 33583 
    if( !is.numeric(mass) ) {
      stop("Please enter a number")
    }
    
    max( floor( mass / 3 ) - 2, 0 )
  }

## Test: 
day_1_1_data <- 
  read.table(
    "~/anala_lytics/AdventOfCode2019/advent_inputs_2019/day_1_1", 
    quote = "\"", 
    comment.char = "", 
    stringsAsFactors = FALSE
  ) %>% 
  unlist()

sum(unlist(purrr::map(day_1_1_data, moduleFuel))) # 3506577


# Day 1.2 ----------------------------------------

moduleFuel4Fuel <- 
  function(mass) {
    # Summary: Fuel required to launch a given module 
    # is based on its mass, also considering additional 
    # fuel for the fuel you just added. 
    
    # Input:
    #   mass: numeric mass of module
    
    # Output: Fuel required, calculated by taking mass, 
    # dividing by three, rounding down, and subtracting 2.
    # Also considers that fuel also requires fuel, 
    # and that fuel requires fuel, and so on. Any mass that 
    # would require negative fuel is instead treated 
    # as if it requires zero fuel; the remaining mass, if any, 
    # is instead handled as having no mass.
    
    # Examples: 
    # moduleFuel4Fuel(0) # 0
    # moduleFuel4Fuel(14) # 2
    # moduleFuel4Fuel(1969) # 966
    # moduleFuel4Fuel(100756) # 50346
    if( !is.numeric(mass) ) {
      stop("Please enter a number")
    }
    
    fuel_for_fuel <- c()
    
    module_fuel <- moduleFuel(mass)
    
    fuel_for_fuel <- c(module_fuel)
    
    while(module_fuel > 0) {
      module_fuel <- moduleFuel(module_fuel)
      fuel_for_fuel <- c(fuel_for_fuel, module_fuel)
    }
    
    sum(fuel_for_fuel)
  }

## Test: 
day_1_2_data <- 
  read.table(
    "~/anala_lytics/AdventOfCode2019/advent_inputs_2019/day_1_2", 
    quote = "\"", 
    comment.char = "", 
    stringsAsFactors = FALSE
  ) %>% 
  unlist()

sum(unlist(purrr::map(day_1_2_data, moduleFuel4Fuel))) # 5256960


# Day 2.1 ----------------------------------------

intcodeGravAssist <-
  function(program) {
    # Summary: Using provided integers, starts by looking at 
    # the first integer = opcode (1 = sum, 2 = 
    # multiply, or 99), indicating what to do; for 
    # example, 99 means that the program is finished 
    # and should immediately halt. The first two 
    # integers after the oppcode indicate the 
    # positions from which you should read the input 
    # values, and the third after the oppcode 
    # indicates the position at which the output 
    # should be stored. Once you're done processing 
    # an opcode, move to the next one by stepping 
    # forward 4 positions.
    
    # Input:
    #   program: a list of integers separated by commas 
    #     (e.g., c(1, 2, 3, 4))
    
    # Output: new program  (list of integers) 
    # after implementing each oppcode. 
    
    # Examples:
    # intcodeGravAssist( c(1,9,10,3,2,3,11,0,99,30,40,50) ) # 3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50
    # intcodeGravAssist( c(1,0,0,0,99) ) # 2, 0, 0, 0, 99
    # intcodeGravAssist( c(2,3,0,3,99) ) # 2, 3, 0, 6, 99
    # intcodeGravAssist( c(2,4,4,5,99,0) ) # 2, 4, 4, 5, 99, 9801
    # intcodeGravAssist( c(1,1,1,4,99,5,6,0,99) ) # 30, 1, 1, 4, 2, 5, 6, 0, 99
    
    # Initialise program: 
    oppcode <- program[1]
    if( !oppcode %in% c(1, 2, 99) ) {
      stop('An incorrect oppcode was used -- something went wrong.')
    }
    
    input_positions <- program[2:3] + 1
    output_position <- program[4] + 1
    if( any( is.na(program[c(input_positions, output_position)]) ) ) {
      stop('Initial specified input / output positions do not match program length')
    }
    
    operation <- 
      ifelse(oppcode == 1, sum, 
             ifelse(oppcode == 2, prod,
                    NA))
    execute_operation <- operation(program[input_positions])
    
    new_program <- program
    
    while(oppcode != 99) {
      
      program[output_position] <- execute_operation
      
      new_program <- program[(length(program)-length(new_program) + 1):length(program)]
      new_program <- new_program[5:length(new_program)]
      
      # Initialise new program: 
      oppcode <- new_program[1]
      input_positions <- new_program[2:3] + 1
      output_position <- new_program[4] + 1 
      operation <- 
        try(
          ifelse(oppcode == 1, sum, 
                 ifelse(oppcode == 2, prod,
                        NA)
          ), 
          TRUE)
      execute_operation <- try(operation(program[input_positions]), TRUE)
    }
    
    return(program)
  }

## Test: 
day_2_data <- 
  read.csv(
    "~/anala_lytics/AdventOfCode2019/advent_inputs_2019/day_2", 
    header = FALSE, 
    stringsAsFactors = FALSE
  ) %>% 
  dplyr::mutate(
    V2 = 12,
    V3 = 2
  ) %>% 
  unlist() %>% 
  as.numeric()

intcodeGravAssist(day_2_data)[1] # 12490719


# Day 2.2 ----------------------------------------

intcodeNounVerb <- 
  function(output_number,
           program) {
    # Summary: Given a pair of inputs, determine which 
    # produces the output number of interest after 
    # replacing them in the program parameters. 
    
    # Input:
    #   output_number: number
    #   program: a list of integers separated by commas 
    #     (e.g., c(1, 2, 3, 4))
    
    # Output: A string with the inputs needed to result 
    # in the program output of interest. 
    for(i in 0:99) {
      for(j in 0:99) {
        noun <- i
        verb <- j
        
        mod_program <- program
        mod_program[2] <- noun
        mod_program[3] <- verb
        
        output <- intcodeGravAssist(mod_program)[1]
        if(output == 19690720) {
          return(
            paste0('The noun is ', noun, ' and the verb is ', verb, '.')
          )
        }
      }
    }
  }

## Test:
intcodeNounVerb(19690720, day_2_data) # "The noun is 20 and the verb is 3." (100 * 20 + 3)


# Day 3 ----------------------------------------


# Day 4 ----------------------------------------


# Day 5 ----------------------------------------


# Day 6 ----------------------------------------


# Day 7 ----------------------------------------


# Day 8 ----------------------------------------


# Day 9 ----------------------------------------


# Day 10 ---------------------------------------


# Day 11 ---------------------------------------


# Day 12 ---------------------------------------


# Day 13 ---------------------------------------


# Day 14 ---------------------------------------


# Day 15 ---------------------------------------


# Day 16 ---------------------------------------


# Day 17 ---------------------------------------


# Day 18 ---------------------------------------


# Day 19 ---------------------------------------


# Day 20 ---------------------------------------


# Day 21 ---------------------------------------


# Day 22 ---------------------------------------


# Day 23 ---------------------------------------


# Day 24 ---------------------------------------


# Day 25 ---------------------------------------


# Session Info:

# R version 3.4.1 (2017-06-30)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS  10.14.6
# 
# Matrix products: default
# BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/# A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
# [1] stats     graphics  grDevices
# [4] utils     datasets  methods  
# [7] base     
# 
# other attached packages:
# [1] purrr_0.2.3   dplyr_0.8.0.1
# 
# loaded via a namespace (and not attached):
#  [1] tidyselect_0.2.5 compiler_3.4.1  
#  [3] magrittr_1.5     assertthat_0.2.0
#  [5] R6_2.4.0         tools_3.4.1     
#  [7] pillar_1.3.1     glue_1.3.1      
#  [9] rstudioapi_0.9.0 tibble_2.0.1    
# [11] crayon_1.3.4     Rcpp_1.0.0      
# [13] pkgconfig_2.0.2  rlang_0.3.1  
