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
day_1_data <- 
  read.table(
    "~/anala_lytics/AdventOfCode2019/advent_inputs_2019/day_1", 
    quote = "\"", 
    comment.char = "", 
    stringsAsFactors = FALSE
  ) %>% 
  unlist()

sum(unlist(purrr::map(day_1_data, moduleFuel))) # 3506577


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
sum(unlist(purrr::map(day_1_data, moduleFuel4Fuel))) # 5256960


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


# Day 3 ------------------------------------------

expandWirePath <- 
  function(wire_path) {
    # Summary: Given a wire path, determine each
    # point on the grid the wire crosses. 
    
    # Input:
    #   wire_path: A wire path in the form of [RUDL][0:9]
    
    # Output: a data frame with each point on the grid
    # the wire crossed returned as x and y from 0, 0. 
    
    # Example:
    # expandWirePath(c('R8','U5','L5','D3'))
    require(dplyr)
    
    wire_path_expanded <- list(NULL)
    
    for(i in 1:length(wire_path)) {
      current_wire_path <- wire_path[i]
      current_wire_direction <- substr(current_wire_path, 1, 1)
      current_wire_steps <- as.numeric(substr(current_wire_path, 2, nchar(current_wire_path)))
      
      wire_path_expanded[[i]] <- 
        if( current_wire_direction == 'R' ) {
          expand.grid(rep(1, current_wire_steps), 0)
        } else 
          if( current_wire_direction == 'L' ) {
            expand.grid(-rep(1, current_wire_steps), 0)
          } else 
            if( current_wire_direction == 'U' ) {
              expand.grid(0, rep(1, current_wire_steps))
            } else 
              if( current_wire_direction == 'D' ) {
                expand.grid(0, -rep(1, current_wire_steps))
              } else {
                stop('The direction does not make sense.')
              }
    }
    
    dplyr::bind_rows(wire_path_expanded) %>% 
      dplyr::transmute(
        x = cumsum(Var1),
        y = cumsum(Var2)
      )
  }


wireClosestCross <- 
  function(wire_1_path,
           wire_2_path) {
    # Summary: Given two wires connected to a central 
    # port (0, 0), provides the Manhattan distance from 
    # the central port to the closest intersection between 
    # the two wires, as well as the shortest steps to an 
    # intersection.
    
    # Input:
    #   wire_1_path: first wire path in the form of [RUDL][0:9]
    #   wire_2_path: second wire path in the form of [RUDL][0:9]
    
    # Output: numeric shortest distance to closest intersection,
    # and numeric fewest steps to an intersection. 
    
    # Examples:
    # wireClosestCross('R8,U5,L5,D3', 'U7,R6,D4,L4') # shortest distance = 6, fewest steps = 30
    # wireClosestCross('R75,D30,R83,U83,L12,D49,R71,U7,L72', 'U62,R66,U55,R34,D71,R55,D58,R83') # distance = 159, fewest steps = 610
    # wireClosestCross('R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51', 'U98,R91,D20,R16,D67,R40,U7,R15,U6,R7') # distance = 135, steps = 410
    require(dplyr)
    
    wire_1_path <- unlist(strsplit(wire_1_path, ","))
    wire_2_path <- unlist(strsplit(wire_2_path, ","))
    
    wire_1_path_expanded <- 
      expandWirePath(wire_1_path) %>% 
      dplyr::mutate(
        n_steps_1 = row_number()
      )
    
    wire_2_path_expanded <- 
      expandWirePath(wire_2_path) %>% 
      dplyr::mutate(
        n_steps_2 = row_number()
      )
    
    wire_path_crosses <- 
      dplyr::inner_join(
        wire_1_path_expanded,
        wire_2_path_expanded,
        by = c("x", "y")
      ) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(
        manhatten_dist = sum(abs(x), abs(y))
      ) %>% 
      dplyr::ungroup()
    
    
    shortest_path <- min(wire_path_crosses$manhatten_dist)
    
    shortest_path_steps <- 
      wire_path_crosses %>% 
      dplyr::mutate(
        sum_n_steps = n_steps_1 + n_steps_2
      ) %>% 
      dplyr::slice(which.min(sum_n_steps)) %>% 
      dplyr::select(sum_n_steps)%>% 
      unlist() %>% 
      as.numeric()
    
    return( c(shortest_manhatten_distance = shortest_path, 
              fewest_steps = shortest_path_steps) )
  }

## Test: 
wireClosestCross(
  'R992,U284,L447,D597,R888,D327,R949,U520,R27,U555,L144,D284,R538,U249,R323,U297,R136,U838,L704,D621,R488,U856,R301,U539,L701,U363,R611,D94,L734,D560,L414,U890,R236,D699,L384,D452,R702,D637,L164,U410,R649,U901,L910,D595,R339,D346,R959,U777,R218,D667,R534,D762,R484,D914,L25,U959,R984,D922,R612,U999,L169,D599,L604,D357,L217,D327,L730,D949,L565,D332,L114,D512,R460,D495,L187,D697,R313,U319,L8,D915,L518,D513,R738,U9,R137,U542,L188,U440,R576,D307,R734,U58,R285,D401,R166,U156,L859,U132,L10,U753,L933,U915,R459,D50,R231,D166,L253,U844,R585,D871,L799,U53,R785,U336,R622,D108,R555,D918,L217,D668,L220,U738,L997,D998,R964,D456,L54,U930,R985,D244,L613,D116,L994,D20,R949,D245,L704,D564,L210,D13,R998,U951,L482,U579,L793,U680,L285,U770,L975,D54,R79,U613,L907,U467,L256,D783,R883,U810,R409,D508,L898,D286,L40,U741,L759,D549,R210,U411,R638,D643,L784,U538,L739,U771,L773,U491,L303,D425,L891,U182,R412,U951,L381,U501,R482,D625,R870,D320,L464,U555,R566,D781,L540,D754,L211,U73,L321,D869,R994,D177,R496,U383,R911,U819,L651,D774,L591,U666,L883,U767,R232,U822,L499,U44,L45,U873,L98,D487,L47,U803,R855,U256,R567,D88,R138,D678,L37,U38,R783,U569,L646,D261,L597,U275,L527,U48,R433,D324,L631,D160,L145,D128,R894,U223,R664,U510,R756,D700,R297,D361,R837,U996,L769,U813,L477,U420,L172,U482,R891,D379,L329,U55,R284,U155,L816,U659,L671,U996,R997,U252,R514,D718,L661,D625,R910,D960,L39,U610,R853,U859,R174,U215,L603,U745,L587,D736,R365,U78,R306,U158,L813,U885,R558,U631,L110,D232,L519,D366,R909,D10,R294',
  'L1001,D833,L855,D123,R36,U295,L319,D700,L164,U576,L68,D757,R192,D738,L640,D660,R940,D778,R888,U772,R771,U900,L188,D464,L572,U184,R889,D991,L961,U751,R560,D490,L887,D748,R37,U910,L424,D401,L385,U415,L929,U193,R710,D855,L596,D323,L966,D505,L422,D139,L108,D135,R737,U176,R538,D173,R21,D951,R949,D61,L343,U704,R127,U468,L240,D834,L858,D127,R328,D863,R329,U477,R131,U864,R997,D38,R418,U611,R28,U705,R148,D414,R786,U264,L785,D650,R201,D250,R528,D910,R670,U309,L658,U190,R704,U21,R288,D7,R930,U62,R782,U621,R328,D725,R305,U700,R494,D137,R969,U142,L867,U577,R300,U162,L13,D698,R333,U865,R941,U796,L60,U902,L784,U832,R78,D578,R196,D390,R728,D922,R858,D994,L457,U547,R238,D345,R329,D498,R873,D212,R501,U474,L657,U910,L335,U133,R213,U417,R698,U829,L2,U704,L273,D83,R231,D247,R675,D23,L692,D472,L325,D659,L408,U746,L715,U395,L596,U296,R52,D849,L713,U815,R684,D551,L319,U768,R176,D182,R557,U731,R314,D543,L9,D256,R38,D809,L567,D332,R375,D572,R81,D479,L71,U968,L831,D247,R989,U390,R463,D576,R740,D539,R488,U367,L596,U375,L763,D824,R70,U448,R979,D977,L744,D379,R488,D671,L516,D334,L542,U517,L488,D390,L713,D932,L28,U924,L448,D229,L488,D501,R19,D910,L979,D411,R711,D824,L973,U291,R794,D485,R208,U370,R655,U450,L40,D804,L374,D671,R962,D829,L209,U111,L84,D876,L832,D747,L733,D560,L702,D972,R188,U817,L111,U26,L492,U485,L71,D59,L269,D870,L152,U539,R65,D918,L932,D260,L485,U77,L699,U254,R924,U643,L264,U96,R395,D917,R360,U354,R101,D682,R854,U450,L376,D378,R872,D311,L881,U630,R77,D766,R672'
) # shortest manhatten distance = 357; fewest steps = 101956


# Day 4.1 ----------------------------------------

is.password <-
  function(potential_password) {
    # Summary: Given a potential password, identify if
    # two adjacent digits are the same (like 22 in 122345); and 
    # when going from left to right, the digits never decrease;
    
    # Input:
    #   potential_password: charater of 6 digit numeric password
    
    # Output: boolean (T/F) for if the potential password meets
    # all password requirements. 
    
    # Examples: 
    # is.password('122345') # T
    # is.password('111123') # T
    # is.password('135679') # F
    # is.password('111111') # T
    # is.password('223450') # F
    # is.password('123789') # F
    potential_password_str <- as.numeric(unlist(strsplit(potential_password, '')))
    
    if(length(potential_password_str) != 6) {
      stop("The password must be 6 digists long")
    }
    
    difference <- NULL
    repeated   <- NULL
    
    for( i in 1:(length(potential_password_str)-1) ) {
      difference[i] <- potential_password_str[[i+1]] - potential_password_str[[i]]
      repeated[i] <- potential_password_str[[i+1]] == potential_password_str[[i]]
    }
    
    any(repeated) & all(difference >= 0)
  }

## Test: 
sum(unlist(lapply(as.character(234208:765869), is.password))) # 1246  


# Day 4.2 ----------------------------------------

longRep <- 
  function(stringz){
    # Summary: Implement a method to identify if two adjacent 
    # matching characters are not part of a larger group of 
    # matching characters. 
    
    # Input: 
    #   stringz: a string of characters 
    
    # Output: boolean (T/F) for if the string has repeated 
    # characters. 
    
    # Examples:
    # longRep('112233') # T
    # longRep('123444') # F
    # longRep('111122') # T
    stringz_split <- unlist(strsplit(as.character(stringz), ""))
    
    stringz_num <- NULL
    for(i in 1:length(stringz_split)) {
      
      if(i > 1) {
        if(!exists('counter')) {
          counter <- 1
        }
        
        if(stringz_split[i] != stringz_split[i-1]) {
          counter <- 1
          stringz_num[i] <- counter
          
        } else {
          counter <- counter + 1
          stringz_num[i] <- counter
        }
        
      } else {
        stringz_num[i] <- 1
      }
      
    }
    
    stringz_num_chr <- paste(stringz_num, collapse = "")
    
    (grepl('2', stringz_num_chr) & !grepl('23', stringz_num_chr)) |
      (grepl('23', stringz_num_chr) & (grepl('121', stringz_num_chr) | endsWith(stringz_num_chr, '2')))
  }

## Test: 
multi_password <- 
  function(x) {
    is.password(x) & longRep(x)
  }

sum(unlist(lapply(as.character(234208:765869), multi_password))) # 814


# Day 5.1 ----------------------------------------

intcodeDiagnosis <- 
  function(program,
           input = 1) {
    # Summary: Given a diagnostic program, provide 
    # all successful diagnostic tests until a failing
    # diagnostic test code. 
    
    # Input:
    #   program: a list of integers separated by commas 
    #     (e.g., c(1, 2, 3, 4))
    # Input: a number to input
    
    # Output: A list of output codes with last number in the
    # list being the diagnostic code. 
    output <- list()
    
    i <- 1
    
    while(i > 0 & i < length(program)) {
      
      opp_param <- 
        paste(
          c( rep(0, 5-nchar(program[i])), 
             as.character(program[i]) ),
          collapse = ''
        )
      
      oppcode <- as.numeric( substr(opp_param, 4, 5) )
      
      param_1 <- program[1+i] + 1
      param_2 <- program[2+i] + 1
      param_3 <- program[3+i] + 1
      
      param_mode_1 <- as.numeric( substr(opp_param, 3, 3) )
      param_mode_2 <- as.numeric( substr(opp_param, 2, 2) )
      param_mode_3 <- as.numeric( substr(opp_param, 1, 1) )
      
      param_mode_applied_1 <- ifelse(param_mode_1 == 0, program[param_1], 
                                     ifelse(param_mode_1 == 1, param_1-1,
                                            NA))
      param_mode_applied_2 <- ifelse(param_mode_2 == 0, program[param_2], 
                                     ifelse(param_mode_2 == 1, param_2-1,
                                            NA))
      
      if ( !param_mode_3 %in% c(0, 1) ) {
        stop('Parameter 3 is incorrect.')
      }
      
      
      if ( oppcode == 1 ) {
        operation = sum
        execute_operation = operation(param_mode_applied_1, param_mode_applied_2)
        program[param_3] = execute_operation
        i = i+4
        
      } else if ( oppcode == 2 ) {
        operation = prod
        execute_operation = operation(param_mode_applied_1, param_mode_applied_2)
        program[param_3] = execute_operation
        i = i+4
        
      } else if ( oppcode == 3 ) {
        program[program[i+1] + 1] = input 
        i = i+2
        
      } else if ( oppcode == 4 ) {
        if ( param_mode_1 == 0 ) {
          output[i] = program[program[i+1] + 1]
        } else if ( param_mode_1 == 1 ) {
          output[i] = param_mode_applied_1
        }
        
        i = i+2
        
      } else if ( oppcode == 5 ) {
        if ( param_mode_applied_1 != 0 ) {
          i = param_mode_applied_2 + 1
        } else {
          i = i+3
        }
        
      } else if ( oppcode == 6 ) {
        if ( param_mode_applied_1 == 0 ) {
          i = param_mode_applied_2 + 1
        } else {
          i = i+3
        }
        
      } else if ( oppcode == 7 ) {
        if ( param_mode_applied_1 < param_mode_applied_2 ) {
          program[param_3] = 1
          i = i+4
        } else {
          program[param_3] = 0
          i = i+4
        }
        
      } else if ( oppcode == 8 ) {
        if ( param_mode_applied_1 == param_mode_applied_2 ) {
          program[param_3] = 1
          i = i+4
        } else {
          program[param_3] = 0
          i = i+4
        }
        
      } else if ( oppcode == 99 ) {
        return(output)
      }
    }
  }

## Test:
day_5_data <- 
  read.csv(
    "~/anala_lytics/AdventOfCode2019/advent_inputs_2019/day_5", 
    header = FALSE, 
    stringsAsFactors = FALSE
  ) %>% 
  unlist() %>% 
  as.numeric()

output_list <- intcodeDiagnosis(day_5_data)
output_list[[length(output_list)]] # 13547311


# Day 5.2 ----------------------------------------

## Test:
output_list <- intcodeDiagnosis(day_5_data, input = 5)
output_list[[length(output_list)]] # 236453


# Day 6.1 ----------------------------------------

numberOrbits <-
  function(orbit_map) {
    # Summary: Count the number of direct and indirect
    # orbits given an orbit map. 
    
    # Input: 
    #   orbit_map: orbit map in the form 
    #     of a planet A orbitting ) some
    #     sun B. 
    
    # Output: The number of direct and indirect orbits. 
    
    # Example: 
    # orbits_map_test <- c("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L")
    # numberOrbits(orbits_map_test) # 42
    orbit_map_planet <- gsub(".*)", "", orbit_map)
    orbit_map_sun    <- gsub(").*", "", orbit_map)
    last_planet      <- orbit_map_planet[!orbit_map_planet %in% orbit_map_sun]
    planets_orbits <- NULL
    
    for ( i in 1:length(last_planet) ) {
      last_planet_orbit <- orbit_map[grepl(paste0(')', last_planet[i]), orbit_map)]
      planets_orbits[i] <- last_planet_orbit
      
      while( !grepl("COM", planets_orbits[i]) ) {
        last_planet_sun <- gsub(").*", "", last_planet_orbit)
        next_planet_orbit <- orbit_map[grepl(paste0(')', last_planet_sun), orbit_map)]
        planets_orbits[i] <- paste(next_planet_orbit, planets_orbits[i], sep = ", ")
        last_planet_orbit <- next_planet_orbit
      }
      
    }
    
    deconstruct <- list()
    
    for ( i in 1:length(planets_orbits) ) {
      planets_orbits_split <- unlist(strsplit(planets_orbits[i], ', '))
      for (j in 1:length(planets_orbits_split) ) {
        deconstruct[length(deconstruct)+1] <- (paste(planets_orbits_split[1:(length(planets_orbits_split) - j)], collapse = ", "))
      }
    }
    
    length(unlist(strsplit(sort(unique( c(unlist(deconstruct), planets_orbits) )), ", ")))
  }

## Test:
day_6_data <- 
  read.table(
    "~/anala_lytics/AdventOfCode2019/advent_inputs_2019/day_6", 
    quote = "\"", 
    comment.char = "", 
    stringsAsFactors = FALSE
  ) %>% 
  rename(orbits = V1) %>% 
  unlist() %>% 
  as.character()

numberOrbits(day_6_data) # 194721


# Day 6.2 ----------------------------------------

orbitTransfers <-
  function(orbit_map) {
    # Summary: figure out how many orbital transfers 
    # you (YOU) need to take to get to Santa (SAN)
    
    # Input: 
    #   orbit_map: orbit map in the form 
    #     of a planet A orbitting ) some
    #     sun B. 
    
    # Output: Return the minimum number of orbital 
    # transfers required to move from the object YOU 
    # are orbiting to the object SAN is orbiting? 
    # (Between the objects they are orbiting - not between YOU and SAN.)
    
    # Example: 
    # orbit_map <- c("COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU","I)SAN")
    # orbitTransfers(orbit_map) # 4
    last_planet      <- c('SAN', 'YOU')
    planets_orbits   <- NULL
    
    for ( i in 1:length(last_planet) ) {
      last_planet_orbit <- orbit_map[grepl(paste0(')', last_planet[i]), orbit_map)]
      planets_orbits[i] <- last_planet_orbit
      
      while( !grepl("COM", planets_orbits[i]) ) {
        last_planet_sun <- gsub(").*", "", last_planet_orbit)
        next_planet_orbit <- orbit_map[grepl(paste0(')', last_planet_sun), orbit_map)]
        planets_orbits[i] <- paste(next_planet_orbit, planets_orbits[i], sep = ", ")
        last_planet_orbit <- next_planet_orbit
      }
    }
    
    planets_orbits_san <- unlist(strsplit(planets_orbits[1], ", "))
    planets_orbits_you <- unlist(strsplit(planets_orbits[2], ", "))
    
    length(planets_orbits_san[!planets_orbits_san %in% planets_orbits_you]) + 
      length(planets_orbits_you[!planets_orbits_you %in% planets_orbits_san]) - 
      2
  }

## Test: 
orbitTransfers(day_6_data) # 316


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
