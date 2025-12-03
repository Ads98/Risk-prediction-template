reduce_mp <- function(mp_data, M = 5){
  return(Reduce(`+`, mp_data) / M)
}

col_shrinkage <- function(mods,rows_with_na){
  #get avergae  across each bs sample 
  # Calculate the mean across the rows of 'row_means'
  org_mods <- mods[[1]]
  bs_mods <- mods[[2]]
  

  male_q_cbl <- remove_rows(org_mods[[1]],rows_with_na)
  male_q_cbl <-colMeans(male_q_cbl)
 
  male_ars_cbl <- remove_rows(org_mods[[2]],rows_with_na)
  male_ars_cbl <-colMeans(male_ars_cbl)
  
  print("female q")
  print(dim(org_mods[[3]][[1]]))
  #female_q_cbl <- sapply(org_mods[[3]], function(df) df[-unique(rows_with_na),'cbl'])
  female_q_cbl <- remove_rows(org_mods[[3]],rows_with_na)
  female_q_cbl <-rowMeans(female_q_cbl)
  
  print("female ars")
  print(dim(org_mods[[4]][[1]]))
  #female_ars_cbl <- sapply(org_mods[[4]], function(df) df[-unique(rows_with_na),'cbl'])
  female_ars_cbl <- remove_rows(org_mods[[4]],rows_with_na)
  female_ars_cbl <-rowMeans(female_ars_cbl)
  
  print('bs mods')
  print("male q")
  print(dim(bs_mods[[1]][[1]]))
  #male_q_bs_cbl <- sapply(bs_mods[[1]], function(df) df[-unique(rows_with_na),'cbl'])
  male_q_bs_cbl <- remove_rows(bs_mods[[1]],rows_with_na)
  male_q_bs_cbl <-rowMeans(male_q_bs_cbl)
  
  print("male ars")
  print(dim(bs_mods[[2]][[1]]))
  #male_ars_bs_cbl <- sapply(bs_mods[[2]], function(df) df[-unique(rows_with_na),'cbl'])
  male_ars_bs_cbl <- remove_rows(bs_mods[[2]],rows_with_na)
  male_ars_bs_cbl <-rowMeans(male_ars_bs_cbl)
  
  
  print("female ars")
  print(dim(bs_mods[[3]][[1]]))
  #female_q_bs_cbl <- sapply(bs_mods[[3]], function(df) df[-unique(rows_with_na),'cbl'])
  female_q_bs_cbl <- remove_rows(bs_mods[[3]],rows_with_na)
  female_q_bs_cbl <-rowMeans(female_q_bs_cbl)
  
  #female_ars_bs_cbl <- sapply(bs_mods[[4]], function(df) df[-unique(rows_with_na),'cbl'])
  print("female ars")
  print(dim(bs_mods[[4]][[1]]))
  female_ars_bs_cbl <- remove_rows(bs_mods[[4]],rows_with_na)
  female_ars_bs_cbl <-rowMeans(female_ars_bs_cbl)
  
  print("shrink lp")
  male_q_us = 1 - mean(male_q_bs_cbl - male_q_cbl)
  male_ars_us = 1 - mean(male_ars_bs_cbl - male_ars_cbl)
  female_q_us = 1 - mean(female_q_bs_cbl - female_q_cbl)
  female_ars_us = 1 - mean(female_ars_bs_cbl - female_ars_cbl)
  
  return(list(male_q_us,male_ars_us,female_q_us,female_ars_us))
}