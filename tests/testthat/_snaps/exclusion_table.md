# Exclusion criteria works

    Code
      exclusion_table(data = mtcars, exclusion_criteria = c(
        "disp <= 70 | disp >= 300", "as.character(gear) == '4'"), labels_exclusion = c(
        "First exclusion", "Second exclusion"))
    Output
      
      =============================================
      Excluded the following observations:
      =============================================
      Exclusions based on EXCLUSION criteria
      
               exclusion n_prior n_post n_excluded
      1  First exclusion      32     21         11
      2 Second exclusion      21      9         12
      3            TOTAL      32      9         23
      
      =============================================
      

# Inclusion criteria works

    Code
      exclusion_table(data = mtcars, inclusion_criteria = c("disp >= 300",
        "as.character(gear) == '4'"), labels_inclusion = c("First exclusion",
        "Second exclusion"))
    Output
      
      =============================================
      Excluded the following observations:
      =============================================
      Exclusions based on INCLUSION criteria
      
               inclusion n_prior n_post n_excluded
      1  First exclusion      32     11         21
      2 Second exclusion      11      0         11
      3            TOTAL      32      0         32
      
      =============================================
      

# Exclusion with object

    Code
      my_selection <- c(8, 6)
      exclusion_table(data = mtcars, exclusion_criteria = c(
        "cyl %in% obj$my_selection"), labels_exclusion = c("First exclusion"), obj = list(
        my_selection = my_selection))
    Output
      
      ============================================
      Excluded the following observations:
      ============================================
      Exclusions based on EXCLUSION criteria
      
              exclusion n_prior n_post n_excluded
      1 First exclusion      32     11         21
      2           TOTAL      32     11         21
      
      ============================================
      

