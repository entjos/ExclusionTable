mtcars$id <- 1:nrow(mtcars)

test_that("Exclusion criteria works", {
  expect_snapshot({
    exclusion_table(
      data = mtcars,
      exclusion_criteria = c("disp <= 70 | disp >= 300",
                             "as.character(gear) == '4'"),
      labels_exclusion   = c("First exclusion",
                             "Second exclusion")
    )
  })
})

test_that("Inclusion criteria works", {
  expect_snapshot({

    exclusion_table(
      data = mtcars,
      inclusion_criteria = c("disp >= 300",
                             "as.character(gear) == '4'"),
      labels_inclusion   = c("First exclusion",
                             "Second exclusion")
    )
  })
})

test_that("Exclusion with object", {
  expect_snapshot({

    my_selection <- c(8, 6)

    exclusion_table(
      data = mtcars,
      exclusion_criteria = c("cyl %in% obj$my_selection"),
      labels_exclusion   = c("First exclusion"),
      obj = list(my_selection = my_selection)
    )
  })
})

test_that("Storing of ID vars", {
  expect_equal({
    temp <- exclusion_table(
      data = mtcars,
      inclusion_criteria = c("disp >= 300",
                             "as.character(gear) == '4'"),
      labels_inclusion   = c("First exclusion",
                             "Second exclusion")
    )
    temp$table_in$n_excluded[[1]]
  }, {
    temp <- exclusion_table(
      data = mtcars,
      inclusion_criteria = c("disp >= 300",
                             "as.character(gear) == '4'"),
      labels_inclusion   = c("First exclusion",
                             "Second exclusion"),
      id = "id"
    )
    length(temp$table_in$ids[[1]])
  })
})
