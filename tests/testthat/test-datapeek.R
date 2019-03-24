context('utility functions')

library(data.table)
library(mltools)

load('../../data/iris.rda')

test_that("data frame with correct number of features is returned", {
    res <- encode_and_bind(iris, 'Species')
    expect_equal(dim(res)[2], 8)
})

test_that("data frame with correct number of features is returned", {
    res <- remove_features(iris, 'Species')
    expect_equal(dim(res)[2], 4)
})

test_that("newly created columns return correct sum", {
    res <- apply_function_to_column(iris, "Sepal.Width, Sepal.Length", "new_col1, new_col2", "x*5") 
    expect_equal(sum(res$new_col1), 2293)
    expect_equal(sum(res$new_col2), 4382.5)
})

test_that("closest matching string returned", {
    res <- get_closest_string(c("hey there", "we are here", "howdy doody"), "doody") 
    expect_true(identical(res, 'howdy doody'))
})


