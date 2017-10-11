context("ridgereg")

data("iris")

Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)
test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda = barabra))
  expect_error(ridgereg_mod <- lridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis, lambda = 0))
})

test_that("class is correct", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
  
  expect_true(class(ridgereg_mod)[1] == "ridgereg")
})

test_that("print() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
  
  expect_output(ridgereg_mod$print(),"ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris, lambda = 1\\)")
  expect_output(ridgereg_mod$print()," \\(Intercept\\)  Sepal\\.Width  Sepal\\.Length")
})

test_that("coef() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
  ridge_coef <- round(as.numeric(ridgereg_mod$coef()),2)
  
  lm.ridge_mod <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
  lm_coef <- round(as.numeric(lm.ridge_mod$coef),2)
  
  is_equivalent_to(lm_coef,ridge_coef)
})