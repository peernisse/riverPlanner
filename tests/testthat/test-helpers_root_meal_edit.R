# TESTS FOR helpers_root_meal_edit

# DATA ----
session <- list(ns = NS('test'))
tRvObj <- list(
    'rootEditIngs' = data.frame(
        INGREDIENT_UNIQUE_ID = c('noTest', 'test_1', 'noTest2'),
        INGREDIENT = c('ingName fal', 'ingName succeed', 'ingName fail')
    )
)

# rootEditMealDelIng() ----

test_that("rootEditMealDelIng returns null on wrong ID", {
    out <- rootEditMealDelIng(session, id = 'not_test_1', data = tRvObj)
    expect_equal(NULL, out)
})
