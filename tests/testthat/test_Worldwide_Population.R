
context("Worldwide_Population")

# test_that("This application only supports these countries: {'Sweden','Italy','Greece','Turkey'}",{
#    expect_error(worldwide<-Worldwide_Pollution$new(list("Spain","Turkey")))
#    expect_error(worldwide<-Worldwide_Pollution$new(list("Brazil","Sweden")))
#    
#  })

#  test_that("This is correct input",{
#     expect_true(worldwide<-Worldwide_Pollution$new(list("Greece")))
#     expect_true(worldwide<-Worldwide_Pollution$new(list("Greece","Sweden")))
#     expect_true(worldwide<-Worldwide_Pollution$new(list("Greece","Sweden","Turkey","Italy")))
# 
# })

# test_that("Parameter should be a  character list",{
#   expect_error(worldwide<-class(a$get_facets_all_responses(c("Turkey"))))
#   expect_error(worldwide<-class(a$get_facets_all_responses(c(5))))
# })

test_that("select correct data",{
   worldwide<-Worldwide_Pollution$new(list("Sweden"))
   expect_true(length(worldwide$get_facets_all_responses(facet_vector = list("value_pm5")))==582)
   })