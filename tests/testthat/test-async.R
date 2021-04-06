if(FALSE)
{
  test_that("async", {
  ii <- function(n) make_delay(for_cps(arg_cps(i),
                                       arg_cps(1:n),
                                       `{_cps`(block_cps(),
                                               arg_cps(n))
                                       ))
  x <- ii(2)
  })
}
