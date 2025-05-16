

test_that("msg_queue", {
  
  x <- expect_silent(msg_queue(max_count = 2, cleanup = TRUE))
  y <- expect_no_error(msg_queue(assert = 'create', max_nchar = 1, file = tempfile()))
  z <- expect_silent(msg_queue(name = y$name, assert = 'exists'))
  
  expect_error(msg_queue(name = uid(),  file = tempfile()))
  expect_error(msg_queue(name = uid(),  assert = 'exists'))
  expect_error(msg_queue(name = x$name, assert = 'create'))
  
  expect_identical(x$max_count(), 2L)
  expect_identical(y$max_nchar(), 1L)
  expect_error(y$send('abc'))
  
  expect_true(x$send('abc', timeout_ms = Inf, priority = 0))
  expect_true(x$send('def', timeout_ms = 100, priority = 1))
  expect_false(x$send('ghi', timeout_ms = 0))
  expect_identical(x$count(), 2L)
  
  expect_identical(x$receive(timeout_ms = Inf),       'def')
  expect_identical(with(x, expr = ., timeout_ms = 0), 'abc')
  expect_null(     with(x, expr = ., timeout_ms = 0))
  expect_null(     with(x, expr = ., timeout_ms = 10))
  
  expect_false(exists(x = '.', inherits = FALSE))
  assign(x = '.', TRUE)
  expect_true(x$send('abc'))
  expect_identical(with(x, expr = invisible(.), timeout_ms = 10), 'abc')
  expect_true(get(x = '.', inherits = FALSE))
  
  expect_true(x$remove())
  expect_true(y$remove())
  expect_false(z$remove())
  
  
  skip_on_cran()
  skip_on_covr()
  
  mq <- expect_silent(msg_queue(max_count = 1))
  f <- function (nm) {
    q <- interprocess::msg_queue(name = nm)
    return (q$receive(timeout_ms = 0))
  }
  expect_true(mq$send('abc'))
  expect_identical(callr::r(f, list(nm = mq$name)), 'abc')
  expect_null(     callr::r(f, list(nm = mq$name)))
  
  expect_true(mq$remove())
  
  
  # cleanup works
  nm <- callr::r(function () interprocess::msg_queue(cleanup = TRUE)$name)
  mq <- expect_silent(interprocess::msg_queue(name = nm, assert = 'create'))
  expect_true(mq$remove())
  
  # persistence works
  nm <- callr::r(function () interprocess::msg_queue(cleanup = FALSE)$name)
  mq <- expect_silent(interprocess::msg_queue(name = nm, assert = 'exists'))
  expect_true(mq$remove())
  
})

