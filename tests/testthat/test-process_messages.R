# tests/testthat/test_process_messages.R

test_that("process_messages handles single message", {
  msg <- "Hello"
  result <- process_messages(msg)
  expect_equal(result, list(list(role = "user", content = msg)))
})

test_that("process_messages handles named vector", {
  msgs <- c(system = "Welcome", user = "Hello")
  result <- process_messages(msgs)
  expect_equal(result, list(list(role = "system", content = "Welcome"),
                            list(role = "user", content = "Hello")))
})

test_that("process_messages handles invalid vector format", {
  msgs <- c(wrong = "Test")
  expect_error(process_messages(msgs), "Invalid format for 'messages' vector.")
})

test_that("process_messages handles correctly formatted list", {
  msgs <- list(list(role = "user", content = "Hello"))
  result <- process_messages(msgs)
  expect_equal(result, msgs)
})

test_that("process_messages handles invalid list format", {
  msgs <- list(list(role = "wrong", content = "Test"))
  expect_error(process_messages(msgs), "Message is neither a valid vector nor a valid list.")
})

test_that("process_messages handles list of messages", {
  msgs <- list(
    list(role = "system", content = "System message"),
    list(role = "user", content = "User message")
  )
  result <- process_messages(msgs)
  expect_equal(result, msgs)
})

test_that("process_messages handles invalid list format", {
  msgs <- list(
    list(wrong = "Invalid message"),
    list(role = "user", content = "User message")
  )
  expect_error(process_messages(msgs), "Message is neither a valid vector nor a valid list.")
})

# Edge case: Empty input
test_that("process_messages handles empty input", {
  expect_error(process_messages(NULL), "User messages are required.")
  expect_error(process_messages(character(0)), "User messages are required.")
})

# Edge case: Unusual characters
test_that("process_messages handles unusual characters", {
  unusual_msg <- "Hello 😊"
  result <- process_messages(unusual_msg)
  expect_equal(result, list(list(role = "user", content = unusual_msg)))
})
