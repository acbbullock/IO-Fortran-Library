program tests
  !---------------------------------------------------------------------------------------------------------------------
  !! Runs the test suite. See `test_support` for the command line, and CONTRIBUTING.md for how to add a case.
  !---------------------------------------------------------------------------------------------------------------------
  use test_support, only: test_suite
  use harness_cases, only: register_harness
  use api_cases, only: register_api
  use cast_cases, only: register_cast
  use str_cases, only: register_str
  use array_files_cases, only: register_array_files
  use delimited_cases, only: register_delimited
  use binary_cases, only: register_binary
  use format_cases, only: register_format
  use string_cases, only: register_string
  use order_cases, only: register_order
  use aprint_cases, only: register_aprint
  use read_line_cases, only: register_read_line
  use text_round_trip_cases, only: register_text_round_trip
  use file_round_trip_cases, only: register_file_round_trip
  implicit none (type, external)

  type(test_suite) :: suite

  call register_harness(suite)
  call register_api(suite)
  call register_cast(suite)
  call register_str(suite)
  call register_array_files(suite)
  call register_delimited(suite)
  call register_binary(suite)
  call register_format(suite)
  call register_string(suite)
  call register_order(suite)
  call register_aprint(suite)
  call register_read_line(suite)
  call register_text_round_trip(suite)
  call register_file_round_trip(suite)

  call suite%run("tests")
end program tests
