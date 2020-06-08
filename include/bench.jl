# Call a function n times and print the average elapsed time.
function benchElapsedTime(n:: Int64, benchfun:: Function)
  acctime = 0.0
  for i = 1:n
    start = time_ns()
    benchfun()
    stop = time_ns()
    acctime += (stop - start) / 1e9
  end
  println("ELAPSEDTIME: " * string(acctime/n))
end
