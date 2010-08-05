def safe_system(command)
  puts "$ " + command
  system command
  res = $?
  if not (res == 0) then
    puts "`" + command + "'"
    puts "returned " + res.to_s
    exit 1
  end
end

safe_system "make clean"
safe_system "make build"

safe_system "make doc"
safe_system "mv 'dist/doc/html/haskell-quant/haskell-quant' #{ENV['CC_BUILD_ARTIFACTS']}/doc"
