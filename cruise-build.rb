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

def make(target)
  safe_system "make #{target} >make-#{target}.log 2>&1"
  safe_system "mv make-#{target}.log #{ENV['CC_BUILD_ARTIFACTS']}/make-#{target}.log"
end

make "clean"
make "build"
make "doc"
safe_system "mv 'dist/doc/html/haskell-quant/haskell-quant' #{ENV['CC_BUILD_ARTIFACTS']}/doc"
