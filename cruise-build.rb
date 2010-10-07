# This file is part of KSQuant2.

# Copyright (c) 2010, Kilian Sprotte. All rights reserved.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
def safe_system(command)
  puts command
  system command
  res = $?
  if not (res == 0) then
    puts "`" + command + "'"
    puts "returned " + res.to_s
    exit 1
  end
end

$counter = 0

def make(target)
  $counter += 1
  name = "#{$counter}-make-#{target}.log"
  safe_system "make #{target} >#{name} 2>&1 && touch #{name}"
  safe_system "mv #{name} #{ENV['CC_BUILD_ARTIFACTS']}/#{name}" if ENV['CC_BUILD_ARTIFACTS']
end

make "clean"
make "build"
make "doc"
safe_system "mv 'dist/doc/html/ksquant2/ksquant2' #{ENV['CC_BUILD_ARTIFACTS']}/doc" if ENV['CC_BUILD_ARTIFACTS']

make "test"
safe_system "ln -s hpc_index.html test-coverage/index.html"
safe_system "mv 'test-coverage' #{ENV['CC_BUILD_ARTIFACTS']}/test-coverage" if ENV['CC_BUILD_ARTIFACTS']


make "runmain"
safe_system "ln -s hpc_index.html main-coverage/index.html"
safe_system "mv 'main-coverage' #{ENV['CC_BUILD_ARTIFACTS']}/main-coverage" if ENV['CC_BUILD_ARTIFACTS']

make "shell-tests"

make "check"
