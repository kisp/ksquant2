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


safe_system "git status | grep 'nothing to commit (working directory clean)'"

puts "version:"
version = readline.chomp
puts version.inspect

# safe_system "git tag -a #{version}"
safe_system "make clean"
safe_system "make"
safe_system "sh utils/link.sh"
safe_system "otool -L Main > otool.out"
safe_system "! grep libgmp <otool.out"
safe_system "rm otool.out"
safe_system "mv Main kernel"
safe_system "git archive --prefix=justtemp/ --format tar -o /tmp/justtemp.tar HEAD"
safe_system "cd /tmp; rm -rf justtemp ; tar xf justtemp.tar"
safe_system "mv kernel /tmp/justtemp"
safe_system "cd /tmp; mv  justtemp ksquant2-#{version}"
safe_system "cd /tmp; tar cfz ksquant2-#{version}.tgz ksquant2-#{version}"
