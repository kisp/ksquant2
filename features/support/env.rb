require 'aruba/cucumber'

Before do
  @aruba_timeout_seconds = 30
end

After do
  if File.exist?("tmp/aruba/ksquant2.tix")
    system "hpc combine --union ksquant2.tix tmp/aruba/ksquant2.tix >/tmp/my.tix"
    if $?.success?
      system "mv /tmp/my.tix ksquant2.tix"
    end
  end
end
