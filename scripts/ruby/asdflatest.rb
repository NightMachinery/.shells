#!/usr/bin/env ruby
def latest(language)
  `asdf plugin-add #{language} 2> /dev/null`
  versions = `asdf list-all #{language} 2> /dev/null`.split("\n")
  stock_versions = versions.select { |v| v.count('a-zA-Z').zero? }
  latest_version = stock_versions.max_by { |v| Gem::Version.new(v) }
  `asdf install #{language} #{latest_version}`
  `asdf global #{language} #{latest_version}`
end
latest(ARGV[0])
