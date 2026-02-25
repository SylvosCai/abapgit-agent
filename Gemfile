# frozen_string_literal: true

source "https://rubygems.org"

# Jekyll
gem "jekyll", "~> 4.3"

# Theme
gem "just-the-docs", "~> 0.10"

# Plugins
group :jekyll_plugins do
  gem "jekyll-seo-tag", "~> 2.8"
  gem "jekyll-relative-links", "~> 0.7"
  gem "jekyll-optional-front-matter", "~> 0.3"
  gem "jekyll-readme-index", "~> 0.3"
  gem "jekyll-sitemap", "~> 1.4"
  gem "jekyll-feed", "~> 0.17"
end

# Windows and JRuby does not include zoneinfo files, so bundle the tzinfo-data gem
# and associated library.
platforms :mingw, :x64_mingw, :mswin, :jruby do
  gem "tzinfo", ">= 1", "< 3"
  gem "tzinfo-data"
end

# Performance-booster for watching directories on Windows
gem "wdm", "~> 0.1", :platforms => [:mingw, :x64_mingw, :mswin]

# Lock `http_parser.rb` gem to `v0.6.x` on JRuby builds since newer versions of the gem
# do not have a Java counterpart.
gem "http_parser.rb", "~> 0.6.0", :platforms => [:jruby]

# webrick is not included with Ruby 3.0+
gem "webrick", "~> 1.8"
