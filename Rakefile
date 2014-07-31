require "rubygems"
require "tmpdir"

require "bundler/setup"
require "jekyll"
require 'rbconfig'

# Change your GitHub reponame eg. "kippt/jekyll-incorporated"
GITHUB_REPONAME = "upbit/upbit.github.io"


namespace :site do
  desc "Generate blog files"
  task :generate do
    Jekyll::Site.new(Jekyll.configuration({
      "source"      => ".",
      "destination" => "_site"
    })).process
  end


  desc "Generate and publish blog to github"
  task :publish => [:generate] do
    Dir.mktmpdir do |tmp|
      cp_r "_site/.", tmp
      Dir.chdir tmp
      system "git init"
      system "git add ."
      message = "Site updated at #{Time.now.utc}"
      system "git commit -m #{message.inspect}"
      system "git remote add origin git@github.com:#{GITHUB_REPONAME}.git"
      #system "git push origin master:refs/heads/gh-pages --force"
      system "git push"
    end
  end
end
