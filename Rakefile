require 'rake-jekyll'

# This task builds the Jekyll site and deploys it to a remote Git repository.
# It's preconfigured to be used with GitHub and Travis CI.
# See http://github.com/jirutka/rake-jekyll for more options.
Rake::Jekyll::GitDeployTask.new(:deploy) do |t|
  # Deploy the built site into remote branch named 'gh-pages'. It will be
  # automatically created if not exist yet.
  t.deploy_branch = 'master'
end
