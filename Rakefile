files = FileList["src/*.hs"]
compile_cmd = "ghc -outputdir bin -isrc -Wall #{ENV['GHC_OPTS']}".strip

files.each do |f|
  o_file = f.pathmap("%{^src/bin}X.o")
  file o_file => f do
    sh "#{compile_cmd} #{f}"
  end
end

file "bin/Main.o" => files.dup.exclude(/Main\.hs/).pathmap("%{^src/bin}X.o")

desc "Compile srcs"
task :compile => files.pathmap("%{^src/bin}X.o")
task :default => :compile

namespace :clean do
  desc "Clean the compiled objects"
  task :src do
    sh "rm -rf ./bin #{files.pathmap("%X")}"
  end

  # desc "Remove the databases in db/"
  # task :db do
  #   sh "rm -rf ./db/db-*"
  # end

  # desc "Clean both compiled objects and db"
  # task :all => [:src, :db]
end
desc "Clean the compiled objects, same as clean:src"
task :clean => "clean:src"
