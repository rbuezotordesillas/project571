#!/bin/sh
#cd "$Home/jmarti73/project57"

alias proj="cd /home/jmarti73/project571"

git init

git remote add upstream https://github.com/rbuezotordesillas/project571.git

git checkout virtualmachine

sudo git add .

echo Hello world

git commit -m "antoher day"

#spawn git push origin virtualmachine
#expect ""
#send "jlmartinezmarin"
#expect
#interact

git push origin virtualmachine



