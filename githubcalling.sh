

#!/bin/sh
#cd "$Home/jmarti73/project57"

alias proj="cd /home/jmarti73/project571"

git init

git remote add upstream https://github.com/rbuezotordesillas/project571.git

git checkout virtualmachine


echo TODO

sudo git add .

git commit -m "antoher day"

#spawn git push origin virtualmachine
#expect "Username for 'https://github.com':"
#send "jlmartinezmarin"
#expect "Password for 'https://jlmartinezmarin@github.com':"
#send ""
#interact

HOME=/home/jmarti73 git push origin virtualmachine

echo DONE

#git push origin virtualmachine



