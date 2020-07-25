#!/bin/bash
token=$(<admin.token)
if [[ "$1" == 'u' ]]
then
    token=$(<user.token)
fi
name="tag_$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 5 | head -n 1)"
query="http://localhost:3000/createTag?&token=$token&name=$name"
result=$(curl -X POST $query)
echo $result > tmp
id=$(jq -r '.result.id' tmp)
echo $result
echo $id > tag.id


echo ''
