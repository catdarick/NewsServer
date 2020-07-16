#!/bin/bash
token=$(<admin.token)
if [[ "$1" == 'u' ]]
then
    token=$(<user.token)
fi
query="http://localhost:3000/createCategory?&token=$token&name=someName"
result=$(curl $query)
echo $result > tmp
id=$(jq -r '.result.id' tmp)
echo $result
echo $id > category.id


echo ''
