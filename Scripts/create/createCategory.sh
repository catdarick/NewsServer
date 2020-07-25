#!/bin/bash
token=$(<admin.token)
if [[ "$1" == 'u' ]]
then
    token=$(<user.token)
fi

query="http://localhost:3000/createCategory?&token=$token&name=root"
if [[ "$1" == 'c' ]]
then
    id=$(<category.id)
    name="child_of_$id"
    query="http://localhost:3000/createCategory?&token=$token&name=$name&parent_id=$id"
fi
result=$(curl -X POST $query)
echo $result > tmp
id=$(jq -r '.result.id' tmp)
echo $result
echo $id > category.id


echo ''
