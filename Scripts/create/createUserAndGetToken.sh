#!/bin/bash
name="user_$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 5 | head -n 1)"
query="http://localhost:3000/signIn?password=password&login=$name&first_name=Abdul&last_name=Sumiya'"
result1=$(curl -X POST $query)
echo $result1 > tmp
echo $result1
id=$(jq -r '.result.id' tmp)

query2="http://localhost:3000/logIn?password=password&login=$name"
result2=$(curl -X POST $query2)
echo $result2 > tmp
echo $result2
token=$(jq -r '.result' tmp)

echo $token > user.token
echo $id > user.id


echo ''
