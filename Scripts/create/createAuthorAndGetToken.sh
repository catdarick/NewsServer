#!/bin/bash
name="author_$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 5 | head -n 1)"
query="http://localhost:3000/createAccount?password=password&login=$name&first_name=Author&last_name=Sumiya'"
result1=$(curl -X POST $query)
echo $result1 > tmp
echo $result1
userId=$(jq -r '.result.id' tmp)

adminToken=$(<admin.token)
query2="http://localhost:3000/createAuthor?user_id=$userId&token=$adminToken"
result2=$(curl -X POST $query2)
echo $result2 > tmp
echo $result2
authorId=$(jq -r '.result.id' tmp)
query3="http://localhost:3000/login?password=password&login=$name"
result3=$(curl -X POST $query3)
echo $result3 > tmp
echo $result3
token=$(jq -r '.result' tmp)

echo $token > author.token
echo $authorId > author.id

echo ''
