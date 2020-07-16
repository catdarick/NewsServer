#!/bin/bash
token=$(<user.token)
newsId=$(<draft.id)
query="http://localhost:3000/postComment?&token=$token&news_id=$newsId&content=content"
result=$(curl $query)
echo $result > tmp
echo $result
id=$(jq -r '.result.id' tmp)
echo $id > comment.id
 
 
 
echo ''
