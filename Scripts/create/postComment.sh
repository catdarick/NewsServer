#!/bin/bash
token=$(<user.token)
newsId=$(<draft.id)
query="http://localhost:3000/createComment?&token=$token&news_id=$newsId&content=content"
result=$(curl -X POST $query)
echo $result > tmp
echo $result
id=$(jq -r '.result.id' tmp)
echo $id > comment.id
 
 
 
echo ''
