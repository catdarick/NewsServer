#!/bin/bash
token=$(<author.token)
if [[ "$1" == 'u' ]]
then
    token=$(<user.token)
fi
categoryId=$(<category.id)
query="http://localhost:3000/createDraft?&token=$token&title=title&content=content&category_id=$categoryId"
result=$(curl -X POST $query)
echo $result > tmp
id=$(jq -r '.result.id' tmp)
echo $result
echo $id > draft.id


echo ''
