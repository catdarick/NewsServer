#!/bin/bash
token=$(<author.token)
if [[ "$1" == 'u' ]]
then
    token=$(<user.token)
fi
id=$(<draft.id)
categoryId=$(<category.id)
query="http://localhost:3000/publishDraft?&token=$token&draft_id=$id"
result=$(curl -X PUT $query)

echo $result


echo ''
