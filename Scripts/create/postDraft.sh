#!/bin/bash
token=$(<author.token)
if [[ "$1" == 'u' ]]
then
    token=$(<user.token)
fi
id=$(<draft.id)
categoryId=$(<category.id)
query="http://localhost:3000/postDraft?&token=$token&draft_id=$id"
result=$(curl $query)

echo $result


echo ''
