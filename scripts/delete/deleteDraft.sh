#!/bin/bash
id=$(<draft.id)
token=$(<author.token)
if [[ "$1" == 'u' ]]
then
    token=$(<user.token)
fi
if [[ "$1" == 'a' ]]
then
    token=$(<admin.token)
fi
query="http://localhost:3000/deleteDraft?draft_id=$id&token=$token"
curl -X DELETE $query


echo ''
