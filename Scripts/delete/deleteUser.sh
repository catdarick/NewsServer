#!/bin/bash
id=$(<user.id)
token=$(<admin.token)
if [[ "$1" == 'u' ]]
then
    token=$(<user.token)
fi
query="http://localhost:3000/deleteUser?user_id=$id&token=$token"
curl -X DELETE $query


echo ''
