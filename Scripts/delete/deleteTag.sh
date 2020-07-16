#!/bin/bash
id=$(<tag.id)
token=$(<admin.token)
if [[ "$1" == 'u' ]]
then
    token=$(<user.token)
fi
query="http://localhost:3000/deleteTag?tag_id=$id&token=$token"
curl $query


echo ''
