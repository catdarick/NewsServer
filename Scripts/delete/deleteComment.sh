#!/bin/bash
id=$(<comment.id)
token=$(<user.token)
if [[ "$1" == 'a' ]]
then
    token=$(<admin.token)
fi
query="http://localhost:3000/deleteComment?comment_id=$id&token=$token"
curl $query

echo ''



