#!/bin/bash
id=$(<author.id)
token=$(<admin.token)
if [[ "$1" == 'u' ]]
then
    token=$(<user.token)
fi
query="http://localhost:3000/deleteAuthor?author_id=$id&token=$token"
curl $query


echo ''
