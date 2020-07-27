#!/bin/bash
id=$(<category.id)
token=$(<admin.token)
if [[ "$1" == 'u' ]]
then
    token=$(<user.token)
fi
query="http://localhost:3000/deleteCategory?category_id=$id&token=$token"
curl -X DELETE $query

echo ''

