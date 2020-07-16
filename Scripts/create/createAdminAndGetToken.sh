#!/bin/bash
curl 'http://localhost:3000/signIn?password=password&login=admin&first_name=Admin&last_name=Sumiya&admin_pass=GlobalAdminPass'
result=$(curl 'http://localhost:3000/logIn?password=password&login=admin' | jq -r '.result'
)
echo $result > admin.token

echo ''
