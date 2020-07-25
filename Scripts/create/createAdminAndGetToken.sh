#!/bin/bash
curl -X POST 'http://localhost:3000/signIn?password=password&login=admin&first_name=Admin&last_name=Sumiya&admin_pass=GlobalAdminPass'
result=$(curl -X POST 'http://localhost:3000/login?password=password&login=admin' | jq -r '.result'
)
echo $result > admin.token

echo ''
