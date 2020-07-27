#!/bin/bash
curl -X POST 'http://localhost:3000/createAccount?password=password&login=admin&first_name=Admin&last_name=Sumiya'
result=$(curl -X PUT 'http://localhost:3000/getToken?password=password&login=admin' | jq -r '.result'
)
echo $result > admin.token
echo $result 
echo ''
