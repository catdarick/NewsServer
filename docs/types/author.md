# Author

Data type that describing the author.

Field | Type |Description
---------- | ------------- | ---------
__id__ | integer | Author's ID
__user__ | [User](user.md) | Author's user profile
description |string | Author's description



**Data example:**

```json
{
    "user":{
        "first_name":"Ivan",
        "creation_time":"2020-07-16T04:34:41",
        "last_name":"Ivanov",
        "is_admin":false,
        "id":1,
        "login":"user1"
    },
    "id":2,
    "description":"The best author in the world!"
}
```
