# Comment

Data type that describing the comment.

Field | Type |Description
---------- | ------------- | ---------
__id__ | integer | Comment ID
__user__ | [User](user.md) | Comment author 
__content__ | string | Comment content
__creation_time__ | string | Comment creation time


**Note:**
`creation_time` contains ISO 8601 formatted time as `YYYY-MM-DDThh:mm:ss`.

**Data example:**

```json
{
    "creation_time":"2020-07-17T21:20:34",
    "content":"It's horrible...",
    "user":{
        "first_name":"Som",
        "creation_time":"2020-07-16T04:36:43",
        "last_name":"Khatri'",
        "is_admin":false,
        "id":7,
        "login":"user3"
    },
    "id":5
}
```
