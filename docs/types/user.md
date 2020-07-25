# User

Data type that describing the user.

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__id__ | int | User's ID
__login__ | string | User's login
__first_name__ | string | User's first name
__last_name__ | string | User's last name
__picture__ | string | User's picture link
__creation_time__ | string | User's creation time
__is_admin__ | bool | Does the user have administrator privileges or not
picture |string | User's picture link

**Note:**
`creation_time` contains ISO 8601 formatted time as `YYYY-MM-DDThh:mm:ss`.

**Data examples**:

```json
{
    "first_name":"Ivan",
    "creation_time":"2020-07-16T04:36:37",
    "last_name":"Ivanon",
    "is_admin":false,
    "id":5,
    "login":"user1"
}
```
