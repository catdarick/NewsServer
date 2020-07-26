# Get Users

Depending of which filters you choose, you will receive JSON-serialized [Response](../types/response.md) with [User](../types/user.md) objects as a result.

**URL** : `/getUsers`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
user_id | integer | To get user with specified ID
login | string | To get user with specified login 
first_name | string | To get users with specified first name 
last_name | string | To get users with specified last name
limit | integer | Limits the number of objects to be retrieved. Defaults to `50`.
offset | integer | Number of entities to skip. Used for pagination. Defaults to `0`.

**Note:**
`limit` must be from interval `(1-200)`

**Method** : `GET`

**Auth required** : No

**Permissions required** : None

## Success Response

**Content:** [Response](../types/response.md) with [[User](../types/user.md)] in `result` field.

* **Condition** : If everything is OK.  
**Code** : `200 OK`

## Error Responses

**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If parameters are incorrect.  
**Code** : `400 BAD REQUEST`


