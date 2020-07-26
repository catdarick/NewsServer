# Get Authors

Depending of which filters you choose, you will receive JSON-serialized [Response](../types/response.md) with [Author](../types/author.md) objects as a result.

**URL** : `/getAuthors`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
author_id | integer | To get authors with specified author ID
user_id | integer | To get authors with specified user ID
login | string | To get authors with specified user login
first_name | string | To get authors with specified user first name 
last_name | string | To get authors with specified user last name
limit | integer | Limits the number of objects to be retrieved. Defaults to `50`.
offset | integer | Number of entities to skip. Used for pagination. Defaults to `0`.

**Note:**
`limit` must be from interval `(1-200)`

**Method** : `GET`

**Auth required** : No

**Permissions required** : None

## Success Response

**Content:** [Response](../types/response.md) with [[Author](../types/author.md)] in `result` field.

* **Condition** : If everything is OK.  
**Code** : `200 OK`

## Error Responses

**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If parameters are incorrect.  
**Code** : `400 BAD REQUEST`


