# Get Comments

Depending on the news ID specified, tou will receive JSON-serialized [Response](../types/response.md) with [Comment](../types/comment.md) objects as a result.

**URL** : `/getComments`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__news_id__ | integer | To get comments for specified news
limit | integer | Limits the number of objects to be retrieved. Defaults to `50`.
offset | integer | Number of entities to skip. Used for pagination. Defaults to `0`.

**Note:**  
`limit` must be from interval `(1-200)`

**Method** : `GET`

**Auth required** : No

**Permissions required** : None

## Success Response

**Content:** [Response](../types/response.md) with [[Comment](../types/comment.md)] in `result` field.

* **Condition** : If everything is OK.  
**Code** : `200 OK`

## Error Responses

**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If parameters are incorrect.  
**Code** : `400 BAD REQUEST`


