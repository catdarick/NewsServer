# Get Categories

Depending of which filters you choose, you will receive JSON-serialized [Response](../types/response.md) with [Category](../types/category.md) objects as a result.

**URL** : `/getCategories`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
category_id | integer | To get category with specified ID 
parent_id | integer | To get categories with specified parent category ID. Used to get child categories.
name | string | To get categories with specified name 
limit | integer | Limits the number of objects to be retrieved. Defaults to `50`.
offset | integer | Number of entities to skip. Used for pagination. Defaults to `0`.

**Note:**  
`limit` must be from interval `(1-200)`

**Method** : `GET`

**Auth required** : No

**Permissions required** : None

## Success Response

**Content:** [Response](../types/response.md) with [[Category](../types/category.md)] in `result` field.

* **Condition** : If everything is OK.  
**Code** : `200 OK`

## Error Responses

**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If parameters are incorrect.  
**Code** : `400 BAD REQUEST`


