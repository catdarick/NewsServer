# Edit Author

Edites [author's](../types/author.md) fields.

**URL** : `/editAuthor`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__author_id__ | integer |Author ID to be changed
description | string | New description

**Note:**
Fields not specified will not be affected in draft.

**Method** : `PUT`

**Auth required** : Yes

**Permissions required** : Admin

## Success Response

**Condition** : If everything is OK.

**Code** : `200 OK`

**Content:** [Response](../types/response.md) with empty `result` field.


## Error Responses

**Content:** None
* **Condition** : If token does not belong to the administrator.  
**Code** : `404 BAD REQUEST`

* **Condition** : If required fields are missed.  
**Code** : `404 BAD REQUEST`


**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If author with specified ID does not exists.  
**Code** : `400  BAD REQUEST`



