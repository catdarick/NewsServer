# Create Author

Grants author priveleges to any user.

**URL** : `/createAuthor`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__user_id__ | integer |User ID to be granted author privileges
description | string | Author's description

**Method** : `POST`

**Auth required** : Yes

**Permissions required** : Admin

## Success Response

**Condition** : If everything is OK.

**Code** : `201 CREATED`

**Content:** [Response](../types/response.md) with created author [id container](../types/idcont.md) in `result` field.



## Error Responses

**Content:** None
* **Condition** : If token does not belong to the administrator.
**Code** : `404 BAD REQUEST`

* **Condition** : If required fields are missed.
**Code** : `404 BAD REQUEST`


**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If user is already an author.  
**Code** : `400  BAD REQUEST`



