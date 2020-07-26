# Delete User

Deletes user.

**URL** : `/deleteUser`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__user_id__ | integer | User ID to be deleted

**Note:**
All child categories will also be deleted!

**Method** : `DELETE`

**Auth required** : Yes

**Permissions required** : Admin

## Success Response

**Content:** [Response](../types/response.md) with empty `result` field.

* **Condition** : If everything is OK.  
**Code** : `200 OK`

## Error Responses

**Content:** None
* **Condition** : If token does not belong to the administrator.  
**Code** : `404 BAD REQUEST`

* **Condition** : If required fields are missed or incorrect.  
**Code** : `404 BAD REQUEST`


**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If user with specified `user_id` does not exists.  
**Code** : `400  BAD REQUEST`



