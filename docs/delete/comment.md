# Delete Comment

Deletes comment.

**URL** : `/deleteComment`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__comment_id__ | integer | Comment ID to be deleted

**Note:**
User can only delete his own comment.

**Method** : `DELETE`

**Auth required** : Yes

**Permissions required** : Admin or comment creator 

## Success Response

**Content:** [Response](../types/response.md) with empty `result` field.

* **Condition** : If everything is OK.  
**Code** : `200 OK`

## Error Responses

**Content:** None
* **Condition** : If token does not belong to admin or comment creator.  
**Code** : `403 BAD REQUEST`

* **Condition** : If required fields are missed or incorrect.  
**Code** : `400 BAD REQUEST`


**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If comment with specified `comment_id` does not exists.  
**Code** : `400  BAD REQUEST`


