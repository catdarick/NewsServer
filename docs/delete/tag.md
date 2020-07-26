# Delete Tag

Deletes tag.

**URL** : `/deleteTag`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__tag_id__ | integer | Tag ID to be deleted


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

* **Condition** : If required fields are missed.  
**Code** : `404 BAD REQUEST`


**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If tag with specified `tag_id` does not exists.  
**Code** : `400  BAD REQUEST`



