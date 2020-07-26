# Delete Draft

Deletes draft.

**URL** : `/deleteDraft`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__draft_id__ | integer | Draft ID to be deleted

**Note:**
The author can only delete his own draft.

**Method** : `DELETE`

**Auth required** : Yes

**Permissions required** : News author

## Success Response

**Content:** [Response](../types/response.md) with empty `result` field.

* **Condition** : If everything is OK.  
**Code** : `200 OK`

## Error Responses

**Content:** None
* **Condition** : If token does not belong to the news author.  
**Code** : `403 BAD REQUEST`

* **Condition** : If required fields are missed or incorrect.  
**Code** : `400 BAD REQUEST`


**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If draft with specified `draft_id` does not exists.  
**Code** : `400  BAD REQUEST`


