# Publish draft

Creates news by publishing draft

**URL** : `/publishDraft`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__draft_id__ | integer | News ID to which the comment relates

**Note:**
The author can only publish his own draft.
News will have the same ID as a draft.

**Method** : `PUT`

**Auth required** : Yes

**Permissions required** : Author

## Success Response

**Content:** [Response](../types/response.md) with empty `result` field.

* **Condition** : If everything is OK.  
**Code** : `200 OK`



## Error Responses

**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If required fields are missed or incorrect.  
**Code** : `400 BAD REQUEST`

* **Condition** : If token does not belong to draft owner author.  
**Code** : `403  UNAUTHORIZED`

* **Condition** : If draft with specified ID doesn't exists.  
**Code** : `400 BAD REQUEST`


