# Delete News

Deletes news.

**URL** : `/deleteNews`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__news_id__ | integer | News ID to be deleted

**Method** : `DELETE`

**Auth required** : Yes

**Permissions required** : Admin or news author

## Success Response

**Content:** [Response](../types/response.md) with empty `result` field.

* **Condition** : If everything is OK.  
**Code** : `200 OK`

## Error Responses

**Content:** None
* **Condition** : If token does not belong to the administrator or news author.  
**Code** : `403 BAD REQUEST`

* **Condition** : If required fields are missed or incorrect.  
**Code** : `400 BAD REQUEST`


**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If news with specified `news_id` does not exists.  
**Code** : `400  BAD REQUEST`



