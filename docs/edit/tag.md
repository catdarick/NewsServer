# Edit Tag

Edites [tag](../types/tag.md) fields.

**URL** : `/editTag`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__tag_id__ | integer | Tag ID to be changed
name | string | New name

**Note:**
Fields not specified will not be affected.

**Method** : `PUT`

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

* **Condition** : If tag with specified `tag_id` does not exists.  
**Code** : `400  BAD REQUEST`

* **Condition** : If tag with specified `name` already exists.  
**Code** : `400  BAD REQUEST`



