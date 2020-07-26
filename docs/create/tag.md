# Create Tag

Creates new tag.

**URL** : `/createTag`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__name__ | string | Name of the new category

**Note:**
Name must be unique.

**Method** : `POST`

**Auth required** : Yes

**Permissions required** : Admin

## Success Response

**Content:** [Response](../types/response.md) with created tag [id container](../types/idcont.md) in `result` field.

* **Condition** : If everything is OK.  

**Code** : `201 CREATED`



## Error Responses
**Content:** None
* **Condition** : If token does not belong to the administrator.  
**Code** : `404 BAD REQUEST`

* **Condition** : If required fields are missed.  
**Code** : `404 BAD REQUEST`

**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If tag with specified name already exists.  
**Code** : `400  BAD REQUEST`



