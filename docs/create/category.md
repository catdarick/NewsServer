# Create Category

Creates new category.

**URL** : `/createCategory`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__name__ | string | Name of the new category
parent_id | integer | ID of parent category
**Note:**
If `parent_id` is not specified -- the category will be root.

**Method** : `POST`

**Auth required** : Yes

**Permissions required** : Admin

## Success Response

**Condition** : If everything is OK.

**Code** : `201 CREATED`

**Content:** [Response](../types/response.md) with created category [id container](../types/idcont.md) in `result` field.



## Error Responses
**Content:** None
* **Condition** : If token does not belong to the administrator.
**Code** : `404 BAD REQUEST`

* **Condition** : If required fields are missed.
**Code** : `404 BAD REQUEST`

**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If parent with specified ID does not exist.  
**Code** : `400  BAD REQUEST`



