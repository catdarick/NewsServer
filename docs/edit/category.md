# Edit Category

Edites [category](../types/category.md) fields.

**URL** : `/editCategory`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__category_id__ | integer | Category ID to be changed
name | string | New name
parent_id | integer | ID of parent category

**Note:**
Fields not specified will not be affected.
If `parent_id` is `0` then the category will become the root.

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

* **Condition** : If category with specified `category_id` does not exists.  
**Code** : `400  BAD REQUEST`
* **Condition** : If category with specified `parent_id` does not exists.  
**Code** : `400  BAD REQUEST`


