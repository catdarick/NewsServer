# Edit draft

Edites [draft](../types/draft.md) fields.

**URL** : `/editDraft`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__draft_id__ | integer | News ID to which the comment relates
title | string | New title
content | string | New content
category_id | integer | New category ID
tags_id | [integer] | New list with tags id
main_picture | string | New main picture's URL
pictures | [string] | New list with URLs of additional pictures

**Note:**
The author can only edit his own draft.
Fields not specified will not be affected in draft.

**Method** : `PUT`

**Auth required** : Yes

**Permissions required** : Author

## Success Response

**Content:** [Response](../types/response.md) with empty `result` field.


* **Condition** : If everything is OK.  
**Code** : `200 OK`


## Error Responses

**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If required fields are missed.  
**Code** : `400 BAD REQUEST`

* **Condition** : If token does not belong to draft owner author.  
**Code** : `403  UNAUTHORIZED`

* **Condition** : If draft with specified ID doesn't exists.  
**Code** : `400 BAD REQUEST`


