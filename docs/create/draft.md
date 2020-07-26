# Create Draft

Creates new draft.

**URL** : `/createDraft`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__title__ | string | Title of the draft (news)
__content__ | string | Content of the draft (news)
__category_id__ | integer | Category ID of the draft (news)
tags_id | [integer] | List with tags id of the draft (news)
main_picture | string | Main picture's URL
pictures | [string] | List with URLs of additional pictures

**Method** : `POST`

**Auth required** : Yea

**Permissions required** : Author

## Success Response

**Condition** : If everything is OK.

**Code** : `201 CREATED`

**Content:** [Response](../types/response.md) with created tag [id container](../types/idcont.md) in `result` field.



## Error Responses
**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If tag with specified name already exists.  
**Code** : `400  BAD REQUEST`

* **Condition** : If required fields are missed.  
**Code** : `400 BAD REQUEST`

* **Condition** : If category doesn't exists.  
**Code** : `400 BAD REQUEST`

* **Condition** : If one of tags doesn't exists.  
**Code** : `400 BAD REQUEST`

* **Condition** : If token does not belong to author.  
**Code** : `403 FORBIDDEN`



