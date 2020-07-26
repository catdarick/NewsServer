# Create Comment

Creates and publishes a comment related to some news.

**URL** : `/createComment`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
__news_id__ | integer | News ID to which the comment relates
__content__ | string | Comment content

**Method** : `POST`

**Auth required** : Yes

**Permissions required** : None

## Success Response

**Content:** [Response](../types/response.md) with created comment [id container](../types/idcont.md) in `result` field.

* **Condition** : If everything is OK.  

**Code** : `201 CREATED`



## Error Responses

**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If required fields are missed.  
**Code** : `400 BAD REQUEST`

* **Condition** : If token is bad.  
**Code** : `401  UNAUTHORIZED`

* **Condition** : If news with specified ID doesn't exists.  
**Code** : `400 BAD REQUEST`


