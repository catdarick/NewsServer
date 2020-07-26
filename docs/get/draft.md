# Get Drafts

Depending of which filters you choose, you will receive JSON-serialized [Response](../types/response.md) with own [Draft](../types/draft.md) objects as a result.

**URL** : `/getDrafts`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__token__ | string | Access token
draft_id | integer | To get draft with specified ID
category_id | integer | To get drafts with specified category ID
tag_id | integer | To get drafts with specified tag ID
tags_id_in | [integer] | To get drafts with at least one tag id from the list
tags_id_all | [integer] | To get drafts with all tags from the list
title | string | To get drafts that contains specified substring in title
content | string | To get drafts that contains specified substring in content
limit | integer | Limits the number of objects to be retrieved. Defaults to `50`.
offset | integer | Number of entities to skip. Used for pagination. Defaults to `0`.

**Note:**  
`limit` must be from interval `(1-200)`
The non-author can call this method, but will receive an empty list.

**Method** : `GET`

**Auth required** : Yes

**Permissions required** : None

## Success Response

**Content:** [Response](../types/response.md) with [[Draft](../types/draft.md)] in `result` field.

* **Condition** : If everything is OK.  
**Code** : `200 OK`

## Error Responses

**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If parameters are incorrect.  
**Code** : `400 BAD REQUEST`


