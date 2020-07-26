# Get News

Depending of which filters you choose, you will receive JSON-serialized [Response](../types/response.md) with [News](../types/news.md) objects as a result.

**URL** : `/getNews`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
news_id | integer | To get news with specified ID
login | string | To get news with specified author login
first_name | string | To get news with specified author first name
last_name | string | To get news with specified author last name
category_id | integer | To get news with specified category ID
tag_id | integer | To get news with specified tag ID
tags_id_in | [integer] | To get news with at least one tag id from the list
tags_id_all | [integer] | To get news with all tags from the list
title | string | To get news that contains specified substring in title
content | string | To get news that contains specified substring in content
date | date | To get news that were published on a specified date
after_date | date | To get news that were published after specified date
before_date | date | To get news that were published before specified date
sort | integer | To get ordered by * news. Defaults to `1`. Read note.
limit | integer | Limits the number of objects to be retrieved. Defaults to `50`.
offset | integer | Number of entities to skip. Used for pagination. Defaults to `0`.

**Note:**
`limit` must be from interval `(1-200)`
Sort possible values:
- `1` To sort from newest to oldest
- `2` To sort from oldest to newest
- `3` To sort by author first name and last name from `'A'` to `'Z'`
- `4` To sort by author first name and last name from `'Z'` to `'A'`
- `5` To sort by category name from `'A'` to `'Z'`
- `6` To sort by category name from `'Z'` to `'A'`
- `7` To sort by pictures amount from smallest to largest
- `8` To sort by pictures amount from largest to smallest


**Method** : `GET`

**Auth required** : No

**Permissions required** : None

## Success Response

**Content:** [Response](../types/response.md) with [[News](../types/news.md)] in `result` field.

* **Condition** : If everything is OK.  
**Code** : `200 OK`

## Error Responses

**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If parameters are incorrect.  
**Code** : `400 BAD REQUEST`


