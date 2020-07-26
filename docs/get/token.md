# Get Token

Updates account token and returns it as a `result` in [Response](../types/response.md).

**URL** : `/getToken`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__login__ | string | Account login
__password__ | string | Account password

**Note:**
After calling this method, all previously received tokens will be invalid.

**Method** : `PUT`

**Auth required** : No

**Permissions required** : None

## Success Response

**Content:** [Response](../types/response.md) with token (`string`) in `result` field.

* **Condition** : If everything is OK.  
**Code** : `200 OK`

## Error Responses

**Content:** [Response](../types/response.md) with error description in `error` field.

* **Condition** : If parameters are incorrect.  
**Code** : `400 BAD REQUEST`


