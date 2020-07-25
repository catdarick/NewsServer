# Create account

Create an account for the user. Authorization is required to all nonreading methods.

**URL** : `/createAccount`

**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__login__ | string |User's login
__password__ | string |User's password
__first_name__ |string | User's first name
__last_name__ | string | User's last name
picture | string | User's profile picture link 
admin_pass | string | Global admin password

**Method** : `POST`

**Auth required** : No

**Permissions required** : None

## Success Response

**Condition** : If everything is OK and login is not busy.

**Code** : `201`

**Content:** [Response](docs/types/response.md) with created user [id container](docs/types/idcont.md) in result field.



## Error Responses

**Condition** : If login is busy.

**Code** : `400  BAD REQUEST`

**Content:** [Response](docs/types/response.md) with description in error field.

### Or

**Condition** : If required fields are missed.

**Code** : `400 BAD REQUEST`

