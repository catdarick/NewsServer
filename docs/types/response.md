# Response

Basic returning type for all API methods.


**Query Parameters:** 
Field | Type |Description
---------- | ------------- | ---------
__success__ | bool | Success state
result | * |Result of API method call. May contain different types, depending on the method called
error |string | Error description


**Data examples**:

```json
{
    "success":true,
    "result":
            {
                "id":27
            }
}
```
```json
{
    "success":false,
    "error":"Login is busy, try another"
}
```
