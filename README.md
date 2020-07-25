# NewsServer

NewsServer is HTTP server that implements REST backend API. It provides service with typical news site functionality.
## Installing

Previously you must install `libpq-dev` package.
```sh
git clone https://github.com/catdarick/NewsServer
cd NewsServer
stack install
```
## Usage
Set PostgreSQL connection info in `/NewsServer/app/server.cfg`.

```sh
# Create tables in PostgreSQL database
# Needs to be done once
NewsServer-exe -i

# Run server
NewsServer-exe
```
## Api

##### Registration and Authentication

Closed endpoints require a valid Token to be included in the header of the
request. A Token can be acquired from the Login view above.
* [Create account](docs/create/account.md) : `POST /createAccount`
* [Get token](docs/get/token.md) : `GET /getToken`

##### Get methods

Each endpoint displays information related to some entity

* [Get users](docs/get/user.md) : `GET /getUsers`
* [Get authors](docs/get/author.md) : `GET /getAuthors`
* [Get tags](docs/get/tag.md) : `GET /getTags`
* [Get categories](docs/get/category.md) : `GET /getCategories`
* [Get drafts](docs/get/deaft.md) : `GET /getDrafts`
* [Get news](docs/get/news.md) : `GET /getNews`
* [Get comments](docs/get/comment.md) : `GET /getComments`


##### Create methods

Each endpoint displays information related to some entity

* [Create user](docs/create/user.md) : `PUT /createUser`
* [Create author](docs/create/author.md) : `POST /createAuthor`
* [Create tag](docs/create/tag.md) : `POST /createTag`
* [Create category](docs/create/category.md) : `POST /createCategory`
* [Create draft](docs/create/deaft.md) : `POST /createDraft`
* [Create news by posting draft](docs/post/draft.md) : `PUT /postDraft`
* [Create and post comment](docs/post/comment.md) : `POST /postComment`

##### Edit methods

Each endpoint displays information related to some entity

* [Edit author](docs/edit/author.md) : `PUT /editAuthor`
* [Edit tag](docs/edit/tag.md) : `PUT /editTag`
* [Edit category](docs/edit/category.md) : `PUT /editCategory`
* [Edit draft](docs/edit/deaft.md) : `PUT /editDraft`

#### Delete methods

Each endpoint displays information related to some entity

* [Delete user](docs/delete/user.md) : `DELETE /deleteUser`
* [Delete author](docs/delete/author.md) : `DELETE /deleteAuthor`
* [Delete tag](docs/delete/tag.md) : `DELETE /deleteTag`
* [Delete category](docs/delete/category.md) : `DELETE /deleteCategory`
* [Delete draft](docs/delete/deaft.md) : `DELETE /deleteDraft`
* [Delete news](docs/delete/draft.md) : `DELETE /deleteNews`
* [Delete comment](docs/delete/comment.md) : `DELETE /deleteComment`
