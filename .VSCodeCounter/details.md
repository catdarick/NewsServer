# Details

Date : 2020-07-20 23:15:17

Directory /home/darick/NewsServer

Total : 109 files,  5074 codes, 397 comments, 701 blanks, all 6172 lines

[summary](results.md)

## Files
| filename | language | code | comment | blank | total |
| :--- | :--- | ---: | ---: | ---: | ---: |
| [.gitattributes](/.gitattributes) | Properties | 1 | 1 | 1 | 3 |
| [ChangeLog.md](/ChangeLog.md) | Markdown | 2 | 0 | 2 | 4 |
| [NewsServer.cabal](/NewsServer.cabal) | Cabal | 208 | 5 | 7 | 220 |
| [NewsServer/.gitattributes](/NewsServer/.gitattributes) | Properties | 1 | 1 | 1 | 3 |
| [NewsServer/README.md](/NewsServer/README.md) | Markdown | 1 | 0 | 2 | 3 |
| [README.md](/README.md) | Markdown | 1 | 0 | 1 | 2 |
| [Scripts/create/createAdminAndGetToken.sh](/Scripts/create/createAdminAndGetToken.sh) | Shell Script | 5 | 1 | 2 | 8 |
| [Scripts/create/createAuthorAndGetToken.sh](/Scripts/create/createAuthorAndGetToken.sh) | Shell Script | 20 | 1 | 4 | 25 |
| [Scripts/create/createCategory.sh](/Scripts/create/createCategory.sh) | Shell Script | 18 | 1 | 4 | 23 |
| [Scripts/create/createDraft.sh](/Scripts/create/createDraft.sh) | Shell Script | 13 | 1 | 3 | 17 |
| [Scripts/create/createTag.sh](/Scripts/create/createTag.sh) | Shell Script | 13 | 1 | 3 | 17 |
| [Scripts/create/createUserAndGetToken.sh](/Scripts/create/createUserAndGetToken.sh) | Shell Script | 14 | 1 | 5 | 20 |
| [Scripts/create/postComment.sh](/Scripts/create/postComment.sh) | Shell Script | 9 | 1 | 4 | 14 |
| [Scripts/create/postDraft.sh](/Scripts/create/postDraft.sh) | Shell Script | 11 | 1 | 4 | 16 |
| [Scripts/delete/deleteAuthor.sh](/Scripts/delete/deleteAuthor.sh) | Shell Script | 9 | 1 | 3 | 13 |
| [Scripts/delete/deleteCategory.sh](/Scripts/delete/deleteCategory.sh) | Shell Script | 9 | 1 | 3 | 13 |
| [Scripts/delete/deleteComment.sh](/Scripts/delete/deleteComment.sh) | Shell Script | 9 | 1 | 5 | 15 |
| [Scripts/delete/deleteDraft.sh](/Scripts/delete/deleteDraft.sh) | Shell Script | 13 | 1 | 3 | 17 |
| [Scripts/delete/deleteTag.sh](/Scripts/delete/deleteTag.sh) | Shell Script | 9 | 1 | 3 | 13 |
| [Scripts/delete/deleteUser.sh](/Scripts/delete/deleteUser.sh) | Shell Script | 9 | 1 | 3 | 13 |
| [Setup.hs](/Setup.hs) | Haskell | 2 | 0 | 1 | 3 |
| [app/Main.hs](/app/Main.hs) | Haskell | 53 | 31 | 12 | 96 |
| [package.yaml](/package.yaml) | YAML | 61 | 6 | 7 | 74 |
| [src/Api/ErrorException.hs](/src/Api/ErrorException.hs) | Haskell | 16 | 0 | 6 | 22 |
| [src/Api/Helpers/Checks.hs](/src/Api/Helpers/Checks.hs) | Haskell | 104 | 3 | 14 | 121 |
| [src/Api/Helpers/Getters.hs](/src/Api/Helpers/Getters.hs) | Haskell | 74 | 1 | 14 | 89 |
| [src/Api/Methods/Create.hs](/src/Api/Methods/Create.hs) | Haskell | 8 | 0 | 2 | 10 |
| [src/Api/Methods/Create/Account.hs](/src/Api/Methods/Create/Account.hs) | Haskell | 40 | 3 | 4 | 47 |
| [src/Api/Methods/Create/Author.hs](/src/Api/Methods/Create/Author.hs) | Haskell | 39 | 2 | 4 | 45 |
| [src/Api/Methods/Create/Category.hs](/src/Api/Methods/Create/Category.hs) | Haskell | 41 | 2 | 4 | 47 |
| [src/Api/Methods/Create/Draft.hs](/src/Api/Methods/Create/Draft.hs) | Haskell | 46 | 3 | 4 | 53 |
| [src/Api/Methods/Create/Tag.hs](/src/Api/Methods/Create/Tag.hs) | Haskell | 40 | 2 | 4 | 46 |
| [src/Api/Methods/Delete.hs](/src/Api/Methods/Delete.hs) | Haskell | 9 | 0 | 2 | 11 |
| [src/Api/Methods/Delete/Author.hs](/src/Api/Methods/Delete/Author.hs) | Haskell | 40 | 2 | 4 | 46 |
| [src/Api/Methods/Delete/Category.hs](/src/Api/Methods/Delete/Category.hs) | Haskell | 40 | 2 | 4 | 46 |
| [src/Api/Methods/Delete/Comment.hs](/src/Api/Methods/Delete/Comment.hs) | Haskell | 46 | 2 | 4 | 52 |
| [src/Api/Methods/Delete/Draft.hs](/src/Api/Methods/Delete/Draft.hs) | Haskell | 45 | 2 | 4 | 51 |
| [src/Api/Methods/Delete/Tag.hs](/src/Api/Methods/Delete/Tag.hs) | Haskell | 40 | 2 | 4 | 46 |
| [src/Api/Methods/Delete/User.hs](/src/Api/Methods/Delete/User.hs) | Haskell | 40 | 2 | 4 | 46 |
| [src/Api/Methods/Edit.hs](/src/Api/Methods/Edit.hs) | Haskell | 7 | 0 | 2 | 9 |
| [src/Api/Methods/Edit/Author.hs](/src/Api/Methods/Edit/Author.hs) | Haskell | 40 | 2 | 4 | 46 |
| [src/Api/Methods/Edit/Category.hs](/src/Api/Methods/Edit/Category.hs) | Haskell | 42 | 2 | 4 | 48 |
| [src/Api/Methods/Edit/Draft.hs](/src/Api/Methods/Edit/Draft.hs) | Haskell | 51 | 2 | 4 | 57 |
| [src/Api/Methods/Edit/Tag.hs](/src/Api/Methods/Edit/Tag.hs) | Haskell | 40 | 2 | 4 | 46 |
| [src/Api/Methods/Errors.hs](/src/Api/Methods/Errors.hs) | Haskell | 36 | 1 | 19 | 56 |
| [src/Api/Methods/Get.hs](/src/Api/Methods/Get.hs) | Haskell | 11 | 0 | 1 | 12 |
| [src/Api/Methods/Get/Author.hs](/src/Api/Methods/Get/Author.hs) | Haskell | 58 | 3 | 4 | 65 |
| [src/Api/Methods/Get/Category.hs](/src/Api/Methods/Get/Category.hs) | Haskell | 40 | 3 | 4 | 47 |
| [src/Api/Methods/Get/Comment.hs](/src/Api/Methods/Get/Comment.hs) | Haskell | 38 | 3 | 4 | 45 |
| [src/Api/Methods/Get/Draft.hs](/src/Api/Methods/Get/Draft.hs) | Haskell | 63 | 3 | 4 | 70 |
| [src/Api/Methods/Get/News.hs](/src/Api/Methods/Get/News.hs) | Haskell | 86 | 3 | 4 | 93 |
| [src/Api/Methods/Get/Tag.hs](/src/Api/Methods/Get/Tag.hs) | Haskell | 39 | 3 | 4 | 46 |
| [src/Api/Methods/Get/Token.hs](/src/Api/Methods/Get/Token.hs) | Haskell | 40 | 3 | 4 | 47 |
| [src/Api/Methods/Get/User.hs](/src/Api/Methods/Get/User.hs) | Haskell | 43 | 3 | 4 | 50 |
| [src/Api/Methods/Post/Comment.hs](/src/Api/Methods/Post/Comment.hs) | Haskell | 39 | 2 | 4 | 45 |
| [src/Api/Methods/Post/Draft.hs](/src/Api/Methods/Post/Draft.hs) | Haskell | 37 | 2 | 5 | 44 |
| [src/Api/Types.hs](/src/Api/Types.hs) | Haskell | 31 | 0 | 30 | 61 |
| [src/Api/Types/Author.hs](/src/Api/Types/Author.hs) | Haskell | 34 | 1 | 6 | 41 |
| [src/Api/Types/Category.hs](/src/Api/Types/Category.hs) | Haskell | 21 | 1 | 7 | 29 |
| [src/Api/Types/Comment.hs](/src/Api/Types/Comment.hs) | Haskell | 29 | 1 | 5 | 35 |
| [src/Api/Types/News.hs](/src/Api/Types/News.hs) | Haskell | 66 | 1 | 7 | 74 |
| [src/Api/Types/Response.hs](/src/Api/Types/Response.hs) | Haskell | 36 | 1 | 13 | 50 |
| [src/Api/Types/Tag.hs](/src/Api/Types/Tag.hs) | Haskell | 18 | 1 | 7 | 26 |
| [src/Api/Types/User.hs](/src/Api/Types/User.hs) | Haskell | 32 | 1 | 6 | 39 |
| [src/Config.hs](/src/Config.hs) | Haskell | 28 | 1 | 5 | 34 |
| [src/Database/Checks/Draft.hs](/src/Database/Checks/Draft.hs) | Haskell | 50 | 3 | 6 | 59 |
| [src/Database/Checks/User.hs](/src/Database/Checks/User.hs) | Haskell | 18 | 2 | 4 | 24 |
| [src/Database/Create/Author.hs](/src/Database/Create/Author.hs) | Haskell | 14 | 2 | 4 | 20 |
| [src/Database/Create/Category.hs](/src/Database/Create/Category.hs) | Haskell | 26 | 3 | 4 | 33 |
| [src/Database/Create/Comment.hs](/src/Database/Create/Comment.hs) | Haskell | 14 | 2 | 4 | 20 |
| [src/Database/Create/Draft.hs](/src/Database/Create/Draft.hs) | Haskell | 72 | 4 | 7 | 83 |
| [src/Database/Create/Tag.hs](/src/Database/Create/Tag.hs) | Haskell | 14 | 3 | 4 | 21 |
| [src/Database/Create/User.hs](/src/Database/Create/User.hs) | Haskell | 36 | 2 | 5 | 43 |
| [src/Database/Delete/Author.hs](/src/Database/Delete/Author.hs) | Haskell | 14 | 2 | 4 | 20 |
| [src/Database/Delete/Category.hs](/src/Database/Delete/Category.hs) | Haskell | 14 | 2 | 4 | 20 |
| [src/Database/Delete/Comment.hs](/src/Database/Delete/Comment.hs) | Haskell | 14 | 2 | 4 | 20 |
| [src/Database/Delete/Draft.hs](/src/Database/Delete/Draft.hs) | Haskell | 22 | 2 | 5 | 29 |
| [src/Database/Delete/Tag.hs](/src/Database/Delete/Tag.hs) | Haskell | 14 | 2 | 4 | 20 |
| [src/Database/Delete/User.hs](/src/Database/Delete/User.hs) | Haskell | 14 | 2 | 4 | 20 |
| [src/Database/Edit/Author.hs](/src/Database/Edit/Author.hs) | Haskell | 14 | 2 | 4 | 20 |
| [src/Database/Edit/Category.hs](/src/Database/Edit/Category.hs) | Haskell | 27 | 2 | 5 | 34 |
| [src/Database/Edit/Draft.hs](/src/Database/Edit/Draft.hs) | Haskell | 41 | 2 | 4 | 47 |
| [src/Database/Edit/Tag.hs](/src/Database/Edit/Tag.hs) | Haskell | 14 | 2 | 4 | 20 |
| [src/Database/Get/Author.hs](/src/Database/Get/Author.hs) | Haskell | 47 | 4 | 5 | 56 |
| [src/Database/Get/Category.hs](/src/Database/Get/Category.hs) | Haskell | 94 | 4 | 9 | 107 |
| [src/Database/Get/Comment.hs](/src/Database/Get/Comment.hs) | Haskell | 31 | 4 | 5 | 40 |
| [src/Database/Get/Draft.hs](/src/Database/Get/Draft.hs) | Haskell | 81 | 4 | 5 | 90 |
| [src/Database/Get/News.hs](/src/Database/Get/News.hs) | Haskell | 118 | 5 | 4 | 127 |
| [src/Database/Get/Tag.hs](/src/Database/Get/Tag.hs) | Haskell | 34 | 4 | 4 | 42 |
| [src/Database/Get/User.hs](/src/Database/Get/User.hs) | Haskell | 50 | 4 | 6 | 60 |
| [src/Lib.hs](/src/Lib.hs) | Haskell | 5 | 0 | 2 | 7 |
| [src/Migration/Create.hs](/src/Migration/Create.hs) | Haskell | 89 | 3 | 13 | 105 |
| [src/State/Types.hs](/src/State/Types.hs) | Haskell | 9 | 0 | 2 | 11 |
| [src/Types/Picture.hs](/src/Types/Picture.hs) | Haskell | 3 | 0 | 3 | 6 |
| [stack.yaml](/stack.yaml) | YAML | 3 | 58 | 6 | 67 |
| [stack.yaml.lock](/stack.yaml.lock) | YAML | 7 | 4 | 2 | 13 |
| [test/DatabaseTest/Author.hs](/test/DatabaseTest/Author.hs) | Haskell | 199 | 1 | 25 | 225 |
| [test/DatabaseTest/Category.hs](/test/DatabaseTest/Category.hs) | Haskell | 67 | 1 | 11 | 79 |
| [test/DatabaseTest/Comment.hs](/test/DatabaseTest/Comment.hs) | Haskell | 60 | 1 | 9 | 70 |
| [test/DatabaseTest/Draft.hs](/test/DatabaseTest/Draft.hs) | Haskell | 220 | 1 | 25 | 246 |
| [test/DatabaseTest/News.hs](/test/DatabaseTest/News.hs) | Haskell | 160 | 1 | 21 | 182 |
| [test/DatabaseTest/Tag.hs](/test/DatabaseTest/Tag.hs) | Haskell | 75 | 1 | 11 | 87 |
| [test/DatabaseTest/User.hs](/test/DatabaseTest/User.hs) | Haskell | 123 | 1 | 16 | 140 |
| [test/MethodsTest/Author.hs](/test/MethodsTest/Author.hs) | Haskell | 188 | 1 | 23 | 212 |
| [test/MethodsTest/Category.hs](/test/MethodsTest/Category.hs) | Haskell | 170 | 1 | 17 | 188 |
| [test/MethodsTest/Draft.hs](/test/MethodsTest/Draft.hs) | Haskell | 180 | 109 | 14 | 303 |
| [test/MethodsTest/Tag.hs](/test/MethodsTest/Tag.hs) | Haskell | 163 | 1 | 15 | 179 |
| [test/MethodsTest/User.hs](/test/MethodsTest/User.hs) | Haskell | 283 | 1 | 34 | 318 |
| [test/Spec.hs](/test/Spec.hs) | Haskell | 19 | 11 | 4 | 34 |

[summary](results.md)