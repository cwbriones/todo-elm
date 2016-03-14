todo-frontend-elm
===

An elm-based todo frontend, made to complement my [erlang backend](https://github.com/cwbriones/todo-backend-erl)
for a completely functional stack.

Styling is from the [todomvc-app-css](https://github.com/tastejs/todomvc-app-css) package.

### Running

If you have **any** todo backend running on port `8080` you simply need to:

```bash
$ elm make Main.elm --output=elm.js
$ open index.html
```

### LICENSE

This code is available under the MIT License.
