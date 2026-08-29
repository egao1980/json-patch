# json-patch

[RFC 6902](https://www.rfc-editor.org/rfc/rfc6902) **JSON Patch** and
[RFC 6901](https://www.rfc-editor.org/rfc/rfc6901) **JSON Pointer** for Common Lisp.

**No dependencies.** Not a `-protocol` system either: there is nothing here to
plug a backend into, only the algorithm.

```lisp
(asdf:load-system "json-patch")

(json-patch:apply-patch
 document
 (list (json-patch:json-object …)))          ; ops as decoded JSON objects

(json-patch:pointer-get document "/foo/0")
(json-patch:pointer-exists-p document "/nope")
(json-patch:json-equal a b)
```

## Representation

Matches what `yason` produces, which is what the rest of the stack passes around:

| JSON | Lisp |
|------|------|
| object | `equal` hash-table |
| array | vector |
| string / number | itself |
| `null`, `false` | `nil` |

`null` and `false` collapsing to `nil` is a yason property, not one this library
introduces. `test` therefore cannot tell them apart.

## Behaviour worth knowing

- **Non-destructive.** `apply-patch` rebuilds only the nodes along each touched
  path and shares the rest, so a consumer still holding the previous document
  keeps seeing it unchanged. That is what makes it safe to drive a UI that may
  still be rendering the old value when the next patch arrives.
- **All or nothing.** Any failing operation — including a `test` that does not
  match — signals, and the input is untouched because nothing is mutated.
- **`test` failure is an error**, per RFC 6902, not a boolean result. It has its
  own condition, `json-patch-test-failed`, so callers can single it out.
- Pointer escaping honours order: `~1` is `/` and `~0` is `~`, so `~01` reads as
  the literal `~1` rather than `/`.

Cases in the test suite follow RFC 6902 Appendix A.

## Why it exists

AG-UI `STATE_DELTA` carries RFC 6902 operations, and nothing in the Lisp stack
could apply them — which made the shared-state pattern unbuildable. Useful well
beyond that: JSON Merge Patch's lossy cousin shows up wherever documents are
synchronised incrementally.

Part of [cl-stack](https://github.com/egao1980/cl-stack).

## License

MIT — see [LICENSE](LICENSE).
