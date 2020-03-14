## Utility Functions

### Error Handling

- `error(info: Any*)`: raise an `cuvee.Error` with some additional information

- `ensure(test: Boolean, info: Any*)`: raise an error if `test` is not satisfied (prefer this one over `assert`)

- `unwrap[A](option: Option[A], info: Any*): A`: get the contents of an `Option[A]`, rais an error if there is `None`


### Iterables

- `k.times { ... }`: execute some code `k` number of times

- `_ disjoint _`: test whether two sets are disjoint

- `_ classes eq`: return equivalence classes of an iterable according to comparison `eq`

- `_ duplicates eq`: return elements that occur more than once in an iterable according to comparison `eq`

- `_.hasDuplicates`: test whether an iterable has duplicates


### Others

- `str __ num`: index a string where `num: Option[Int]` with unicode subscripts

- `file.text`: return contents of a `java.io.File` as `String`

- `time[A](f: => A): (Long, A)`: measure time it takes to execute thunk `f`

