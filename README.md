# Declare Diagram

Using a relatively simple syntax, render a diagram, including support for automated drawing of arrows.

## Example

```
Some Diagram
  class cols
  Area One
    class cols

    Sub Area One
      class rows
      Some Thing
      Other Thing
        Component One
          arrow Baz
            class dotted
          arrow Quz
            label Hey there this is a label
        Component Two
          arrow Component Three
        Component Three
      And Another

    Sub Area Two
      class rows
      Other Thing
        Baz
        Quz
      Other Other Thing
        Barr
          class compact
          Bar

  Area Two
    class rows
    Foo
      Bar
```

## Usage

$ stack run < sample
