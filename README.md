# Derivation

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.evolution/derivation_3/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.evolution/derivation_3)

A derivation library for scala 3 with annotation based configuration

## Usage

add library to your project as 

```sbt
libraryDependencies += "com.evolution" %% "evo-derivation-circe" % "{version}"
```

Then define your own type, deriving circe instances


```scala
import evo.derivation.*
import evo.derivation.circe.*


@SnakeCase
@Discriminator("type")
enum User derives Config, EvoCodec:
    case AuthorizedClient(@Rename("client_id") id: UUID, name: String)
    case Anonymous
```


## Annotation reference

### `@SnakeCase`
 Transforms all the constructor and\or field names to the snake case

### `@Discriminator(name: String)`
 Defines discriminator field for serialization of `sealed trait` or `enum`

### `@Rename(name: String)`

 Renames single `case class` field or `enum` constructor

### `@Embed`
Write\Reads all inner fields of some field on the upper level during serialization
