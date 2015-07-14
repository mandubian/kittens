/*
 * Copyright (c) 2015 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package cats.derived

import TestDefns._

import shapeless.{HNil, Const, WrappedOrphan}

class PointedTests extends CatsSuite {
  // import cats.derived.pointed0._

  test("Pointed[IList]") {

    // val F = Pointed[Const[HNil]#λ]
    assert(Pointed[Const[HNil]#λ].point(5) == HNil)


    assert(Pointed[Some].point(5) == Some(5))
    assert(Pointed[Const[None.type]#λ].point(5) == None)
    assert(Pointed[Option].point(5) == Some(5))

    // assert(Pointed[IList].point(5) == INil[Int])

  }

}