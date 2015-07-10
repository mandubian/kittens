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

import cats._, free.Trampoline, Trampoline.done, std.function._
import shapeless._

object pointed {
  implicit def apply[F[_]](implicit mff: WrappedOrphan[Pointed[F]]): Pointed[F] = mff.instance
}

trait Pointed[F[_]] {
  def point[A](a: A): F[A]
}

object Pointed extends Pointed0 {
  def apply[F[_]](implicit mff: Pointed[F]): Pointed[F] = mff
}

trait Pointed0 extends Pointed1 {

  implicit def hcons[F[_]](implicit ihc: IsHCons1[F, Pointed, Pointed]): Pointed[F] =
    new Pointed[F] {
      def point[A](a: A): F[A] = {
        ihc.pack(ihc.fh.point(a), ihc.ft.point(a))
      }
    }

  implicit def ccons[F[_]](implicit ihc: IsCCons1[F, Pointed, Pointed]): Pointed[F] =
    new Pointed[F] {
      def point[A](a: A): F[A] = {
        ihc.pack(Left(ihc.fh.point(a)))
      }
    }

}

trait Pointed1 extends Pointed2 {

  implicit def split[F[_]](implicit split: Split1[F, Pointed, Pointed]): Pointed[F] =
    new Pointed[F] {
      import split._
      def point[A](a: A): F[A] = {
        pack(fo.point(fi.point(a)))
      }
    }
}

trait Pointed2 extends Pointed3 {

  implicit def generic[F[_]](implicit gen: Generic1[F, Pointed]): Pointed[F] =
    new Pointed[F] {
      def point[A](a: A): F[A] = gen.from(gen.fr.point(a))
    }
}

trait Pointed3 {

  // implicit val idPointed: Pointed[shapeless.Id] =
  //   new Pointed[shapeless.Id] {
  //     def point[A](a: A): shapeless.Id[A] = a
  //   }

  implicit def isCPointedSimpleType: Pointed[({type λ[A] = A :+: Const[CNil]#λ[A] })#λ] =
    new Pointed[({type λ[A] = A :+: Const[CNil]#λ[A] })#λ] {
      def point[A](a: A): A :+: Const[CNil]#λ[A] = Inl(a)
    }
    

  implicit val constHNilPointed: Pointed[Const[HNil]#λ] =
    new Pointed[Const[HNil]#λ] {
      def point[A](a: A): HNil = HNil
    }

}