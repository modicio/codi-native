/**
 * Copyright 2022 Karl Kegel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package codi.core

import scala.concurrent.Future

/**
 * @param typeFactory
 * @param instanceFactory
 */
abstract class Registry(val typeFactory: TypeFactory, val instanceFactory: InstanceFactory) {

  def getType(name: String, identity: String): Future[Option[TypeHandle]]
  def getReferences: Future[Set[TypeHandle]]
  def getSingletonTypes(name: String): Future[Set[TypeHandle]]

  def setType(typeHandle: TypeHandle): Future[Unit]

  def deleteTypeNoCascade(name: String, SINGLETON_IDENTITY: String): Future[Any]

  def get(instanceId: String): Future[Option[DeepInstance]]
  def getAll(typeName: String): Future[Set[DeepInstance]]

  def setInstance(deepInstance: DeepInstance): Future[Unit]

}
