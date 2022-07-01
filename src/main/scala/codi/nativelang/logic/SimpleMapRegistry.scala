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
package codi.nativelang.logic

import codi.core._

import scala.collection.mutable
import scala.concurrent.Future

/**
 * TODO documentation
 *
 * @param typeFactory
 * @param instanceFactory
 */
class SimpleMapRegistry(typeFactory: TypeFactory, instanceFactory: InstanceFactory)
  extends Registry(typeFactory, instanceFactory) {

  private val typeRegistry = mutable.Map[String, mutable.Map[String, TypeHandle]]()
  private val instanceRegistry = mutable.Map[String, DeepInstance]()

  override def getType(name: String, identity: String): Future[Option[TypeHandle]] = {
    val typeGroup = typeRegistry.get(name)
    if (typeGroup.isEmpty) {
      return Future.successful(None)
    }
    val result = typeGroup.get.get(identity)
    Future.successful(result)
  }

  override def setType(typeHandle: TypeHandle): Future[Unit] = {
    val name = typeHandle.getTypeName
    val identity = typeHandle.getTypeIdentity
    if (!typeRegistry.contains(name)) {
      typeRegistry.addOne(name, mutable.Map[String, TypeHandle]())
    }
    val typeGroup = typeRegistry(name)
    if (typeGroup.contains(identity)) {
      typeGroup.remove(identity)
    }
    Future.successful(typeGroup.addOne(identity, typeHandle))
  }

  override def getReferences: Future[Set[TypeHandle]] = {
    Future.successful(typeRegistry.values.flatMap(_.values).filter(_.getTypeIdentity == Fragment.REFERENCE_IDENTITY).toSet)
  }

  override def get(instanceId: String): Future[Option[DeepInstance]] = {
    Future.successful(instanceRegistry.get(instanceId))
  }

  override def getAll(typeName: String): Future[Set[DeepInstance]] = {
    Future.successful(instanceRegistry.toSet.filter(_._2.getTypeHandle.getTypeName == typeName).map(_._2))
  }

  override def setInstance(deepInstance: DeepInstance): Future[Unit] = {
    Future.successful(instanceRegistry.addOne(deepInstance.getInstanceId, deepInstance))
  }

}
