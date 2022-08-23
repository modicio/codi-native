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
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

  override def getSingletonTypes(name: String): Future[Set[TypeHandle]] = {
    val typeGroup = typeRegistry.get(name)
    if (typeGroup.isEmpty) {
      return Future.successful(Set())
    }
    val result = typeGroup.get.filter(element => Fragment.isSingletonIdentity(element._1)).values.toSet
    Future.successful(result)
  }

  override protected def setNode(typeHandle: TypeHandle): Future[Unit] = {
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
    Future.successful(typeRegistry.values.flatMap(_.values).filter(_.getTypeIdentity == Fragment.REFERENCE_IDENTITY)
      .toSet ++ baseModels.values.map(_.createHandle))
  }

  override def get(instanceId: String): Future[Option[DeepInstance]] = {
    if (DeepInstance.isSingletonRoot(instanceId)) {
      Future.successful(instanceRegistry.get(
        DeepInstance.deriveRootSingletonInstanceId(
          Fragment.decomposeSingletonIdentity(instanceId))))
    } else {
      Future.successful(instanceRegistry.get(instanceId))
    }

  }

  override def getAll(typeName: String): Future[Set[DeepInstance]] = {
    Future.successful(instanceRegistry.toSet.filter(_._2.getTypeHandle.getTypeName == typeName).map(_._2))
  }

  override def setInstance(deepInstance: DeepInstance): Future[Unit] = {
    Future.successful(instanceRegistry.addOne(deepInstance.getInstanceId, deepInstance))
  }

  override def deleteTypeNoCascade(name: String, identity: String): Future[Any] = {
    if (identity == Fragment.REFERENCE_IDENTITY) {
      val typeGroupOption = typeRegistry.get(name)
      if (typeGroupOption.isDefined) {
        Future.successful(typeGroupOption.get.remove(identity))
      } else {
        Future.failed(new IllegalArgumentException())
      }
    } else if (identity == Fragment.SINGLETON_IDENTITY) {
      val singletonInstanceId = DeepInstance.deriveSingletonInstanceId(identity, name)
      val deepInstanceOption = instanceRegistry.get(singletonInstanceId)
      if (deepInstanceOption.isDefined) {
        deepInstanceOption.get.unfold() map (unfoldedInstance => {
          val extensions = unfoldedInstance.getTypeHandle.getFragment.getParents
          instanceRegistry.remove(singletonInstanceId)
          val mapOfFutures = extensions.map(extension => deleteTypeNoCascade(extension.name, Fragment.SINGLETON_IDENTITY))
          Future.sequence(mapOfFutures)
        })
      } else {
        Future.successful()
      }
    } else {
      Future.successful()
    }
  }
}
