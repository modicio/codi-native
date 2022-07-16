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

import codi.core.datamappings.{AssociationData, AttributeData, ExtensionData, InstanceData}
import codi.util.Identity
import codi.verification.{DefinitionVerifier, ModelVerifier}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * TODO documentation
 * @param definitionVerifier
 * @param modelVerifier
 */
class InstanceFactory(definitionVerifier: DefinitionVerifier,
                      modelVerifier: ModelVerifier) {

  private var registry: Registry = _

  def setRegistry(registry: Registry): Unit = this.registry = registry

  //TODO consider VALUES

  def newInstance(typeName: String, identity: String): Future[DeepInstance] = ???

  def newInstance(typeName: String): Future[DeepInstance] = {
    registry.getType(typeName, Fragment.REFERENCE_IDENTITY) flatMap (typeHandleOption =>
      typeHandleOption.getOrElse(throw new Exception("type-model not found")).unfold() flatMap (referenceTypeHandle => {
        if (referenceTypeHandle.getFragment.isTemplate) {
          Future.failed(new Exception("unable to instantiate template type-models at bottom level"))
        } else {
          val identity: String = Identity.create()
          val unfoldedReferenceFragment: Fragment = referenceTypeHandle.getFragment.fork(identity)
          unfoldedReferenceFragment.setVerifiers(definitionVerifier, modelVerifier)
          unfoldedReferenceFragment.createHandle.unfold() flatMap (unfoldedTypeHandle => {
            val instanceBuffer: mutable.Set[DeepInstance] = mutable.Set[DeepInstance]()
            val rootInstanceId = deriveInstance(unfoldedTypeHandle, identity, instanceBuffer).getInstanceId
            Future.sequence(instanceBuffer.map(registry.setInstance)) map (_ => instanceBuffer.find(_.getInstanceId == rootInstanceId).get)
          })
        }
      }))
  }

  /**
   *
   * @param typeHandle unfolded TypeHandle with forked and identified fragment model
   * @param identity
   * @param instanceBuffer
   * @return
   */
  private def createExtensions(typeHandle: TypeHandle, identity: String, rootInstanceId: String, instanceBuffer: mutable.Set[DeepInstance]): Set[ExtensionData] = {
    val fragment = typeHandle.getFragment
    val extensions = fragment.getParents
    extensions.map(extensionFragment => deriveInstance(extensionFragment.createHandle, identity, instanceBuffer)).map(newInstance => ExtensionData(0, rootInstanceId, newInstance.getInstanceId))
  }

  /**
   *
   * @param typeHandle unfolded TypeHandle with forked and identified fragment model
   * @param identity
   * @param instanceBuffer
   * @return
   */
  private def deriveInstance(typeHandle: TypeHandle, identity: String, instanceBuffer: mutable.Set[DeepInstance]): DeepInstance = {
    val instanceId: String = Identity.create()
    val extensions = createExtensions(typeHandle, identity, instanceId, instanceBuffer)
    val configuration = new Shape(deriveProperties(typeHandle, instanceId), mutable.Set[AssociationData](), extensions)
    val deepInstance = new DeepInstance(instanceId, identity, configuration, typeHandle, registry)
    instanceBuffer.add(deepInstance)
    deepInstance
  }

  private def deriveProperties(typeHandle: TypeHandle, instanceId: String): Set[AttributeData] = {
    val attributeRules = typeHandle.getFragment.definition.getAttributeRules
    attributeRules.map(propertyRule => AttributeData(0, instanceId, propertyRule.name, ""))
  }

  //TODO construct Instance from a lot of stuff here
  def loadInstance(instanceData: InstanceData, configuration: Shape, typeHandle: TypeHandle): DeepInstance = ???

}
