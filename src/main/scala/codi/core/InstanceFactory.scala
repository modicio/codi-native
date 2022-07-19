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
import codi.core.values.ConcreteValue
import codi.util.Identity
import codi.verification.{DefinitionVerifier, ModelVerifier}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @param definitionVerifier
 * @param modelVerifier
 */
class InstanceFactory(definitionVerifier: DefinitionVerifier,
                      modelVerifier: ModelVerifier) {

  private var registry: Registry = _

  def setRegistry(registry: Registry): Unit = this.registry = registry

  def newInstance(typeName: String): Future[DeepInstance] = newInstance(typeName, Identity.create())

  def newInstance(typeName: String, newIdentity: String): Future[DeepInstance] = {
    registry.getType(typeName, Fragment.REFERENCE_IDENTITY) flatMap (typeHandleOption =>
      typeHandleOption.getOrElse(throw new Exception("type-model not found")).unfold() flatMap (referenceTypeHandle => {
        if (referenceTypeHandle.getFragment.isTemplate) {
          Future.failed(new Exception("unable to instantiate template type-models at bottom level"))
        } else {
          val identity: String = newIdentity
          val deepValueSet: Set[ConcreteValue] = referenceTypeHandle.getFragment.deepValueSet
          val unfoldedReferenceFragment: Fragment = referenceTypeHandle.getFragment.fork(identity)

          unfoldedReferenceFragment.setVerifiers(definitionVerifier, modelVerifier)
          unfoldedReferenceFragment.createHandle.unfold() flatMap (unfoldedTypeHandle => {
            val instanceBuffer: mutable.Set[DeepInstance] = mutable.Set[DeepInstance]()
            val rootInstanceId = deriveInstance(unfoldedTypeHandle, identity, deepValueSet, instanceBuffer).getInstanceId
            Future.sequence(instanceBuffer.map(registry.setInstance)) map (_ => instanceBuffer.find(_.getInstanceId == rootInstanceId).get)
          })
        }
      }))
  }

  /**
   *
   * @param typeHandle unfolded TypeHandle with forked and identified fragment model
   * @param identity
   * @param deepValueSet
   * @param instanceBuffer
   * @return
   */
  private def createExtensions(typeHandle: TypeHandle,
                               identity: String, rootInstanceId: String,
                               deepValueSet: Set[ConcreteValue],
                               instanceBuffer: mutable.Set[DeepInstance]): Set[ExtensionData] = {
    val fragment = typeHandle.getFragment
    val extensions = fragment.getParents
    extensions.map(extensionFragment => deriveInstance(extensionFragment.createHandle, identity, deepValueSet, instanceBuffer)).map(newInstance => ExtensionData(0, rootInstanceId, newInstance.getInstanceId))
  }

  /**
   *
   * @param typeHandle unfolded TypeHandle with forked and identified fragment model
   * @param identity
   * @param deepValueSet
   * @param instanceBuffer
   * @return
   */
  private def deriveInstance(typeHandle: TypeHandle,
                             identity: String,
                             deepValueSet: Set[ConcreteValue],
                             instanceBuffer: mutable.Set[DeepInstance]): DeepInstance = {
    val instanceId: String = {
      if(Fragment.isSingletonIdentity(identity)){
        DeepInstance.deriveSingletonInstanceId(identity, typeHandle.getTypeName)
      }else{
        Identity.create()
      }
    }
    val extensions = createExtensions(typeHandle, identity, instanceId, deepValueSet, instanceBuffer)
    val shape = new Shape(
      deriveAttributes(typeHandle, instanceId, deepValueSet),
      mutable.Set.from(deriveAssociations(typeHandle, instanceId, deepValueSet)),
      extensions)
    val deepInstance = new DeepInstance(instanceId, identity, shape, typeHandle, registry)
    instanceBuffer.add(deepInstance)
    deepInstance
  }

  /**
   * TODO resolve overwrites and if this is actually the most concrete point to assign a value
   * @param typeHandle
   * @param instanceId
   * @param deepValueSet
   * @return
   */
  private def deriveAttributes(typeHandle: TypeHandle, instanceId: String, deepValueSet: Set[ConcreteValue]): Set[AttributeData] = {
    val attributeRules = typeHandle.getFragment.definition.getAttributeRules
    attributeRules.map(attributeRule => {
      val valueOption = deepValueSet.find(value => value.concreteOf(attributeRule))
      if(valueOption.isEmpty) {
        AttributeData(0, instanceId, attributeRule.name, "", isFinal = false)
      }else{
        val value = valueOption.get
        AttributeData(0, instanceId, attributeRule.name, value.getAttributeDescriptor.attributeValue, isFinal = true)
      }
    })
  }

  /**
   * TODO resolve overwrites and if this is actually the most concrete point to assign a value
   *  here, we should check also if the assigned association target exists actually. This is currently based on trust.
   * @param typeHandle
   * @param instanceId
   * @param deepValueSet
   * @return
   */
  private def deriveAssociations(typeHandle: TypeHandle, instanceId: String, deepValueSet: Set[ConcreteValue]): Set[AssociationData] = {
    val associationRules = typeHandle.getFragment.definition.getAssociationRules
    val result = mutable.Set[AssociationData]()
    associationRules.foreach(associationRule => {
      //there may be concrete values of this rule
      deepValueSet.filter(value => value.concreteOf(associationRule)).foreach(value => {
        val relationName = value.valueName
        val concreteAssociation = value.getAssociationDescriptor
        val target = concreteAssociation.targetFragment
        //val targetIdentity = concreteAssociation.targetIdentity // we assume only singleton identities here!
        result.add(AssociationData(0, relationName, instanceId, DeepInstance.deriveRootSingletonInstanceId(target), isFinal = true))
      })
    })
    result.toSet
  }

  //TODO construct Instance from a lot of stuff here #
  // extensions do not need to be considered :D, they are loaded only on unfold
  def loadInstance(instanceData: InstanceData, configuration: Shape, typeHandle: TypeHandle): DeepInstance = ???

}
