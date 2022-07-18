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

import codi.core.datamappings._
import codi.core._

import scala.concurrent.Future

/**
 * TODO documentation
 * @note experimental feature - not fully supported yet
 * @param typeFactory
 * @param instanceFactory
 */
abstract class AbstractPersistentRegistry(typeFactory: TypeFactory, instanceFactory: InstanceFactory)
  extends Registry(typeFactory, instanceFactory) {

  protected def fetchFragmentData(name: String, identity: String): Future[FragmentData]

  protected def fetchFragmentData(identity: String): Future[Set[FragmentData]]

  protected def writeFragmentData(fragmentData: FragmentData): Future[FragmentData]

  protected def fetchInstanceDataOfType(typeName: String): Future[Set[InstanceData]]

  protected def fetchInstanceData(instanceId: String): Future[InstanceData]

  protected def fetchInstanceDataOfIdentity(identity: String): Future[Set[InstanceData]]

  protected def writeInstanceData(instanceData: InstanceData): Future[InstanceData]

  protected def fetchRuleData(fragmentName: String, identity: String): Future[Set[RuleData]]

  protected def writeRuleData(ruleData: RuleData): Future[RuleData]

  protected def writeRuleData(ruleData: Set[RuleData]): Future[Set[RuleData]]

  protected def fetchAttributeData(instanceId: String): Future[Set[AttributeData]]

  protected def writeAttributeData(attributeData: AttributeData): Future[AttributeData]

  protected def writeAttributeData(attributeData: Set[AttributeData]): Future[Set[AttributeData]]

  protected def fetchExtensionData(instanceId: String): Future[Set[ExtensionData]]

  protected def writeExtensionData(extensionData: ExtensionData): Future[ExtensionData]

  protected def writeExtensionData(extensionData: Set[ExtensionData]): Future[Set[ExtensionData]]

  protected def fetchAssociationData(instanceId: String): Future[Set[AssociationData]]

  protected def writeAssociationData(associationData: AssociationData): Future[AssociationData]

  protected def writeAssociationData(associationData: Set[AssociationData]): Future[Set[AssociationData]]


  override def getType(name: String, identity: String): Future[Option[TypeHandle]] = ???

  //TODO make a difference between default Fragments and BaseModels, maybe we need Reflections here to collect all
  // BaseModel classes to create the required instance, we just set the identity and pretend its from the db...

  override def getReferences: Future[Set[TypeHandle]] = ???


  override def setType(typeHandle: TypeHandle): Future[Unit] = ???

  //TODO make a difference between default Fragments (Nodes isNode = true ) and BaseModels -> BaseModels are not persisted
  //val (fragmentData, ruleData) = typeHandle.getFragment.toData
  //TODO this thing recursive with the iterator basically if unfolded otherwise not (sets to go are empty)

  override def get(identity: String): Future[Option[DeepInstance]] = ???


  override def getAll(typeName: String): Future[Set[DeepInstance]] = ???


  override def setInstance(deepInstance: DeepInstance): Future[Unit] = ???

  override def deleteTypeNoCascade(name: String, SINGLETON_IDENTITY: String): Future[Any] = ???
}
