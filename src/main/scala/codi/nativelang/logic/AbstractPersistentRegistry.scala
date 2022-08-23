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

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

abstract class AbstractPersistentRegistry(typeFactory: TypeFactory, instanceFactory: InstanceFactory)
                                         (implicit executionContext: ExecutionContext)
  extends Registry(typeFactory, instanceFactory) {

  protected def fetchFragmentData(name: String, identity: String): Future[Option[FragmentData]]

  protected def fetchFragmentData(identity: String): Future[Set[FragmentData]]

  protected def writeFragmentData(fragmentData: FragmentData): Future[FragmentData]

  protected def fetchInstanceDataOfType(typeName: String): Future[Set[InstanceData]]

  protected def fetchInstanceData(instanceId: String): Future[Option[InstanceData]]

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


  override def getType(name: String, identity: String): Future[Option[TypeHandle]] = {
    for {
      fragmentDataOption <- fetchFragmentData(name, identity)
      ruleData <- fetchRuleData(name, identity)
    } yield {
      if(fragmentDataOption.isDefined){
        val fragmentData = fragmentDataOption.get;
        Some(typeFactory.loadType(fragmentData, ruleData))
      }else {
        None
      }
    }
  }

  override def getReferences: Future[Set[TypeHandle]] = {
    for {
      fragmentDataSet <- fetchFragmentData(Fragment.REFERENCE_IDENTITY)
      ruleDataSet <- Future.sequence(fragmentDataSet.map(f => fetchRuleData(f.name, f.identity)))
    } yield {
      if(fragmentDataSet.size != ruleDataSet.size){
        Future.failed(new Exception("Not matching fragment and rule-set relations"))
      }
      fragmentDataSet.map(fragmentData => (fragmentData, {
        ruleDataSet.find(rules => rules.exists(ruleData =>
          ruleData.fragmentName == fragmentData.name && ruleData.identity == fragmentData.identity))
      })).map(modelTuple => {
        val (fragmentData, ruleDataOption) = modelTuple
        val ruleData: Set[RuleData] = ruleDataOption.getOrElse(Set())
        typeFactory.loadType(fragmentData, ruleData)
      }) ++ baseModels.values.map(_.createHandle).toSet
    }
  }

  override protected def setNode(typeHandle: TypeHandle): Future[Unit] = {
    val (fragmentData, ruleData) = typeHandle.getFragment.toData
    for {
      _ <- writeRuleData(ruleData)
      _ <- writeFragmentData(fragmentData)
    } yield Future.successful()
  }

  override def get(instanceId: String): Future[Option[DeepInstance]] = {
    fetchInstanceData(instanceId) flatMap (instanceDataOption => {
      if(instanceDataOption.isDefined){
        val instanceData = instanceDataOption.get
        for {
          attributeData <- fetchAttributeData(instanceId)
          extensionData <- fetchExtensionData(instanceId)
          associationData <- fetchAssociationData(instanceId)
          typeOption <- getType(instanceData.instanceOf, instanceData.identity)
        } yield {
          if(typeOption.isDefined){
            val shape = new Shape(attributeData, mutable.Set(associationData), extensionData)
            instanceFactory.loadInstance(instanceData, shape, typeOption.get)
          }else{
            None
          }
        }
      }else{
        Future.successful(None)
      }
    })
  }

  override def getAll(typeName: String): Future[Set[DeepInstance]] = {

  }

  override def setInstance(deepInstance: DeepInstance): Future[Unit] = {

  }

  override def deleteTypeNoCascade(name: String, SINGLETON_IDENTITY: String): Future[Any] = {

  }
}
